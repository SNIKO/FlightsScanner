import dispatch._, dispatch.Defaults._
import com.github.nscala_time.time.DurationBuilder
import com.github.nscala_time.time.Imports._
import org.joda.time.{DateTime, Period, LocalDate}
import org.joda.time.format.ISODateTimeFormat
import java.io.PrintWriter
import scala.concurrent.Future

object Extensions {
  implicit class RichLocalDate(date: LocalDate) {
    def to(endDate: LocalDate): (Period) => Iterator[LocalDate] = step => Iterator.iterate(date)(_.plus(step)).takeWhile(_ <= endDate)
  }
  implicit class RichLocalDateIterator(filter: Period => Iterator[LocalDate]) {
    def withStep(period: Period) = filter(period)
  }
  case class SplitAt(n: Int) {
    def unapply[T](s: Seq[T]): Option[(Seq[T], Seq[T])] =
      if (s.isEmpty) None
      else Some(s.splitAt(n))
  }
}

object after {
  import java.util.{Timer, TimerTask}

  def apply[T](duration: DurationBuilder)(action: => T): Unit = timer.schedule(asTimerTask(action), duration.millis)

  private val timer = new Timer
  private def asTimerTask(action: => Unit) = new TimerTask { override def run() = action }
}

object main extends App {
  import Extensions._
  type Trip = Seq[OneTwoTrip.Flight]

  def log(msg: String) = println(s"${LocalDateTime.now.toString(DateTimeFormat.shortDateTime())} $msg")

  def getFilePath(date: DateTime, flights: Seq[OneTwoTrip.Flight]): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.from + f.to).mkString
    val timestamp = date.toString(ISODateTimeFormat.basicDateTimeNoMillis)
    val fileName = s"$timestamp $route.json"
    AppConfig.baseFolder + fileName
  }

  def saveToFile(filePath: String, content: String): Future[String] = Future {
    val writer = new PrintWriter(filePath)
    writer.write(content)
    writer.close()
    filePath
  }

  def loadTrip(trip: Trip): Future[Either[Throwable, String]] = {
    import scala.concurrent.duration._
    val filePath = getFilePath(DateTime.now, trip)

    log(s" Loading '$trip'...")
    for {
      content <- retry.Backoff(100, Duration(10, SECONDS), 2)(() => OneTwoTrip.Api.search(trip)).right
      filePath <- saveToFile(filePath, content)
    } yield Right(filePath)
  }

  def loadTrips(trips: Seq[Trip], maxConcurrentLoads: Int): Future[Seq[Either[Throwable, String]]] = {
    val split = SplitAt(maxConcurrentLoads)
    trips match {
      case split(chunk, Nil) => Future.sequence(chunk.map(loadTrip))
      case split(chunk, remaining) => for {
        c <- loadTrips(chunk, maxConcurrentLoads)
        r <- loadTrips(remaining, maxConcurrentLoads)
      } yield c ++ r
    }
  }

  val tripCombinations = for {
    trip     <- AppConfig.tripConfigs
    date     <- trip.minDate to trip.maxDate withStep 1.day
    duration <- trip.minDuration to trip.maxDuration
    flight        = new OneTwoTrip.Flight(trip.fromAirport, trip.toAirport, date)
    returnFlight  = new OneTwoTrip.Flight(trip.toAirport, trip.fromAirport, date.plusDays(duration))
  } yield Seq(flight, returnFlight)

  def check: Unit = {
    log(s"Loading ${tripCombinations.length} trip combinations...")

    loadTrips(tripCombinations, 3) onSuccess {
      case results => {
        log(s"${results.count(_.isRight)} out of ${tripCombinations.length} trips have been successfully loaded")
        log(s"The next check is scheduled on ${LocalDateTime.now.plusHours(24).toString(DateTimeFormat.shortDateTime())}")

        after(24.hours)(check)
      }
    }
  }

  check

  scala.io.StdIn.readLine()
}
