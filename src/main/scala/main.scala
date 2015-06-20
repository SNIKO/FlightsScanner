import dispatch._, dispatch.Defaults._
import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import java.io.PrintWriter
import scala.concurrent.Future
import utils._
import utils.Implicits._

object main extends App {

  type Trip = Seq[OneTwoTrip.Flight]

  def getFilePath(flights: Seq[OneTwoTrip.Flight]): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.from + f.to).mkString
    val timestamp = DateTime.now.toString(ISODateTimeFormat.basicDateTimeNoMillis)
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

    Log(s"Loading '$trip'...")
    for {
      content <- retry.Backoff(100, Duration(10, SECONDS), 2)(() => OneTwoTrip.Api.search(trip)).right
      filePath <- saveToFile(getFilePath(trip), content)
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
    trip        <- AppConfig.tripConfigs
    origin      <- trip.fromAirports
    destination <- trip.toAirports
    date        <- trip.minDate to trip.maxDate withStep 1.day
    duration    <- trip.minDuration to trip.maxDuration
    flight        = new OneTwoTrip.Flight(origin, destination, date)
    returnFlight  = new OneTwoTrip.Flight(destination, origin, date.plusDays(duration))
  } yield Seq(flight, returnFlight)

  def check: Unit = {
    Log(s"Loading ${tripCombinations.length} trip combinations...")

    loadTrips(tripCombinations, 3) onSuccess {
      case results => {
        Log(s"${results.count(_.isRight)} out of ${tripCombinations.length} trips have been successfully loaded")
        Log(s"The next check is scheduled on ${LocalDateTime.now.plusHours(24).toString(DateTimeFormat.shortDateTime())}")

        After(24.hours)(check)
      }
    }
  }

  check

  scala.io.StdIn.readLine()
}
