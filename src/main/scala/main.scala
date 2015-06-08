import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Failure, Success}
import java.io.PrintWriter
import com.github.nscala_time.time.Imports._
import org.joda.time.format.ISODateTimeFormat

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

object main extends App {

  import Extensions._

  type Trip = Seq[OneTwoTrip.Flight]

  def getFilePath(date: DateTime, flights: Seq[OneTwoTrip.Flight]): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.from + f.to).mkString
    val timestamp = date.toString(ISODateTimeFormat.basicDateTimeNoMillis)
    val fileName = s"$timestamp $route.json"
    AppConfig.baseFolder + fileName
  }

  def saveToFile(filePath: String, content: Array[Char]): Future[String] = Future {
    val writer = new PrintWriter(filePath)
    writer.write(content)
    writer.close()

    filePath
  }

  def loadTrip(trip: Trip): Future[String] = {
    println(s"Loading $trip...")
    for {
      content <- OneTwoTrip.Api.search(trip)
      filePath = getFilePath(DateTime.now, trip)
      filePath <- saveToFile(filePath, content)
    } yield filePath
  }

  case class TripLoadException(trip: Trip, ex: Throwable) extends Throwable
  def loadTripWithRecover(trip: Trip): Future[Try[String]] = loadTrip(trip).map(Success(_)) recover { case ex: Exception => Failure(TripLoadException(trip, ex)) }

  def loadTrips(trips: Seq[Trip], maxConcurrentLoads: Int): Future[Seq[Try[String]]] = {
    val split = SplitAt(maxConcurrentLoads)
    trips match {
      case split(chunk, Nil) => Future.sequence(chunk.map(loadTripWithRecover))
      case split(chunk, remaining) => for {
        c <- loadTrips(chunk, maxConcurrentLoads)
        r <- loadTrips(remaining, maxConcurrentLoads)
      } yield c ++ r
    }
  }

  val flightCombinations = (for {
    trip <- AppConfig.tripConfigs
    date <- trip.minDate to trip.maxDate withStep 1.day
    duration <- trip.minDuration to trip.maxDuration
    flight = new OneTwoTrip.Flight(trip.fromAirport, trip.toAirport, date)
    returnFlight = new OneTwoTrip.Flight(trip.toAirport, trip.fromAirport, date.plusDays(duration))
  } yield Seq(flight, returnFlight)).drop(40)

  println(s"${flightCombinations.length} flight combinations found")

  loadTrips(flightCombinations, 3) onSuccess {
    case res => {
      val failed = res.filter(_.isFailure)
      println(s"The search has been completed. ${failed.length} requests failed:")
      failed foreach { case Failure(TripLoadException(trip, ex)) => println(trip + ": " + ex) }
  }}

  scala.io.StdIn.readLine()
}
