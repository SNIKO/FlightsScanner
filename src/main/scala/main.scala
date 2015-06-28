import OneTwoTrip.Api.Route
import OneTwoTrip.Fare
import com.github.nscala_time.time.Imports._
import config.AppConfig
import dispatch.Defaults._
import dispatch._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import utils.Implicits._
import utils._

import scala.concurrent.Future
import scala.util.Success

object main extends App {

  private def getFilePath(flights: Route): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTime.now.toString(ISODateTimeFormat.basicDateTimeNoMillis)
    val fileName = s"$timestamp $route.txt"

    AppConfig.baseFolder + "formatted\\" + fileName
  }

  def loadRoutes(route: Route): Future[Either[Throwable, Seq[Fare]]] = {
    import scala.concurrent.duration._

    Log(s"Loading '$route'...")

    retry.Backoff(100, Duration(10, SECONDS), 2)(() => OneTwoTrip.Api.search(route)).andThen {
      case Success(res) => res match {
        case Right(fares) => {
          val formattedFares = fares
            .sortBy(_.price)
            .map(f => f.prettyPrint)
            .mkString("\n")

          Utils.saveToFile(getFilePath(route), formattedFares)
        }
        case Left(_) =>
      }
    }
  }

  def loadRoutes(routes: Seq[Route], maxConcurrentLoads: Int): Future[Seq[Either[Throwable, Seq[Fare]]]] = {
    val split = SplitAt(maxConcurrentLoads)
    routes match {
      case split(chunk, Nil) => Future.sequence(chunk.map(loadRoutes))
      case split(chunk, remaining) => for {
        c <- loadRoutes(chunk, maxConcurrentLoads)
        r <- loadRoutes(remaining, maxConcurrentLoads)
      } yield c ++ r
    }
  }

  val routeCombinations = for {
    trip        <- AppConfig.tripConfigs
    origin      <- trip.fromAirports
    destination <- trip.toAirports
    date        <- trip.minDate to trip.maxDate withStep 1.day
    duration    <- trip.minDuration to trip.maxDuration
    flight        = new OneTwoTrip.Direction(origin, destination, date)
    returnFlight  = new OneTwoTrip.Direction(destination, origin, date.plusDays(duration))
  } yield Seq(flight, returnFlight)

  def check: Unit = {
    Log(s"Loading ${routeCombinations.length} route combinations...")

    loadRoutes(routeCombinations, 3) onSuccess {
      case results => {
        Log(s"${results.count(_.isRight)} out of ${routeCombinations.length} routes have been successfully loaded")
        Log(s"The next check is scheduled on ${LocalDateTime.now.plusHours(22).toString(DateTimeFormat.shortDateTime())}")

        After(22.hours)(check)
      }
    }
  }

  check

  scala.io.StdIn.readLine()
}
