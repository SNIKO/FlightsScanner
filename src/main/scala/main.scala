import OneTwoTrip.Api.Route
import OneTwoTrip.{Fare, LimitReachedException}
import com.github.nscala_time.time.Imports._
import config.AppConfig
import dispatch.Defaults._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import utils.Implicits._
import utils.{After, Log, SplitAt, Utils}

import scala.concurrent.Future
import scala.util.Success

object main extends App {

  def getFilePath(flights: Route): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTime.now.toString(ISODateTimeFormat.basicDateTimeNoMillis)
    val fileName = s"$timestamp $route.txt"

    AppConfig.baseFolder + "formatted\\" + fileName
  }

  def logError(route: Route, error: String) = Log(s"An error occurred when loading '$route': $error")

  def saveFares(route: Route, fares: Seq[Fare]) = {
    if (fares.length > 0) {
      val file = getFilePath(route)
      val formattedFares = fares.sortBy(_.price).map(f => f.prettyPrint).mkString("\n")

      Utils.saveToFile(file, formattedFares)
    }
  }

  def loadRoute(route: Route): Future[Either[Throwable, Seq[Fare]]] = {
    Log(s"Loading fares for route: '$route'...")

    OneTwoTrip.Api.search(route) flatMap {
      case Right(fares) =>
        Log(s"${fares.length} fares for $route have been loaded")
        saveFares(route, fares)
        Future.fromTry(Success(Right(fares)))
      case Left(ex) => ex match {
        case e: LimitReachedException =>
          logError(route, "Requests limit reached")
          After(90.minutes)(loadRoute(route))
        case e =>
          logError(route, e.getMessage)
          After(1.minute)(loadRoute(route))
      }
    }
  }

  def loadRoutes(routes: Seq[Route], maxConcurrentLoads: Int): Future[Seq[Either[Throwable, Seq[Fare]]]] = {
    val split = SplitAt(maxConcurrentLoads)
    routes match {
      case split(chunk, Nil) => Future.sequence(chunk.map(loadRoute))
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

  def check = {
    Log(s"Loading ${routeCombinations.length} route combinations...")
    loadRoutes(routeCombinations, 3)
  }

  check.onSuccess {
    case results => {
      Log(s"${results.count(_.isRight)} out of ${routeCombinations.length} routes have been successfully loaded")
      Log(s"The next check is scheduled on ${LocalDateTime.now.plusHours(22).toString(DateTimeFormat.shortDateTime())}")

      After(22.hours)(check)
    }
  }

  scala.io.StdIn.readLine()
}
