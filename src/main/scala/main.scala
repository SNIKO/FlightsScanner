import OneTwoTrip.Api.Route
import OneTwoTrip.{Fare, LimitReachedException}
import com.github.nscala_time.time.Imports._
import config.{AppConfig, SearchConfig}
import dispatch.Defaults._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import utils.Implicits._
import utils.{After, Log, SplitAt, Utils}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object main extends App {

  val MaxConcurrentLoads = 3

  def getFilePath(flights: Route): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTime.now.toString(ISODateTimeFormat.basicDateTimeNoMillis)
    val fileName = s"$timestamp $route.txt"

    AppConfig.baseFolder + "formatted\\" + fileName
  }

  def logError(route: Route, error: String) = Log(s"An error occurred when loading '$route': $error")

  def saveFares(allFares: Seq[Fare]) = {
    val faresByDirection = allFares.groupBy(f => (f.flights.head.segments.head.fromAirport, f.flights.head.segments.last.toAirport))

    faresByDirection.foreach {
      case ((fromAirport, toAirport), fares) =>
        val formattedFares = fares.sortBy(_.price).map(f => f.prettyPrint).mkString("\n")
        val file = s"${AppConfig.baseFolder}formatted\\$fromAirport $toAirport ${fares.head.flights.head.segments.head.departureDate.toString("ddMM")}.txt"

        Utils.saveToFile(file, formattedFares)
    }
  }

  def loadFares(route: Route): Future[Either[Throwable, Seq[Fare]]] = {
    Log(s"Loading fares for route: '$route'...")

    OneTwoTrip.Api.search(route) flatMap {
      case Right(fares) =>
        Log(s"${fares.length} fares for $route have been loaded")
        Future.fromTry(Success(Right(fares)))
      case Left(ex) => ex match {
        case e: LimitReachedException =>
          logError(route, "Requests limit reached")
          After(5.minutes)(loadFares(route))
        case e =>
          logError(route, e.getMessage)
          After(1.minute)(loadFares(route))
      }
    }
  }

  def loadFaresInParallel(batch: Seq[Route]): Future[Seq[Fare]] = {
    val routeFutures = batch.map(loadFares)
    val batchFuture = Future.sequence(routeFutures)

    batchFuture.map(_.flatMap(_.right.get))
  }

  def loadFares(routes: Seq[Route], maxConcurrentLoads: Int): Future[Seq[Fare]] = {
    val split = SplitAt(maxConcurrentLoads)

    routes match {
      case split(batch, Nil) => loadFaresInParallel(batch)
      case split(batch, remaining) => for {
        b <- loadFares(batch, maxConcurrentLoads)
        r <- loadFares(remaining, maxConcurrentLoads)
      } yield b ++ r
    }
  }

  def loadFares(config: SearchConfig, maxConcurrentLoads: Int): Future[Seq[Fare]] = {
    val routes = for {
      origin      <- config.fromAirports
      destination <- config.toAirports
      date        <- config.minDate to config.maxDate withStep 1.day
      duration    <- config.minDuration to config.maxDuration
      flight       = new OneTwoTrip.Direction(origin, destination, date)
      returnFlight = new OneTwoTrip.Direction(destination, origin, date.plusDays(duration))
    } yield Seq(flight, returnFlight)

    loadFares(routes, maxConcurrentLoads).andThen { case Success(allFares) => saveFares(allFares) }
  }

  def check = {
    Log(s"Loading fares for ${AppConfig.tripConfigs.length} trip configurations...")

    AppConfig.tripConfigs.foldLeft(Future(Seq.empty[Fare])) {
      (previousFutures, nextConfig) =>
        for {
          previousFares <- previousFutures
          newFares <- loadFares(nextConfig, MaxConcurrentLoads)
        } yield previousFares ++ newFares
    }
  }

  check.onComplete {
    case Success(fares) =>
      Log(s"${fares.length} have been successfully loaded")
      Log(s"The next check is scheduled on ${LocalDateTime.now.plusHours(22).toString(DateTimeFormat.shortDateTime())}")

      After(22.hours)(check)
    case Failure(ex) => println(ex)
  }

  scala.io.StdIn.readLine()
}
