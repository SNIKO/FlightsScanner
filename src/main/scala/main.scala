import OneTwoTrip.Api.Route
import OneTwoTrip.{Fare, LimitReachedException}
import com.github.nscala_time.time.Imports._
import config.{AppConfig, TripConfig}
import dispatch.Defaults._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import utils.Implicits._
import utils.{After, Log, Utils}

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

  def saveTripFares(allFares: Seq[Fare]) = {
    val faresByDirection = allFares.groupBy(f => {
      val origin      = f.flights.head.segments.head.fromAirport
      val destination = f.flights.head.segments.last.toAirport

      (origin, destination)
    })

    faresByDirection.foreach {
      case ((origin, destination), fares) =>
        val formattedFares = fares.sortBy(_.price).map(f => f.prettyPrint).mkString("\n")
        val tripDate = fares.head.flights.head.segments.head.departureDate
        val filePath = s"${AppConfig.baseFolder}formatted\\$origin $destination ${tripDate.toString("ddMM")}.txt"

        Utils.saveToFile(filePath, formattedFares)
    }
  }

  def getTripOptions(config: TripConfig): Seq[Seq[OneTwoTrip.Direction]] = {
    for {
      origin      <- config.fromAirports
      destination <- config.toAirports
      date        <- config.minDate to config.maxDate withStep 1.day
      duration    <- config.minDuration to config.maxDuration
      flight       = new OneTwoTrip.Direction(origin, destination, date)
      returnFlight = new OneTwoTrip.Direction(destination, origin, date.plusDays(duration))
    } yield Seq(flight, returnFlight)
  }
  
  def loadFares(route: Route): Future[Seq[Fare]] = {
    Log(s"Loading fares for route: '$route'...")

    OneTwoTrip.Api.search(route) flatMap {
      case Right(fares) =>
        Log(s"${fares.length} fares for $route have been loaded")
        Future(fares)
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

  def loadFares(routes: Seq[Route], maxConcurrentLoads: Int): Future[Seq[Fare]] = {
    routes.grouped(maxConcurrentLoads).foldLeft(Future(Seq.empty[Fare])) {
      (resultFuture, slice) => {
        for {
          resultFares <- resultFuture
          sliceFares <- Future.sequence(slice.map(route => loadFares(route))).map(_.flatten)
        } yield resultFares ++ sliceFares
      }
    }
  }

  def loadFares(config: TripConfig, maxConcurrentLoads: Int): Future[Seq[Fare]] = {
    val tripOptions = getTripOptions(config)
    loadFares(tripOptions, maxConcurrentLoads).andThen { case Success(allFares) => saveTripFares(allFares) }
  }
  
  def loadFares: Future[Seq[Fare]] = {
    Log(s"Loading fares for ${AppConfig.tripConfigs.length} trip configurations...")

    AppConfig.tripConfigs.foldLeft(Future(Seq.empty[Fare])) {
      (resultFuture, config) =>
        for {
          resultFares <- resultFuture
          configFares <- loadFares(config, MaxConcurrentLoads)
        } yield resultFares ++ configFares
    }
  }

  loadFares.onComplete {
    case Success(fares) =>
      Log(s"${fares.length} have been successfully loaded")
      Log(s"The next check is scheduled on ${LocalDateTime.now.plusHours(22).toString(DateTimeFormat.shortDateTime())}")

      After(22.hours)(loadFares)
    case Failure(ex) => println(ex)
  }

  scala.io.StdIn.readLine()
}
