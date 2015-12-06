package flights

import java.time._
import java.time.format.DateTimeFormatter

import config.{AppConfig, TripConfig}
import utils.Implicits._
import utils.{After, Log, Utils}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

import utils.Utils.{ActionFailure, ActionSuccess}

class FaresScanner {
  val MaxConcurrentLoads = 3
  val CheckInterval = Duration.ofHours(24)

  val cities = flights.ReferenceData.airports.map(a => (a.iataCode, a.city)).toMap

  def getFilePath(flights: Seq[FlightDirection]): String = {
    val route = flights.map(f => DateTimeFormatter.ofPattern("ddMM").format(f.date) + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmssZ").format(LocalDateTime.now)
    val fileName = s"$timestamp $route.txt"

    AppConfig.baseFolder + "formatted\\" + fileName
  }

  def logError(route: Seq[FlightDirection], error: String) = Log(s"An error occurred when loading '$route': $error")

  def saveTripFares(allFares: Seq[Fare]) = {
    val faresByDirection = allFares.groupBy(f => {
      val origin = cities(f.itineraries.head.flights.head.fromAirport)
      val destination = cities(f.itineraries.head.flights.last.toAirport)

      (origin, destination)
    })

    faresByDirection.foreach {
      case ((origin, destination), fares) =>
        val formattedFares = fares.sortBy(_.prices.map(_.price).min).map(f => f.prettyPrint).mkString("\n")
        val tripDate = fares.head.itineraries.head.flights.head.departureDate
        val filePath = s"${AppConfig.baseFolder}formatted\\$origin - $destination ${DateTimeFormatter.ofPattern("ddMM").format(tripDate)}.txt"

        Utils.saveToFile(filePath, formattedFares)
    }
  }

  def getTripOptions(config: TripConfig): Seq[Seq[FlightDirection]] = {
    for {
      origin <- config.fromAirports
      destination <- config.toAirports
      date <- config.minDate to config.maxDate withStep Period.ofDays(1)
      duration <- config.minDuration to config.maxDuration
      flight = new FlightDirection(origin, destination, date)
      returnFlight = new FlightDirection(destination, origin, date.plusDays(duration))
    } yield Seq(flight, returnFlight)
  }

  def loadFares(route: Seq[FlightDirection]): Future[Seq[Fare]] = {
    Log(s"Loading fares for route: '$route'...")

    FaresProvider.OneTwoTrip.search(route) flatMap {
      case ActionSuccess(fares) =>
        Log(s"${fares.length} fares for $route have been loaded")
        Future(fares)
      case ActionFailure(ex) =>
        logError(route, ex.msg)
        After(Duration.ofMinutes(1))(loadFares(route))
    }
  }

  def loadFares(routes: Seq[Seq[FlightDirection]], maxConcurrentLoads: Int): Future[Seq[Fare]] = {
    routes.grouped(maxConcurrentLoads).foldLeft(Future(Seq.empty[Fare])) {
      (resultFuture, group) => {
        for {
          resultFares <- resultFuture
          groupFares <- Future.sequence(group.map(route => loadFares(route))).map(_.flatten)
        } yield resultFares ++ groupFares
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

  def scan(): Unit = {
    val startTime = Instant.now

    loadFares.onSuccess {
      case fares =>
        val endTime = Instant.now
        val elapsedTime = Duration.between(startTime, endTime)
        val timeToNextCheck = if (CheckInterval.minus(elapsedTime).isNegative) Duration.ofSeconds(1) else CheckInterval.minus(elapsedTime)
        val nextCheckTime = LocalDateTime.now.plus(timeToNextCheck)

        Log(s"${fares.length} have been successfully loaded")
        Log(s"The next check is scheduled on $nextCheckTime")

        After(timeToNextCheck)(Future(scan()))
    }
  }
}
