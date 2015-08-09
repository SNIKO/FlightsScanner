import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, LocalDateTime, Period}

import OneTwoTrip.Api.Route
import OneTwoTrip.{Fare, LimitReachedException}
import config.{AppConfig, TripConfig}
import utils.Implicits._
import utils.{After, Log, Utils}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

object main extends App {

  val MaxConcurrentLoads = 3
  val CheckInterval = Duration.ofHours(24)

  val cities = flights.ReferenceData.airports.map(a => (a.iataCode, a.city)).toMap

  def getFilePath(flights: Route): String = {
    val route = flights.map(f => DateTimeFormatter.ofPattern("ddMM").format(f.date) + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmssZ").format(LocalDateTime.now)
    val fileName = s"$timestamp $route.txt"

    AppConfig.baseFolder + "formatted\\" + fileName
  }

  def logError(route: Route, error: String) = Log(s"An error occurred when loading '$route': $error")

  def saveTripFares(allFares: Seq[Fare]) = {
    val faresByDirection = allFares.groupBy(f => {
      val origin      = cities(f.flights.head.segments.head.fromAirport)
      val destination = cities(f.flights.head.segments.last.toAirport)

      (origin, destination)
    })

    faresByDirection.foreach {
      case ((origin, destination), fares) =>
        val formattedFares = fares.sortBy(_.price).map(f => f.prettyPrint).mkString("\n")
        val tripDate = fares.head.flights.head.segments.head.departureDate
        val filePath = s"${AppConfig.baseFolder}formatted\\$origin - $destination ${DateTimeFormatter.ofPattern("ddMM").format(tripDate)}.txt"

        Utils.saveToFile(filePath, formattedFares)
    }
  }

  def getTripOptions(config: TripConfig): Seq[Route] = {
    for {
      origin      <- config.fromAirports
      destination <- config.toAirports
      date        <- config.minDate to config.maxDate withStep Period.ofDays(1)
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
          After(Duration.ofMinutes(5))(loadFares(route))
        case e =>
          logError(route, e.getMessage)
          After(Duration.ofMinutes(1))(loadFares(route))
      }
    }
  }

  def loadFares(routes: Seq[Route], maxConcurrentLoads: Int): Future[Seq[Fare]] = {
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

  def checkFares(): Unit = {
    val startTime = Instant.now

    loadFares.onSuccess {
      case fares =>
        val endTime = Instant.now
        val elapsedTime = Duration.between(startTime, endTime)
        val timeToNextCheck = if (CheckInterval.minus(elapsedTime).isNegative) Duration.ofSeconds(1) else CheckInterval.minus(elapsedTime)
        val nextCheckTime = LocalDateTime.now.plus(timeToNextCheck)

        Log(s"${fares.length} have been successfully loaded")
        Log(s"The next check is scheduled on ${nextCheckTime}")

        After(timeToNextCheck)(Future(checkFares()))
    }
  }

  checkFares()

  scala.io.StdIn.readLine()
}
