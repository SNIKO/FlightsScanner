package OneTwoTrip

import com.github.nscala_time.time.Imports._
import config.AppConfig
import dispatch._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import utils.{Log, Utils}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class Direction(val fromAirport: String, val toAirport: String, val date: LocalDate) {
  override def toString = fromAirport + "->" + toAirport + " " + date.toString("dd MMM")
}

object Api {
  type Route = Seq[Direction]

  // Constants
  private val ResponseTimeout = 5.minutes

  def search(trip: Route): Future[Either[Throwable, Seq[Fare]]] = {
    val request = url(getSearchUrl(trip))

    http(request).either.map {
      case Left(error) =>
        logError(trip, error.getMessage)
        Left(error)
      case Right(response) => response.getResponseBody match {
        case Errors.requestLimitReached =>
          logError(trip, "Request limit reached")
          Left(new Exception("Request limit reached"))
        case content => JsonProtocol.parse(content) match {
          case Success(trips) => {
            val filePath = getFilePath(trip)
            Utils.saveToFile(filePath, content)
            Right(trips)
          }
          case Failure(ex) =>
            logError(trip, s"Failed to parse response")
            Left(ex)
        }
      }
    }
  }

  private val http = Http.configure(_.setRequestTimeoutInMs(ResponseTimeout.millis.toInt))

  private object Errors {
    val requestLimitReached = "{\"error\":\"REQUEST_LIMIT_REACHED\"}"
  }

  private def getSearchUrl(route: Route): String = {
    val r = route.map(flight => s"${flight.date.toString("ddMM")}" + flight.fromAirport + flight.toAirport).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$r&ad=1&cs=E"
  }

  private def getFilePath(flights: Route): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTime.now.toString(ISODateTimeFormat.basicDateTimeNoMillis)
    val fileName = s"$timestamp $route.json"
    AppConfig.baseFolder + fileName
  }

  private def logError(route: Route, error: String) = Log(s"An error occurred when loading '$route': $error")
}