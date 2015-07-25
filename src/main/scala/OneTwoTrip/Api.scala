package OneTwoTrip

import java.io.IOException

import com.github.nscala_time.time.Imports._
import config.AppConfig
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}

class Direction(val fromAirport: String, val toAirport: String, val date: LocalDate) {
  override def toString = fromAirport + "->" + toAirport + " " + date.toString("dd MMM")
}

class LimitReachedException extends Exception

object Api {

  type Route = Seq[Direction]

  // TODO: Rewrite it using Spray
  def search(trip: Route): Future[Either[Throwable, Seq[Fare]]] = Future {
    try {
      val searchUrl = getSearchUrl(trip)
      val content = Source.fromURL(searchUrl).mkString

      content match {
        case Errors.requestLimitReached => Left(new LimitReachedException())
        case faresAsJson => JsonProtocol.parse(faresAsJson) match {
          case Success(fares) =>
            val filePath = AppConfig.baseFolder + getFileName(trip) + ".json"
            Utils.saveToFile(filePath, content)
            Right(fares)
          case Failure(ex) =>
            val filePath = AppConfig.baseFolder + "Errors\\" + getFileName(trip) + ".txt"
            val msg = searchUrl + "\n\n" + ex.getMessage

            Utils.saveToFile(filePath, msg)
            Left(ex)
        }
      }
    } catch {
      case e: IOException => Left(e)
    }
  }

  private object Errors {
    val requestLimitReached = "{\"error\":\"REQUEST_LIMIT_REACHED\"}"
  }

  private def getSearchUrl(route: Route): String = {
    val r = route.map(flight => s"${flight.date.toString("ddMM")}" + flight.fromAirport + flight.toAirport).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$r&ad=1&cs=E"
  }

  private def getFileName(flights: Route): String = {
    val route = flights.map(f => f.date.toString("ddMM") + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTime.now.toString(ISODateTimeFormat.basicDateTimeNoMillis)

    timestamp + " " + route
  }
}