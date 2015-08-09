package OneTwoTrip

import java.io.IOException
import java.time.{OffsetDateTime, LocalDate}
import java.time.format.DateTimeFormatter

import config.AppConfig
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}

class Direction(val fromAirport: String, val toAirport: String, val date: LocalDate) {
  override def toString = fromAirport + "->" + toAirport + " " + DateTimeFormatter.ofPattern("dd MMM").format(date)
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
    val r = route.map(flight => s"${DateTimeFormatter.ofPattern("ddMM").format(flight.date)}" + flight.fromAirport + flight.toAirport).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$r&ad=1&cs=E"
  }

  private def getFileName(flights: Route): String = {
    val route = flights.map(f => DateTimeFormatter.ofPattern("ddMM").format(f.date) + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmssZ").format(OffsetDateTime.now)

    timestamp + " " + route
  }
}