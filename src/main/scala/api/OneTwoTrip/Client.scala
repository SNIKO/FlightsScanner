package api.OneTwoTrip

import java.io.IOException
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, OffsetDateTime}

import api.OneTwoTrip.JsonProtocol._
import argonaut.Argonaut._
import config.AppConfig
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

trait SearchError
class RequestLimitReachedError extends SearchError
case class ResponseParseError(msg: String) extends SearchError
case class ServiceError(msg: String) extends SearchError

case class Flight(fromAirport: String, toAirport: String, date: LocalDate) {
  override def toString = fromAirport + "->" + toAirport + " " + DateTimeFormatter.ofPattern("dd MMM").format(date)
}

object Client {

  // TODO: Rewrite it using Spray
  def search(trip: Seq[Flight]): Future[Either[SearchError, SearchResponse]] = Future {
    try {
      val searchUrl = getSearchUrl(trip)
      val content = Source.fromURL(searchUrl).mkString

      content match {
        case Errors.requestLimitReached => Left(new RequestLimitReachedError())
        case faresAsJson => faresAsJson.decodeEither[SearchResponse].toEither match {
          case Right(fares) =>
            val filePath = AppConfig.baseFolder + getFileName(trip) + ".json"
            Utils.saveToFile(filePath, content)
            Right(fares)
          case Left(error) =>
            val filePath = AppConfig.baseFolder + "Errors\\" + getFileName(trip) + ".txt"
            val msg = searchUrl + "\n\n" + error

            Utils.saveToFile(filePath, msg)
            Left(ResponseParseError(error))
        }
      }
    } catch {
      case e: IOException => Left(ServiceError(e.getMessage))
    }
  }

  private object Errors {
    val requestLimitReached = "{\"error\":\"REQUEST_LIMIT_REACHED\"}"
  }

  private def getSearchUrl(route: Seq[Flight]): String = {
    val r = route.map(flight => s"${DateTimeFormatter.ofPattern("ddMM").format(flight.date)}" + flight.fromAirport + flight.toAirport).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$r&ad=1&cs=E"
  }

  private def getFileName(flights: Seq[Flight]): String = {
    val route = flights.map(f => DateTimeFormatter.ofPattern("ddMM").format(f.date) + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmssZ").format(OffsetDateTime.now)

    timestamp + " " + route
  }
}