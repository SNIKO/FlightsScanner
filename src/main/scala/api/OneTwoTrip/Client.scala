package api.OneTwoTrip

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import api.OneTwoTrip.JsonProtocol._
import argonaut.Argonaut._
import utils.Utils.{ActionFailure, FutureActionResult}

import scala.concurrent.ExecutionContext.Implicits.global

sealed trait ServiceError
final class RequestLimitReachedError extends ServiceError
final case class APIError(msg: String) extends ServiceError

object Client {

  implicit val sys = ActorSystem()
  implicit val mat = ActorMaterializer()

  case class Flight(fromAirport: String, toAirport: String, date: LocalDate) {
    override def toString = fromAirport + "->" + toAirport + " " + DateTimeFormatter.ofPattern("dd MMM").format(date)
  }

  type Trip = Seq[Flight]

  def search(trip: Trip): FutureActionResult[ServiceError, SearchResponse] = {
    val url = getSearchUrl(trip)
    val response = for {
      response <- Http().singleRequest(HttpRequest(GET, url))
      responseEntity <- Unmarshal(response.entity).to[String]
    } yield responseEntity

    response.map {
      case Errors.requestLimitReached => ActionFailure(new RequestLimitReachedError())
      case json => json.decodeEither[SearchResponse].leftMap(error => new APIError(s"Failed to parse response for search request '$url'. $error"))
    }
  }

  private object Errors {
    val requestLimitReached = "{\"error\":\"REQUEST_LIMIT_REACHED\"}"
  }

  private def getSearchUrl(trip: Trip): String = {
    val r = trip.map(flight => s"${DateTimeFormatter.ofPattern("ddMM").format(flight.date)}" + flight.fromAirport + flight.toAirport).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$r&ad=1&cs=E"
  }
}