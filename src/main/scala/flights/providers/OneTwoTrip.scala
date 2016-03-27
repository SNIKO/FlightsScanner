package flights.providers

import java.time.{LocalDateTime, OffsetDateTime}

import api.OneTwoTrip.JsonProtocol._
import flights._
import utils.Implicits._
import utils.Utils.{ActionFailure, ActionResult, ActionSuccess, FutureActionResult}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalaz.EitherT

class OneTwoTrip extends FaresProvider{

  def search(trip: flights.Trip): FutureActionResult[FaresProviderError, Seq[flights.Fare]] = {
    val flightsToSearch = trip.flights.map(d => api.OneTwoTrip.Client.Flight(d.fromAirport, d.toAirport, d.date))

    val res = for {
      response <- EitherT(api.OneTwoTrip.Client.search(flightsToSearch))
      fares <- EitherT(Future(parse(response)))
    } yield fares

    res.leftMap(error => FaresProviderError("OneTwoTrip", s"Failed to load fares for route '$trip'. $error")).run
  }

  // TODO error handling
  def parse(response: SearchResponse): ActionResult[String, Seq[flights.Fare]] = try {
    val fares = response.fares.map(f => {
      val directions = f.directions.map(d => {
        val trips = d.trips.map(t => {
          val trip = response.trips(t.id)
          val plane = response.planes.find(p => p.code == trip.plane).map(_.name).getOrElse(trip.plane)
          val departureTime = OffsetDateTime.of(LocalDateTime.of(trip.date, trip.time), timeZoneOffsets(trip.from))
          val arrivalTime = departureTime.plus(trip.duration).withOffsetSameInstant(timeZoneOffsets(trip.to))

          Flight(departureTime, arrivalTime, trip.from, trip.to, trip.airline, trip.flightNumber, trip.operatedBy, plane, t.reservationClass, t.cabinClass)
        })

        Itinerary(trips)
      })

      val price = f.priceInfo.adultFare + f.priceInfo.adultTaxes + f.priceInfo.markup
      val usdPrice = f.priceInfo.currency match {
        case "USD" => price
        case currency =>
          val rate = response.rates.find(r => r.currencyFrom == currency && r.currencyTo == "USD").head
          price * rate.factor.toDouble
      }

      flights.Fare(directions, OffsetDateTime.now(), Seq(flights.PriceInfo(usdPrice, "USD", "OneTwoTrip")))
    })

    ActionSuccess(fares)
  } catch {
    case e: Throwable => ActionFailure(e.toString)
  }

  val timeZoneOffsets = flights.ReferenceData.airports.map(a => (a.iataCode, a.zoneOffset)).toMap
}