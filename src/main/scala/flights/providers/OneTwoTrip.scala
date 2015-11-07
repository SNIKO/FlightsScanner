package flights.providers

import java.time.{LocalDateTime, OffsetDateTime}

import api.OneTwoTrip.SearchResponse
import flights._

import scala.concurrent.Future
import scala.util.{Success, Failure, Try}

import scala.concurrent.ExecutionContext.Implicits.global

class OneTwoTrip extends FaresProvider{

  def search(directions: Seq[FlightDirection]): Future[Either[FaresProviderError, Seq[Fare]]] = {
    val flightsToSearch = directions.map(d => api.OneTwoTrip.Flight(d.fromAirport, d.toAirport, d.date))

    api.OneTwoTrip.Api.search(flightsToSearch).map {
      case Success(response) =>
        parse(response) match {
          case Success(fares) => Right(fares)
          case Failure(ex) =>
            // TODO: Log
            Left(FaresProviderError(ex.getMessage))
        }
      case Failure(ex) =>
        // TODO: Log
        Left(FaresProviderError(ex.getMessage))
    }
  }

  def parse(response: SearchResponse): Try[Seq[Fare]] = Try {
    val fares = response.fares.map(f => {
      val directions = f.directions.map(d => {
        val trips = d.trips.map(t => {
          val trip = response.trips(t.id)
          val plane = response.planes.find(p => p.code == trip.plane).map(_.name).getOrElse(trip.plane)
          val localDateTime = LocalDateTime.of(trip.date, trip.time)
          val offsetDateTime = OffsetDateTime.of(localDateTime, timeZoneOffsets(trip.from))

          Flight(offsetDateTime, trip.from, trip.to, trip.airline, trip.flightNumber, trip.operatedBy, plane, t.reservationClass, t.cabinClass)
        })

        Itenerary(trips)
      })

      val price = f.priceInfo.adultFare + f.priceInfo.adultTaxes + f.priceInfo.markup
      val usdPrice = f.priceInfo.currency match {
        case "USD" => price
        case currency =>
          val rate = response.rates.find(r => r.currencyFrom == currency && r.currencyTo == "USD").head
          price * rate.factor
      }

      Fare(directions, usdPrice, OffsetDateTime.now())
    })

    fares
  }

  val timeZoneOffsets = flights.ReferenceData.airports.map(a => (a.iataCode, a.zoneOffset)).toMap
}
