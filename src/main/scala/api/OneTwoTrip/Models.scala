package api.OneTwoTrip

import java.time.format.DateTimeFormatter
import java.time.{LocalTime, LocalDate}

case class Flight(fromAirport: String, toAirport: String, date: LocalDate) {
  override def toString = fromAirport + "->" + toAirport + " " + DateTimeFormatter.ofPattern("dd MMM").format(date)
}

case class SearchResponse(fares: Seq[Fare], trips: Seq[Trip], planes: Seq[Plane], rates: Seq[Rate])

case class Fare(id: Int, directions: Seq[Direction], priceInfo: PriceInfo)

case class Direction(id: Int, trips: Seq[TripRef])

case class TripRef(id: Int, reservationClass: String, cabinClass: String)

case class PriceInfo(adultFare: BigDecimal, adultTaxes: BigDecimal, currency: String, markup: BigDecimal)

case class Trip(date: LocalDate,
                time: LocalTime,
                from: String,
                to: String,
                airline: String,
                flightNumber: String,
                operatedBy: Option[String],
                plane: String)

case class Plane(code: String, name: String)

case class Rate(currencyFrom: String, currencyTo: String, factor: BigDecimal)