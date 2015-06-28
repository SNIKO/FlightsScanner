package OneTwoTrip

import com.github.nscala_time.time.Imports._

case class Plane(code: String, name: String)

case class Segment(departureDate: DateTime,
                   fromAirport: String,
                   toAirport: String,
                   airline: String,
                   flightNumber: String,
                   operatedBy: Option[String],
                   plane: String,
                   reservationClass: String,
                   cabinClass: String)

case class Flight(segments: Seq[Segment])

case class Fare(flights: Seq[Flight], price: BigDecimal) {

  def prettyPrint: String = {
    val airports = flights.map(_.segments.head.fromAirport).mkString("-")
    val dates = flights.map(_.segments.head.departureDate.toString("dd MMM")).mkString(", ")

    val sb = new StringBuilder
    sb.append(s"$airports \t $dates \t Price: USD $price").append("\n")
    flights.zipWithIndex foreach { case (flight, index) =>
        sb.append(s"\tFlight $index").append("\n")
        flight.segments foreach { case segment =>
            sb.append(f"\t\t${segment.airline} ${segment.flightNumber}%4s ${segment.fromAirport} ${segment.toAirport} ${segment.departureDate}").append("\n")}}

    sb.mkString
  }
}