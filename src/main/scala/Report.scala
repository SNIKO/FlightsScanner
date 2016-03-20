import flights.Fare
import utils.Implicits._

object Report {

  def generate(fares: Seq[Fare]): String = {
    val sb = new StringBuilder
    sb.appendLine(s"${fares.length} options found")
    sb.appendLine("")
    sb.appendLine(faresByCity(fares))
    sb.appendLine("")
    sb.appendLine(faresByAirline(fares))
    sb.appendLine("")
    sb.appendLine(faresFlatList(fares))

    sb.mkString
  }

  def faresByCity(fares: Seq[Fare]): String = {
    val citiesWithPrices = for {
      fare <- fares
      itinerary <- fare.itineraries
      flight <- itinerary.flights
      city = getCityAirportName(flight.toAirport)
    } yield (city, fare.prices)

    val groupedByCities = citiesWithPrices
      .groupBy(m => m._1)
      .map(cityPrices => {
        val city = cityPrices._1
        val prices = cityPrices._2.flatMap(_._2)
        val minPrice = prices.filter(_.currency == "USD").minBy(_.price)

        (city, minPrice)})
      .toSeq
      .sortBy(_._2.price)

    val sb = new StringBuilder
    sb.appendLine("Fares by City:")
    groupedByCities.foreach(g => sb.appendLine(f"${g._1}%18s\t${g._2}"))

    sb.mkString
  }

  def faresByAirline(fares: Seq[Fare]): String = {
    val airlinesWithPrices = for {
      fare <- fares
      itinerary <- fare.itineraries
      flight <- itinerary.flights
      airline = getAirlineName(flight.airline)
    } yield (airline, fare.prices)

    val groupedByAirlines = airlinesWithPrices
      .groupBy(m => m._1)
      .map(airlinePrices => {
        val airline = airlinePrices._1
        val prices = airlinePrices._2.flatMap(_._2)
        val minPrice = prices.filter(_.currency == "USD").minBy(_.price)

        (airline, minPrice)})
      .toSeq
      .sortBy(_._2.price)

    val sb = new StringBuilder
    sb.appendLine("Fares by Airline:")
    groupedByAirlines.foreach(g => sb.appendLine(f"${g._1}%30s\t${g._2}"))

    sb.mkString
  }

  def faresFlatList(fare: Seq[Fare]) = {
    val sb = new StringBuilder
    sb.appendLine("All fares:")
    fare.sortBy(_.prices.map(_.price).min).foreach(f => sb.appendLine(f.prettyPrint))

    sb.mkString
  }

  def getCityAirportName(iataCode: String) = {
    flights.ReferenceData.airports
      .find(a => a.iataCode == iataCode)
      .map(a => s"${a.city} ($iataCode})")
      .getOrElse(iataCode)
  }

  def getAirlineName(iataCode: String) = {
    flights.ReferenceData.airlines.find(a => a.iataCode == iataCode).map(a => a.name).getOrElse(iataCode)
  }
}
