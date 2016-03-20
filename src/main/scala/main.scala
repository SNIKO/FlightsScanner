import java.time._
import java.time.format.DateTimeFormatter

import api.yahoo.Finance._
import api.yahoo.Model._
import config.AppConfig
import flights._
import utils.Implicits._
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scalaz.EitherT

object main extends App {

  val cities = flights.ReferenceData.airports.map(a => (a.iataCode, a.city)).toMap
  val targetCurrency = "USD"

  def saveTripFares(allFares: Seq[Fare]) = {
    val faresByDirection = allFares.groupBy(f => {
      val origin = cities(f.itineraries.head.flights.head.fromAirport)
      val destination = cities(f.itineraries.head.flights.last.toAirport)

      (origin, destination)
    })

    faresByDirection.foreach {
      case ((origin, destination), flights) =>
        val tripDate = flights.head.itineraries.head.flights.head.departureDate
        val filePath = s"${AppConfig.baseFolder}formatted\\$origin - $destination ${DateTimeFormatter.ofPattern("ddMM").format(tripDate)}.txt"

        Utils.saveToFile(filePath, Report.generate(flights))
    }
  }

  var trips = for {
    config      <- AppConfig.tripConfigs
    origin      <- config.fromAirports
    destination <- config.toAirports
    date        <- config.minDate to config.maxDate withStep Period.ofDays(1)
    duration    <- config.minDuration to config.maxDuration
    flight        = new FlightDirection(origin, destination, date)
    returnFlight  = new FlightDirection(destination, origin, date.plusDays(duration))
  } yield Trip(Seq(flight, returnFlight))

  val scanFuture = for {
    fares <- Scanner.search(trips)
    exchangeRates <- {
      val pairs = fares
        .flatMap(f => f.prices.map(p => p.currency))
        .filter(c => c != targetCurrency)
        .distinct.map(c => CurrenciesPair(c, targetCurrency))

      val rates = EitherT(getCurrencyRate(pairs)).map(q => q.query.results.rates)
      rates.getOrElse(Seq.empty[api.yahoo.Model.Rate])
    }
    faresWithTargetCurrency = fares.map(fare => {
      val pricesToConvert = fare.prices.filter(p => p.currency != targetCurrency)
      val convertedPrices = pricesToConvert.map(p => {
        val rate = exchangeRates.find(r => r.from(p.currency) && r.to(targetCurrency))
        rate.map(r => flights.PriceInfo(p.price * r.rate.toDouble, targetCurrency, p.provider))
      })

      fare.copy(prices = fare.prices ++ convertedPrices.flatten)
    })
  } yield faresWithTargetCurrency

  scanFuture.onComplete {
    case Success(fares) =>
      saveTripFares(fares)
      println("Completed")
    case Failure(ex) => println(ex)
  }

  scala.io.StdIn.readLine()
}
