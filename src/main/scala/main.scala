import java.time._

import api.yahoo.Finance._
import api.yahoo.Model._
import flights.{FaresProvider, FlightDirection}
import utils.Implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scalaz.EitherT

object main extends App {

  var route = Seq(
    FlightDirection("SYD", "KBP", LocalDate.of(2016, 12, 17)),
    FlightDirection("KBP", "SYD", LocalDate.of(2017, 1, 15)))

  val providers = Seq(FaresProvider.Momondo, FaresProvider.OneTwoTrip)

  val faresFuture = providers.map(p => p.search(route)).foldLeft(Future(Seq.empty[flights.Fare])) {
    (resultFuture, providerFuture) => for {
      resultFares <- resultFuture
      providerFares <- EitherT(providerFuture).leftMap { case err => println(err) }.getOrElse(Seq.empty[flights.Fare])
    } yield resultFares ++ providerFares
  }

  val targetCurrency = "USD"

  val faresInTargetCurrency = for {
    fares <- faresFuture
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

  faresInTargetCurrency.onComplete {
    case Success(fares) =>
      val groupedFares = fares.groupBy(f => f.itineraries).map(g => flights.Fare(g._1, g._2.head.date, g._2.flatMap(_.prices))).toSeq
      println(Report.generate(groupedFares))
    case Failure(ex) => println(ex)
  }

  scala.io.StdIn.readLine()
}
