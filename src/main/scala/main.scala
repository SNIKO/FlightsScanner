import java.time._

import flights.{FaresProvider, FlightDirection}
import utils.Implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scalaz.EitherT

object main extends App {

  var route = Seq(
    FlightDirection("SYD", "KBP", LocalDate.of(2016, 8, 12)),
    FlightDirection("KBP", "SYD", LocalDate.of(2016, 9, 3)))

  val providers = Seq(FaresProvider.Momondo, FaresProvider.OneTwoTrip)

  var response = providers.map(p => p.search(route)).foldLeft(Future(Seq.empty[flights.Fare])) {
    (resultFuture, providerFuture) => for {
      resultFares <- resultFuture
      providerFares <- EitherT(providerFuture).leftMap { case err => println(err) } . getOrElse(Seq.empty[flights.Fare])
    } yield resultFares ++ providerFares
  }

  response.onComplete {
    case Success(fares) =>
      val groupedFares = fares.groupBy(f => f.itineraries).map(g => flights.Fare(g._1, g._2.head.date, g._2.flatMap(_.prices))).toSeq
      println(s"${groupedFares.length} options found: ")

      groupedFares.sortBy(_.prices.map(_.price).min).foreach(f => println(f.prettyPrint))
    case Failure(ex) => println(ex)
  }

  scala.io.StdIn.readLine()
}
