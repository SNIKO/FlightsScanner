package flights

import java.time.LocalTime

import utils.Utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success
import scalaz.EitherT

object Scanner {

  val providers = Seq(FaresProvider.Momondo, FaresProvider.OneTwoTrip)

  def search(trips: Seq[Trip]): Future[Seq[Fare]] = {
    trips.foldLeft(Future(Seq.empty[Fare])) {
      (resultFuture, route) => for {
        result <- resultFuture
        routeFares <- search(route)
      } yield result ++ routeFares
    } map merge
  }

  def search(trip: Trip): Future[Seq[Fare]] = {
    println(s"Searching flights for route '$trip'...")

    val providersResults = providers.map(p => {
      EitherT(p.search(trip)).run.map {
        case ActionFailure(err) =>
          println(s"${LocalTime.now()} Failed to load rates from a provider: $err")
          Seq.empty[Fare]
        case ActionSuccess(fares) => fares
      } recover { case err =>
        println(s"${LocalTime.now()} Failed to load rates from a provider: $err")
        Seq.empty[Fare]
      }
    })

    Future.sequence(providersResults)
      .map(_.flatten)
      .andThen {
        case Success(res) => println(s"${res.length} flight options have been found for route '$trip'")
      }
  }

  def merge(fares: Seq[Fare]) = {
    fares
      .groupBy(f => f.itineraries)
      .map { case (itineraries, frs) => flights.Fare(itineraries, frs.head.date, frs.flatMap(_.prices)) }
      .toSeq
  }
}
