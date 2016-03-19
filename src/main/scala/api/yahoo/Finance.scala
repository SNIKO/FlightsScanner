package api.yahoo

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import api.yahoo.Model.XChangeResponse
import argonaut.DecodeJson
import utils.Utils.FutureActionResult
import java.net.URLEncoder

import argonaut.Argonaut._
import utils.Implicits._

import scala.concurrent.ExecutionContext.Implicits.global

object Finance {

  implicit val sys = ActorSystem()
  implicit val mat = ActorMaterializer()

  def getCurrencyRate(pairs: Seq[Model.CurrenciesPair]): FutureActionResult[String, XChangeResponse] = {
    val pairsList = pairs.map(p => s"'${p.from}${p.to}'").mkString(", ")
    val query = URLEncoder.encode(s"select * from yahoo.finance.xchange where pair in ($pairsList)", "UTF-8")
    val url = s"http://query.yahooapis.com/v1/public/yql?q=$query&env=store://datatables.org/alltableswithkeys&format=json"

    for {
      response <- Http().singleRequest(HttpRequest(GET, url))
      responseEntity <- Unmarshal(response.entity).to[String]
      query = responseEntity.decodeEither[XChangeResponse]
    } yield query
  }
}

object Model {

  //  {
  //    "query": {
  //      "count": 2
  //      "created": "2016-03-19T07:58:57Z"
  //      "lang": "en-GB"
  //      "results": {
  //      "rate": [2]
  //        0:  {
  //          "id": "AUDUSD"
  //          "Name": "AUD/USD"
  //          "Rate": "0.7607"
  //          "Date": "3/19/2016"
  //          "Time": "7:15am"
  //          "Ask": "0.7614"
  //          "Bid": "0.7600"
  //        }
  //        1:  {
  //          "id": "UAHUSD"
  //          "Name": "UAH/USD"
  //          "Rate": "0.0376"
  //          "Date": "3/18/2016"
  //          "Time": "7:08pm"
  //          "Ask": "0.0377"
  //          "Bid": "0.0375"
  //        }
  //      }
  //    }
  //  }


  /**
    * CurrenciesPair
    */
  case class CurrenciesPair(from: String, to: String)

  /**
    * YahooResponse
    */
  case class XChangeResponse(query: Query)

  object XChangeResponse {
    implicit def Decoder: DecodeJson[XChangeResponse] =
      DecodeJson(root => for {
        query <- (root --\ "query").read[Query]
      } yield XChangeResponse(query))
  }

  /**
    * Query
    */
  case class Query(results: Results)

  object Query {
    implicit def Decoder: DecodeJson[Query] =
      DecodeJson(query => for {
        results <- (query --\ "results").read[Results]
      } yield Query(results))
  }

  /**
    * Results
    */
  case class Results(rates: Seq[Rate])

  object Results {
    implicit def Decoder: DecodeJson[Results] =
      DecodeJson(results => for {
        rates <- (results --\ "rate").readArrayOf[Rate]
      } yield Results(rates))
  }

  /**
    * Rate
    */
  case class Rate(id: String, name: String, rate: String) {
    def from(currency: String) = name.split("/") match {
      case Array(c, _) if c == currency => true
      case _ => false
    }

    def to(currency: String) = name.split("/") match {
      case Array(_, c) if c == currency => true
      case _ => false
    }
  }

  object Rate {
    implicit def Decoder: DecodeJson[Rate] =
      DecodeJson(rate => for {
        id <- (rate --\ "id").read[String]
        name <- (rate --\ "Name").read[String]
        xrate <- (rate --\ "Rate").read[String]
      } yield Rate(id, name, xrate))
  }
}