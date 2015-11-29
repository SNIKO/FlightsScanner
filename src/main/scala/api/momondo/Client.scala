package api.momondo

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import api.momondo.JsonProtocol._
import argonaut.Argonaut._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Client {

  implicit val sys = ActorSystem()
  implicit val mat = ActorMaterializer()

  val customHeaders = List(headers.`User-Agent`("Dalvik/2.1.0 (Linux; U; Android 5.0.2; HTC_PN071 Build/LRX22G)"))

  def startSearch(request: SearchRequest): Future[Either[String, SearchSessionInfo]] = {
    val requestEntity = HttpEntity(ContentTypes.`application/json`, request.asJson.toString())

    for {
      response <- Http().singleRequest(HttpRequest(POST, "http://api.momondo.com/api/3.0/FlightSearch", customHeaders, requestEntity))
      responseEntity <- Unmarshal(response.entity).to[String]
      result = responseEntity.decodeEither[SearchSessionInfo].toEither
    } yield result
  }

  def pollSearchResult(searchId: String, engineId: Int): Future[Either[String, SearchResult]] = for {
    response <- Http().singleRequest(HttpRequest(GET, s"http://api.momondo.com/api/3.0/FlightSearch/$searchId/$engineId/true", customHeaders))
    entity <- Unmarshal(response.entity).to[String]
    result = entity.decodeEither[SearchResult].toEither
  } yield result
}
