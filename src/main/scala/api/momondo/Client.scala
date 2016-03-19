package api.momondo

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import api.momondo.JsonProtocol._
import argonaut.Argonaut._
import utils.Utils.FutureActionResult

import scala.concurrent.ExecutionContext.Implicits.global

object Client {

  implicit val sys = ActorSystem()
  implicit val mat = ActorMaterializer()

  val customHeaders = List(headers.`User-Agent`("Dalvik/2.1.0 (Linux; U; Android 5.0.2; HTC_PN071 Build/LRX22G)"))

  def startSearch(request: SearchRequest): FutureActionResult[String, SearchSessionInfo] = {
    val requestEntity = HttpEntity(ContentTypes.`application/json`, request.asJson.toString())
    val req = HttpRequest(POST, "http://api.momondo.com/api/3.0/FlightSearch", customHeaders, requestEntity)

    for {
      response <- Http().singleRequest(req)
      responseEntity <- Unmarshal(response.entity).to[String]
      result = responseEntity.decodeEither[SearchSessionInfo]
    } yield result
  }

  def pollSearchResult(searchId: String, engineId: Int): FutureActionResult[String, SearchResult] = {
    val request = HttpRequest(GET, s"http://api.momondo.com/api/3.0/FlightSearch/$searchId/$engineId/true", customHeaders)

    for {
      response <- Http().singleRequest(request)
      entity <- Unmarshal(response.entity).to[String]
      result = entity.decodeEither[SearchResult]
    } yield result
  }
}
