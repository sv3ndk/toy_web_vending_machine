package controllers

import model.Web.TotalPriceResponse
import play.api.libs.json.{ JsError, JsSuccess, Reads }
import play.api.libs.ws.WSResponse

import scala.util.{ Failure, Success, Try }

/**
 * Ugly hack to make state updating function idempotent:
 *
 * Simply keeps in memory the info where that request has already been received
 * and if so, whether it was successful and its response.
 *
 * If the request is unknown or failed before, we try to apply here and return
 * the result.
 *
 * If it succeeded before, we simply return the previously known return value.
 *
 * This will of course OOM at some point
 *
 */
case class Idempotent[Req, Res](f: (Req, Int) => Try[Res]) {

  var memory = Map[Int, Try[Res]]()

  private def update(txId: Int, req: Req) = {
    memory += txId -> f(req, txId)
    memory(txId)
  }

  // This is not-threadsafe => assuming it's properly wrapped upstream
  def apply(txId: Int, req: Req): Try[Res] = {

    memory.get(txId) match {

      case Some(prev) =>
        prev match {
          case res: Success[Res] => res

          // we could imagine only retrying a certain amount of times
          // and/or retrying only retryable errors...
          case _: Failure[Res] => update(txId, req)
        }

      case None => update(txId, req)
    }
  }

}

object Idempotent {

  // helper constructor to handle functions of 2 arguments (+ the txid)
  def apply[Req1, Req2, Res](f: (Req1, Req2, Int) => Try[Res]): Idempotent[(Req1, Req2), Res] =
    Idempotent[(Req1, Req2), Res] {
      case ((r1, r2), txid) => f(r1, r2, txid)
    }

}

object WsUtils {

  /**
   * Parses the JSON body of a WS call out of this WSResponse, extract some value thanks to the extract method
   * and return the result.
   *
   * In case of error, bubbles up an exception aligned with the error handling mechanism upstream
   */
  def processWsJsonResponse[Resp, Out](extract: Resp => Out)(response: WSResponse)(implicit rds: Reads[Resp]): Out =
    if (response.status == 200)
      response.json.validate[Resp] match {
        case resp: JsSuccess[Resp] => extract(resp.get)
        case _: JsError =>
          // TODO: we should populate the jserror in the exception here...
          throw WsCallException("invalid response received from stock service")
      }
    else
      throwWsCallException(response)

  /**
   * Similar to processWsJsonResponse for the case where the successful 200 response is not expected to contain a
   * body (or we just do not care about it...^^)
   */
  def processWsEmptyResponse[Resp, Out](response: WSResponse): Unit =
    if (response.status != 200)
      throwWsCallException(response)

  private def throwWsCallException(response: WSResponse): Nothing = {
    // tries to parse the json body of the received error, which could itself fail...
    val ex = Try {
      WsCallException(response.json, Some(response.status))
    }.getOrElse {
      WsCallException(response.body, Some(response.status))
    }

    throw ex
  }

}
