package controllers

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