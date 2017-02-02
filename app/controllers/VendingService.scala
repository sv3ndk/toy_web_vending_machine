package controllers

import javax.inject.Inject

import model.Web
import model.Web.ItemQuantity
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.functional.syntax._
import play.api.mvc.{ Action, BodyParsers, Controller }
import play.api.libs.concurrent.Execution.Implicits._

import scala.concurrent.Future
import scala.util.{ Failure, Success }

case class PurchaseRequest(txId: Int, items: Seq[ItemQuantity], coins: Seq[Int], notes: Seq[Int])

/**
 * This is the "public" front-end service of this app: exposing the possibility
 * of purchasing Items by taking them from the stock, putting the money into the
 * bank and getting some change back.
 */
class VendingService @Inject() (ws: WSClient) extends Controller {

  import VendingService._

  def purchase = Action.async(BodyParsers.parse.json) { request =>

    request.body.validate[PurchaseRequest] match {
      case e: JsError =>
        Future {
          BadRequest(Web.errorJsonResponse(JsError.toJson(e).toString()))
        }

      case success: JsSuccess[PurchaseRequest] =>

        val req = success.get

        val parsed = for {
          bank <- Web.parseTokens(req.coins, req.notes)
          itemQuantities <- Web.parseItemsQuantities(req.items)
        } yield (bank, itemQuantities)

        parsed match {

          case f: Failure[_] => Future {
            BadRequest(Web.errorResponse(f.exception))
          }

          case Success((bank, itemQuantities)) =>

            ws.url(routes.StockService.totalPrice().url)
              .withBody(req.items)
              .post()
              .map(Ok(_))
        }

    }

  }

}

object VendingService {

  implicit val purchaseRequestRead: Reads[PurchaseRequest] = (
    (JsPath \ "txid").read[Int] and
    (JsPath \ "items").read[Seq[ItemQuantity]] and
    (JsPath \ "payment" \ "coins").read[Seq[Int]] and
    (JsPath \ "payment" \ "notes").read[Seq[Int]]
  )(PurchaseRequest.apply _)

}
