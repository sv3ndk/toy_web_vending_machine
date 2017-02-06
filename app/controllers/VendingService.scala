package controllers

import javax.inject.Inject

import model.Web
import model._
import model.Web._
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.functional.syntax._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import vending.Bank
import play.api.Logger

import scala.concurrent.Future
import scala.util.{ Failure, Success }

case class PurchaseRequest(txid: Int, items: Seq[ItemQuantity], coins: Seq[Int], notes: Seq[Int])

case class InvalidJsonRequestException(msg: String, jsonError: JsError) extends Exception(msg)

case class InvalidRequestException(msg: String, source: Throwable) extends Exception(msg, source)

case class PurchaseResponse(txid: Int, price: Int, change: Change)

/**
 * Exception while calling an external WS
 */
case class WsCallException(error: JsValue, code: Option[Int]) extends Exception(error.toString())

object WsCallException {

  def apply(message: String, code: Option[Int] = None): WsCallException =
    WsCallException(Json.toJson(message), code)
}

/**
 * This is the "public" front-end service of this app: exposing the possibility
 * of purchasing Items by taking them from the stock, putting the money into the
 * bank and getting some change back.
 */
class VendingService @Inject() (ws: WSClient) extends Controller {

  import VendingService._

  def purchase = Action.async(BodyParsers.parse.json) { request =>

    implicit val iRequest: Request[JsValue] = request

    val response = for {

      request <- Future {
        parsePurchaseRequest(request)
      }

      price <- totalPrice(request.items)

      (paidTokens, demandedItemQantities) = parseRequestContent(request)

      _ <- updateStocks(request.txid, request.items, added = false)

      change <- makePayment(request.txid, paidTokens, price).recoverWith {
        // in case the payment fails, tries to put back the items before propagating the failure
        case throwable =>

          Logger.warn(s"error whiled trying to deposit money, trying to roll back the stock update", throwable)

          // Now if this one fails or if we crash in the middle, the state becomes inconsistent, that's not good ^^
          // Also, ugly trick: using -txid to have a non-conflicting txid that is retry-able. Don't do this at home...
          updateStocks(-request.txid, request.items, added = true)
          throw throwable
      }

    } yield Ok(Json.toJson(PurchaseResponse(request.txid, price, change)))

    response recover {
      case InvalidJsonRequestException(msg, jsError) =>
        BadRequest(Web.jsonErrorResponse(msg, jsError))

      case InvalidRequestException(msg, th) =>
        BadRequest(Web.errorResponse(msg, th))

      case WsCallException(js, Some(code)) =>
        new Status(code)(Web.jsonErrorResponse("Error while calling external service", js))

      case WsCallException(js, None) =>
        InternalServerError(Web.jsonErrorResponse("Error while calling external service", js))
    }
  }

  private def parsePurchaseRequest(request: Request[JsValue]): PurchaseRequest =
    request.body.validate[PurchaseRequest] match {
      case jsError: JsError =>
        throw InvalidJsonRequestException("invalid purchase json request", jsError)

      case success: JsSuccess[PurchaseRequest] => success.get
    }

  private def parseRequestContent(purchaseRequest: PurchaseRequest): (Bank, List[(Item.Value, Int)]) = {

    val parsed = for {
      bank <- Web.parseTokens(purchaseRequest.coins, purchaseRequest.notes)
      itemQuantities <- Web.parseItemsQuantities(purchaseRequest.items)
    } yield (bank, itemQuantities)

    parsed match {
      case f: Failure[_] =>
        throw InvalidRequestException("invalid purchase json request", f.exception)

      case Success((bank, itemQuantities)) => (bank, itemQuantities)
    }
  }

  /**
   * query the Stock Service to get the total price of the ordered items
   */
  private def totalPrice(items: Seq[ItemQuantity])(implicit request: Request[JsValue]) =
    ws.url(routes.StockService.totalPrice().absoluteURL)
      .post(Json.toJson(items))
      .map(WsUtils.processWsJsonResponse[TotalPriceResponse, Int](response => response.price))

  /**
   * adds or remove those items to the stock
   */
  private def updateStocks(txid: Int, items: Seq[ItemQuantity], added: Boolean)(implicit request: Request[JsValue]) = {

    val itemUpdates =
      if (added) items
      else items.map { case ItemQuantity(it, qty) => ItemQuantity(it, -qty) }

    ws.url(routes.StockService.updateStock().absoluteURL)
      .put(Json.toJson(UpdateStockRequest(txid, itemUpdates)))
      .map { WsUtils.processWsEmptyResponse }
  }

  /**
   * deposit the payment in the bank
   */
  private def makePayment(txid: Int, paidTokens: Bank, price: Int)(implicit request: Request[JsValue]) =
    ws.url(routes.BankService.deposit().absoluteURL)
      .put(Json.toJson(DepositRequest.build(txid, price, paidTokens)))
      .map(WsUtils.processWsJsonResponse[DepositOkResponse, Change](response => response.change))

}

object VendingService {

  implicit val purchaseRequestRead: Reads[PurchaseRequest] = (
    (JsPath \ "txid").read[Int] and
    (JsPath \ "items").read[Seq[ItemQuantity]] and
    (JsPath \ "payment" \ "coins").read[Seq[Int]] and
    (JsPath \ "payment" \ "notes").read[Seq[Int]]
  )(PurchaseRequest.apply _)

  implicit val purchaseResponseWrite: Writes[PurchaseResponse] = (
    (JsPath \ "txid").write[Int] and
    (JsPath \ "price").write[Int] and
    (JsPath \ "change").write[Change]
  )(unlift(PurchaseResponse.unapply))

}
