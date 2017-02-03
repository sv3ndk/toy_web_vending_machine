package controllers

import javax.inject.Inject

import model.Web
import model.Web._
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.functional.syntax._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import vending.Bank

import scala.concurrent.Future
import scala.util.{ Failure, Success }

case class PurchaseRequest(txId: Int, items: Seq[ItemQuantity], coins: Seq[Int], notes: Seq[Int])

case class WsCallException(error: JsValue, code: Option[Int] = None) extends Exception

object WsCallException {

  def apply(message: String): WsCallException = WsCallException(Json.toJson(message))

}

/**
 * This is the "public" front-end service of this app: exposing the possibility
 * of purchasing Items by taking them from the stock, putting the money into the
 * bank and getting some change back.
 */
class VendingService @Inject() (ws: WSClient) extends Controller {

  import VendingService._

  def purchase = Action.async(BodyParsers.parse.json) { implicit request =>

    request.body.validate[PurchaseRequest] match {
      case e: JsError => Future { BadRequest(Web.jsonErrorResponse("invalid purchase json request", e)) }

      case success: JsSuccess[PurchaseRequest] =>

        val req = success.get

        val parsed = for {
          bank <- Web.parseTokens(req.coins, req.notes)
          itemQuantities <- Web.parseItemsQuantities(req.items)
        } yield (bank, itemQuantities)

        parsed match {

          case f: Failure[_] => Future {
            BadRequest(Web.errorResponse("invalid purchase json request", f.exception))
          }

          case Success((bank, itemQuantities)) =>

            val result = for {
              price <- totalPrice(req.items)
              _ <- depositMoney(bank, price)
              response = "all good"
            } yield response

            formatResponse(result)

        }
    }
  }

  private def totalPrice(items: Seq[ItemQuantity])(implicit request: RequestHeader) =

    ws
      .url(routes.StockService.totalPrice().absoluteURL)

      // this works because of the implicit itemQuantityWrite
      .post(Json.toJson(items))
      .map {
        response =>
          if (response.status == 200)
            response.json.validate[TotalPriceResponse] match {
              case resp: JsSuccess[TotalPriceResponse] => resp.get.price
              case _: JsError =>
                throw WsCallException("invalid response received from stock service")
            }
          else
            throw WsCallException(response.json, Some(response.status))
      }

  private def depositMoney(tokens: Bank, price: Int)(implicit request: RequestHeader) = {
    if (tokens.total < price)
      Future.failed(
        new IllegalArgumentException(s"total value of provided coins is ${tokens.total}, which is below the total price of the requested items: $price")
      )
    else
      Future.successful(Unit) // TODO
  }

  /**
   * reponse and error handling
   */
  private def formatResponse(result: Future[String]): Future[Result] =

    result.map(message => Ok(message)) recover {

      // this typically happen if an error happened in some  downstream service
      // => we mostly just propagate it up, with its original HTTP code
      case WsCallException(err, Some(code)) =>
        new Status(code)(Web.jsonErrorResponse("invalid purchase json request", err))

      case e: IllegalArgumentException =>
        BadRequest(Web.errorResponse("input error during purchasing execution", e))

      case e: Exception =>
        InternalServerError(Web.errorResponse("unknown error during purchasing execution", e))
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
