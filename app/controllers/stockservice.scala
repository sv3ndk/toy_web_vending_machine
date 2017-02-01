package controllers

import model.Item
import model.Web
import play.api.libs.json._
import play.api.mvc.{ Action, BodyParsers, Controller }
import play.api.libs.functional.syntax._
import vending.Stock
import play.api.Logger

import scala.util.{ Failure, Success, Try }

case class PriceResponse(item: String, price: Int)
case class ItemQuantity(item: String, quantity: Int)
case class TotalPriceResponse(items: Seq[ItemQuantity], price: Int)

class StockService extends Controller {

  import StockService._

  /**
   * provides the price of an individual element
   */
  def price(itemName: String) = Action {

    Item.item(itemName) match {
      case f: Failure[_] => BadRequest(Web.errorResponse(f.exception))

      case Success(item) =>
        val response = PriceResponse(itemName, Stock.getPrice(item))
        Ok(Json.toJson(response))
    }
  }

  /**
   * provides the total price of this set of items.
   *
   * Accepts inbound json arrays as follows:
   *
   * [
   * {"item": "Coke", "quantity": 20},
   * {"item": "Screw driver", "quantity": 10}
   * ]
   */
  def totalPrice = Action(BodyParsers.parse.json) { request =>

    request.body.validate[Seq[ItemQuantity]] match {
      case e: JsError => BadRequest(Web.errorJsonResponse(JsError.toJson(e).toString()))

      case success: JsSuccess[Seq[ItemQuantity]] =>
        val items = success.value

        parseItemsQuantities(items) match {
          case f: Failure[_] => BadRequest(Web.errorResponse(f.exception))

          case Success(parsedItems) =>
            val response = TotalPriceResponse(items, Stock.getTotalPrice(parsedItems))
            Ok(Json.toJson(response))
        }
    }
  }
}

object StockService {

  implicit val priceResponseWrite: Writes[PriceResponse] = (
    (JsPath \ "item").write[String] and
    (JsPath \ "price").write[Int]
  )(unlift(PriceResponse.unapply))

  implicit val itemQuantityRead: Reads[ItemQuantity] = (
    (JsPath \ "item").read[String] and
    (JsPath \ "quantity").read[Int](Reads.min(0))
  )(ItemQuantity.apply _)

  /**
   * parse items into enumeration
   */
  def parseItemsQuantities(items: Seq[ItemQuantity]) =
    items.map {
      itemQuantity =>
        Item.item(itemQuantity.item)
          .map((_, itemQuantity.quantity))
    }.foldLeft(Success(List.empty[(Item.Value, Int)]): Try[List[(Item.Value, Int)]]) {
      case (agg, next) => next match {
        case Success(tuple) => agg.map(tuple :: _)
        case f: Failure[_] => Failure(f.exception)
      }
    }

  implicit val itemQuantityWrite: Writes[ItemQuantity] = (
    (JsPath \ "item").write[String] and
    (JsPath \ "quantity").write[Int]
  )(unlift(ItemQuantity.unapply))

  implicit val totalPriceResponseWrite: Writes[TotalPriceResponse] = (
    (JsPath \ "items").write[Seq[ItemQuantity]] and
    (JsPath \ "price").write[Int]
  )(unlift(TotalPriceResponse.unapply))

}

