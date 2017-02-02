package controllers

import java.util.concurrent.Executors
import javax.inject.Inject

import com.google.inject.Singleton
import model.Item
import model.Web
import play.api.libs.json._
import play.api.mvc.{ Action, BodyParsers, Controller }
import play.api.libs.functional.syntax._
import vending.Stock
import play.api.Logger

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

case class PriceResponse(item: String, price: Int)
case class ItemQuantity(item: String, quantity: Int)
case class TotalPriceResponse(items: Seq[ItemQuantity], price: Int)
case class StockLevels(items: Seq[ItemQuantity])

case class UpdateStockRequest(txid: Int, deltas: Seq[ItemQuantity])

/**
 * Non thread-safe state of the stock service.
 */
@Singleton
class StockServiceState {
  var stock = new Stock(levels = Item.values.map(_ -> 100).toMap)

  /**
   * Tries to apply all the delta provided in the list. If all successful,
   * we replace the stock with the new one
   */
  private def _incLevels(update: List[(Item.Value, Int)], txId: Int): Try[Unit] = {
    stock.incLevels(update) match {
      case Success(updatedStock) =>
        stock = updatedStock
        Success(Unit)

      case f: Failure[_] => Failure[Unit](f.exception)
    }
  }

  // we only save the request after they're parsed successfully
  val incLevels = Idempotent(_incLevels _)

}

class StockService @Inject() (state: StockServiceState) extends Controller {

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

  /**
   * return the current stock levels
   */
  def currentStock = Action {

    val levels = StockLevels(
      state.stock.levels.toSeq.map {
        case (it, qt) => ItemQuantity(it.toString, qt)
      }
    )
    Ok(Json.toJson(levels))
  }

  /**
   * Update the stocks by adding the specified quantities to each item
   * (quantities may be negative)
   */
  def updateStock = Action.async(BodyParsers.parse.json) { request =>

    Future {

      request.body.validate[UpdateStockRequest] match {
        case e: JsError => BadRequest(Web.errorJsonResponse(JsError.toJson(e).toString()))

        case success: JsSuccess[UpdateStockRequest] =>
          val req = success.value

          parseItemsQuantities(req.deltas) match {
            case f: Failure[_] => BadRequest(Web.errorResponse(f.exception))

            case Success(parsedItems) =>
              state.incLevels(req.txid, parsedItems) match {
                case Success(_) => Ok
                case f: Failure[_] => InternalServerError(Web.errorResponse(f.exception))
              }
          }
      }

    }(monothreadEc)
  }

}

object StockService {

  val monothreadEc = ExecutionContext.fromExecutor(
    Executors.newFixedThreadPool(1)
  )

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
    }.foldLeft(Try(List.empty[(Item.Value, Int)])) {
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

  implicit val stockLevelsWrite: Writes[StockLevels] =
    (JsPath \ "items").write[Seq[ItemQuantity]]
      .contramap(unlift(StockLevels.unapply))

  implicit val updateStockRequestRead: Reads[UpdateStockRequest] = (
    (JsPath \ "txid").read[Int] and
    (JsPath \ "deltas").read[Seq[ItemQuantity]]
  )(UpdateStockRequest.apply _)

}

