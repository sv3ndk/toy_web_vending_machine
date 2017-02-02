package model

import play.api.libs.functional.syntax._
import play.api.libs.json._
import vending.Bank

import scala.util.{ Failure, Success, Try }

/**
 * Data models shared among the web services
 */
object Web {

  case class ItemQuantity(item: String, quantity: Int)

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

  def parseTokens(coins: Seq[Int], notes: Seq[Int]): Try[Bank] =
    for {
      coins <- Web.parseCoins(coins)
      notes <- Web.parseNotes(notes)
    } yield Bank(coins ++ notes)

  /**
   * Attempts to parse this sequence of coin values into the corresponding sequence of MoneyTokens
   */
  def parseCoins = parseTokens(MoneyToken.coin) _

  /**
   * Attempts to parse this sequence of note values into the corresponding sequence of MoneyTokens
   */
  def parseNotes = parseTokens(MoneyToken.note) _

  def parseTokens[T <: MoneyToken](parse: Int => Try[T])(coinValues: Seq[Int]) =
    coinValues
      .foldLeft(Try(List.empty[MoneyToken])) { (maybeAgg, nextRawCoin) =>
        maybeAgg match {
          case meh: Failure[_] => meh
          case Success(okSoFar) => parse(nextRawCoin) match {
            case Success(coin) => Success(coin :: okSoFar)
            case f: Failure[MoneyToken] => Failure(f.exception)
          }
        }
      }.map(_.reverse)

  def errorResponse(ex: Throwable): JsValue = errorResponse(ex.getMessage)

  def errorResponse(details: String): JsValue = errorJsonResponse(s""" "$details" """)

  /**
   * Builds a JSON error response with the provided details, which must be valid json element
   */
  def errorJsonResponse(jsonDetails: String): JsValue =
    Json.parse(s"""{"error": $jsonDetails}""")

}