package vending

import model.Item
import model.Item._

import scala.util.{ Failure, Success, Try }

case class Stock(levels: Map[Item.Value, Int]) {

  /**
   * adds or remove (if quantity is negative) some items to/from this stock.
   */
  def incLevels(item: Item.Value, quantity: Int): Try[Stock] = {

    val newQuantity = levels.getOrElse(item, 0) + quantity

    if (newQuantity < 0)
      Failure(
        new IllegalArgumentException(s"Refusing to add $quantity $item to stock: that would make the stock level negative")
      )
    else
      Success(this.copy(levels = levels + (item -> newQuantity)))
  }

}

object Stock {

  val prices = Map[Item.Value, Int](
    coke -> 2,
    water -> 1,
    orangeJuice -> 2,
    smoothie -> 3,

    chocolate -> 3,
    cerealBar -> 2,
    apple -> 1,

    screwDriver -> 4,
    usbCharger -> 8,
    battery -> 11
  )

  def getPrice(item: Item.Value): Int = prices(item)

  def getTotalPrice(items: Seq[(Item.Value, Int)]): Int =
    items.map {
      case (item, quantity) => getPrice(item) * quantity
    }.sum

}
