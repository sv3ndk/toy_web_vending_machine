package vending

import model.Item
import org.scalacheck.Gen._

/**
  * Just a bunch of handy generators
  */
object VendingGens {

  val item = oneOf(Item.values.toSeq)

  val items = choose(1, 10).flatMap(n => listOfN(n, item))

  val uniqueItems = items.map(_.distinct)

  /**
    * Generator of list of item->positiveDelta,
    * each item being present only once
    * */
  val posDeltas = uniqueItems.flatMap(items =>
    listOfN(items.length, posNum[Int])
      .map(deltas => items.zip(deltas))
  )


}
