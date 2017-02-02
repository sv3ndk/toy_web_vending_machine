import org.scalatest.{FlatSpec, Matchers}
import vending.VendingGens
import org.scalacheck.Gen._
import org.scalatest.prop.PropertyChecks
import model.Item
import vending.Stock


class StockCheck extends FlatSpec with PropertyChecks with Matchers {


  val emptyStock = Stock(Map())

  "applying one positive delta to empty stock" should
    "result in a stock equal to the deltas" in {
    forAll((VendingGens.item, "item"), (posNum[Int], "quantity")) {
      case (item: Item.Value, quantity: Int) =>

        val newStock = emptyStock.incLevels(item, quantity).get

        newStock.levels.keySet shouldEqual Set(item)
        newStock.levels(item) shouldEqual quantity

    }
  }

  "applying several positive deltas to empty stock" should
    "result in a stock equal to the deltas" in {
    forAll((VendingGens.posDeltas, "deltas")) {
      case (deltas: List[(Item.Value, Int)]) =>

        val deltasMap = deltas.toMap
        val newStock = emptyStock.incLevels(deltas).get

        newStock.levels.keySet shouldEqual Set(deltasMap.keySet)
        deltasMap.foreach{
          case (item, delta) => newStock.levels(item) == delta
        }
    }
  }

}