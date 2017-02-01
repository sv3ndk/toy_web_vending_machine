package model

import scala.util.{ Failure, Success, Try }

sealed trait MoneyToken {
  val value: Int
}

object MoneyToken {

  case class Coin private (value: Int) extends MoneyToken
  case class Note private (value: Int) extends MoneyToken

  val one: MoneyToken = Coin(1)
  val two: MoneyToken = Coin(2)
  val five: MoneyToken = Note(5)
  val ten: MoneyToken = Note(10)
  val twenty: MoneyToken = Note(20)
  val fifty: MoneyToken = Note(50)

  val allTokens = one :: two :: five :: ten :: twenty :: fifty :: Nil

  def coin(value: Int): Try[MoneyToken] = value match {
    case i: Int if i == one.value => Success(one)
    case i: Int if i == two.value => Success(two)

    case _ => Failure(new IllegalArgumentException(s"invalid coin value: $value"))
  }

  def note(value: Int): Try[MoneyToken] = value match {
    case i: Int if i == five.value => Success(five)
    case i: Int if i == ten.value => Success(ten)
    case i: Int if i == twenty.value => Success(twenty)
    case i: Int if i == fifty.value => Success(fifty)

    case _ => Failure(new IllegalArgumentException(s"invalid note value: $value"))
  }

}

object Item extends Enumeration {

  val coke = Value("Coke")
  val water = Value("Mineral water")
  val orangeJuice = Value("Orange juice")
  val smoothie = Value("Orange juice")

  val chocolate = Value("Chocolate")
  val cerealBar = Value("Cereal bar")
  val apple = Value("Fresh apple")

  val screwDriver = Value("Screw driver")
  val usbCharger = Value("USB charger")
  val battery = Value("Triple A battery pack")

  // this is a copy-past of withValue, I just want to have a custom error message
  def item(name: String) = Try(values.find(_.toString == name).getOrElse(
    throw new NoSuchElementException(s"Unknown item: '$name'")
  ))

}
