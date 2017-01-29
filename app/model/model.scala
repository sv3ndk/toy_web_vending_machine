package model

import scala.util.{Failure, Success, Try}

sealed trait MoneyToken {
  val value: Int
}

object MoneyToken {

  case class Coin private (value: Int) extends MoneyToken
  case class Note private (value: Int) extends MoneyToken

  val one: MoneyToken   = Coin(1)
  val two: MoneyToken   = Coin(2)
  val five: MoneyToken  = Note(5)
  val ten: MoneyToken   = Note(10)
  val twenty: MoneyToken = Note(20)
  val fifty: MoneyToken = Note(50)

  val allTokens = one :: two :: five :: ten :: twenty :: fifty :: Nil


  def coin(value: Int): Try[MoneyToken] = value match {
    case i: Int if i == one.value=> Success(one)
    case i: Int if i == two.value => Success(two)

    case _ => Failure(new IllegalArgumentException(s"invalid coin value: $value"))
  }

  def note(value: Int): Try[MoneyToken] = value match {
    case i: Int if i == five.value=> Success(five)
    case i: Int if i == ten.value=> Success(ten)
    case i: Int if i == twenty.value=> Success(twenty)
    case i: Int if i == fifty.value=> Success(fifty)

    case _ => Failure(new IllegalArgumentException(s"invalid note value: $value"))
  }

}