package model

import MoneyToken._
import org.scalatest.{ FlatSpec, Matchers }

import scala.util.{ Failure, Success }

class TestWebChecks extends FlatSpec with Matchers {

  "parsing valid set of coin" should "produce a successful list of money token" in {
    val parsed = Web.parseCoins(1 :: 1 :: 1 :: 2 :: 2 :: 1 :: Nil)
    parsed shouldEqual Success(one :: one :: one :: two :: two :: one :: Nil)
  }

  "parsing an empty set of coin" should "produce a successful emtpy list of money token" in {
    val parsed = Web.parseCoins(List.empty[Int])
    parsed shouldEqual Success(Nil)
  }

  "parsing invalid set of coins" should "produce a Failure" in {
    val parsed = Web.parseCoins(1 :: 18 :: 1 :: 2 :: 2 :: 1 :: Nil)
    parsed should matchPattern { case _: Failure[List[MoneyToken]] => }
  }

  "parsing valid set of notes" should "produce a successful list of money token" in {
    val parsed = Web.parseNotes(5 :: 10 :: 5 :: 20 :: 50 :: 10 :: Nil)
    parsed shouldEqual (Success(five :: ten :: five :: twenty :: fifty :: ten :: Nil))
  }

  "parsing an empty set of notes" should "produce a successful emtpy list of money token" in {
    val parsed = Web.parseNotes(List.empty[Int])
    parsed shouldEqual (Success(Nil))
  }

  "parsing invalid set of notes" should "produce a Failure" in {
    val parsed = Web.parseNotes(1 :: 18 :: 1 :: 2 :: 2 :: 1 :: Nil)
    parsed should matchPattern { case _: Failure[List[MoneyToken]] => }
  }

}