package vending

import controllers.BankService
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import model.MoneyToken
import MoneyToken._

import scala.util.{Failure, Success}

class TestBank extends FlatSpec with Matchers {

  "empty Bank" should "be the same thing as a Bank containing nothing" in {
    val e = Bank(0)
    val e2 = Bank(Nil)
    e should be (Bank.empty)
    e2 should be (Bank.empty)


    e.total should be (0)
    e2.total should be (0)
    Bank.empty.total should be (0)
  }

  "pack of one of each" should "contain exactly one of each token" in {

    // just the coin of 1EUR
    Bank.justOne should be (Bank(total=1))

    // coins of 1EUR and 2EUR
    Bank.toTwo should be (Bank(total=3))

    // coins of 1EUR, 2EUR and 5 EUR
    Bank.toFive should be (Bank(total=8))

    // coins of 1EUR, 2EUR, 5EUR and 10 EUR
    Bank.toTen should be (Bank(total=18))

    // coins of 1EUR, 2EUR, 5EUR, 10 EUR and 20 EUR
    Bank.toTwenty should be (Bank(total=38))

    // coins of 1EUR, 2EUR, 5EUR, 10 EUR, 20 EUR and 50 EUR
    Bank.oneOfEach should be (Bank(total=88))

  }

  " adding exact amount" should "yield the bank with just the tokens added and no change" in  {

    val orig = Bank(MoneyToken.five :: MoneyToken.five :: MoneyToken.one :: Nil)

    orig.deposit(Bank(2), 2) should be (Right(
      Bank(MoneyToken.five :: MoneyToken.five :: MoneyToken.one :: MoneyToken.one :: MoneyToken.one ::Nil),
      Bank.empty
    ))

  }

  " adding zero with a coin of one to an empty" should "yield leave the bank untouched and yield a change of one" in  {

    val emptyBank = Bank(0)

    emptyBank.deposit(Bank(1), 0) should be (Right(
      // updated bank should still be empty
      emptyBank,

      // returned change should be 1
      Bank(1)
    ))
  }

  " adding zero with a coin of one to a small bank" should "leave the bank untouched and yield a change of one" in  {

    val smallBank = Bank(1)

    smallBank.deposit(Bank(1), 0) should be (Right(
      // updated bank should still be empty
      smallBank,

      // returned change should be 1
      Bank(1)
    ))
  }

  "adding 8 dollars" should "update the bank and produce some change" in  {

    val smallBank = Bank(five :: five :: one :: two :: Nil)
    val deposit = Bank (ten)
    val targetAmount = 8

    smallBank.deposit(deposit, targetAmount) should be (Right(
      // ten note should have been added, two coing should have been removed
      Bank(ten :: five :: five :: one ::  Nil),

      // returned change should be 1
      Bank(two)
    ))
  }

  " adding 8 dollars" should "fail due to lack of change" in  {



    // the bank cannot change this at the moment: we do not have enough coin!
    val smallBank = Bank(five :: five :: one :: Nil)
    val deposit = Bank (ten)
    val targetAmount = 8

    smallBank.deposit(deposit, targetAmount) shouldEqual Left("Currently enable to provide change for the specified deposit")
  }


  "parsing valid set of coin" should "produce a successful list of money token" in  {
    val parsed = BankService.parseCoins(1 :: 1 :: 1:: 2 :: 2 :: 1 :: Nil)
    parsed shouldEqual Success(one :: one :: one :: two :: two :: one :: Nil)
  }

  "parsing an empty set of coin" should "produce a successful emtpy list of money token" in  {
    val parsed = BankService.parseCoins( List.empty[Int])
    parsed shouldEqual Success(Nil)
  }

  "parsing invalid set of coins" should "produce a Failure" in  {
    val parsed = BankService.parseCoins(1 :: 18 :: 1:: 2 :: 2 :: 1 :: Nil)
    parsed should matchPattern { case _: Failure[List[MoneyToken]] => }
  }

  "parsing valid set of notes" should "produce a successful list of money token" in  {
    val parsed = BankService.parseNotes(5 :: 10 :: 5:: 20 :: 50 :: 10 :: Nil)
    parsed shouldEqual(Success(five :: ten :: five :: twenty :: fifty :: ten :: Nil))
  }

  "parsing an empty set of notes" should "produce a successful emtpy list of money token" in  {
    val parsed = BankService.parseNotes( List.empty[Int])
    parsed shouldEqual(Success(Nil))
  }

  "parsing invalid set of notes" should "produce a Failure" in  {
    val parsed = BankService.parseNotes(1 :: 18 :: 1:: 2 :: 2 :: 1 :: Nil)
    parsed should matchPattern { case _: Failure[List[MoneyToken]] => }
  }

}


class TestBankChecks extends FlatSpec with PropertyChecks with Matchers {

  def amount = Gen.posNum[Int]

  "building a negative amount of money" should "be refused" in {
    forAll((Gen.negNum[Int], "total")) { (n: Int) =>
      a[IllegalArgumentException] should be thrownBy Bank(n)
    }
  }

  "building a bank with positive total" should "result in a bank with correct total" in {
    forAll((amount, "total")) {
      // keeping the whenever despite the positive integers generator since the erroneous case
      // can be shrunk to a negative one in case of bugs
      (n: Int) =>
        whenever(n >= 0) {
          withClue(s"total of ${Bank(n)} should be $n") {
            Bank(n).total shouldEqual n


          }
        }
    }
  }

  "adding the exact change to a bank" should "produce a bank with more money and no change" in {
    forAll((amount, "startAmount"), (amount, "addedAmount")) {
      (startAmount: Int, added: Int) =>
        whenever(startAmount >= 0 && added >= 0) {

          // cannot be None => forcing get
          val (updatedBank, change) = Bank(startAmount).deposit(Bank(added), added).right.get

          change should be(Bank.empty)
          updatedBank.total shouldEqual startAmount + added
        }
    }
  }


  "depositing zero dollars" should "leave the bank untouched and yield some change" in {
    forAll((amount, "bankCash"), (amount, "change")) {
      (bankCash: Int, change: Int) => {

        val (updatedBank, obtainedChange) = Bank(bankCash).deposit(Bank(change), targetAmount = 0).right.get

        withClue("bank should be exactly as before") {updatedBank should be (updatedBank)}
        withClue("all provided money should be returned") {obtainedChange.total should be(change)}

      }
    }
  }

  "depositing some amount to a bank having the necessary change" should
    "produce a bank with more money + some change back" in {
    forAll((amount, "bankCash"), (amount, "deposit"), (amount, "change")) {
      (bankCash: Int, deposit: Int, change: Int) => {

        // build the bank this way makes sure it contains the coins to build up the returned change
        val bank = Bank(bankCash) + Bank(change)

        // we deposit too much (deposit + change) and expect change in return
        val (updatedBank, obtainedChange) = bank.deposit(Bank(deposit+change), targetAmount=deposit).right.get

        withClue("change should be the full provided amount") {obtainedChange.total should be (change)}
        withClue("bank should have been updated") {updatedBank.total shouldEqual bankCash + change + deposit}
      }
    }
  }





}


