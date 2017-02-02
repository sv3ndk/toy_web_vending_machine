package vending

import model.MoneyToken

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

/**
 * Container for a any positive integer amount of money, materialized by a set of tokens
 *
 */
trait Bank {

  def +(other: Bank): Bank
  def +(addedToken: MoneyToken): Bank
  def +(addedTokens: List[MoneyToken]): Bank

  val tokens: List[MoneyToken]
  val coins: List[MoneyToken]
  val notes: List[MoneyToken]

  val coinsValue: List[Int]
  val notesValue: List[Int]

  val total: Int

  def deposit(added: Bank, targetAmount: Int): Try[(Bank, Bank)]
}

object Bank {

  val empty: Bank = _Bank(Nil)

  // some instances of banks, each containing one money token of each kind up to a certain highest value token
  val justOne = Bank(MoneyToken.one :: Nil)
  val toTwo = justOne + MoneyToken.two
  val toFive = toTwo + MoneyToken.five
  val toTen = toFive + MoneyToken.ten
  val toTwenty = toTen + MoneyToken.twenty
  val oneOfEach = toTwenty + MoneyToken.fifty

  def apply(unsortedTokens: List[MoneyToken]): Bank = _Bank(unsortedTokens.sortBy(_.value).reverse)

  def apply(token: MoneyToken): Bank = Bank(token :: Nil)

  /**
   * Instantiate a bank with the necessary coins for the specified target total.
   * We build up the total by piling up "packs" of tokens, such that we ensure that we
   * have a large amount of small tokens (which is handy to have more chances of being able to provide change)
   */
  def apply(total: Int): Bank = {
    require(total >= 0, s"cannot gather money token for a negative total: $total")

    @tailrec
    def _buildTokens(acc: Bank, target: Int): Bank = {
      target match {
        case 0 => acc
        case i if i < toTwo.total => _buildTokens(justOne + acc, target - justOne.total)
        case i if i < toFive.total => _buildTokens(toTwo + acc, target - toTwo.total)
        case i if i < toTen.total => _buildTokens(toFive + acc, target - toFive.total)
        case i if i < toTwenty.total => _buildTokens(toTen + acc, target - toTen.total)
        case i if i < oneOfEach.total => _buildTokens(toTwenty + acc, target - toTwenty.total)
        case _ => _buildTokens(oneOfEach + acc, target - oneOfEach.total)
      }
    }

    _buildTokens(empty, total)
  }

  // keeping this class fully private makes sure it's never instanciated with unsorted tokens
  private case class _Bank(sortedTokens: List[MoneyToken]) extends Bank {

    // making sure sortedCoins is always sorted simplifies the implementation of deposit(),
    // + avoids the need to re-implement equals() and hashcode()

    val tokens = sortedTokens

    lazy val total = sortedTokens.map(_.value).sum

    lazy val (coins, notes) = sortedTokens.partition {
      case MoneyToken.Coin(_) => true
      case MoneyToken.Note(_) => false
    }

    lazy val coinsValue = coins.map(_.value)
    lazy val notesValue = notes.map(_.value)

    def +(other: Bank): Bank = Bank(tokens ++ other.tokens)
    def +(addedToken: MoneyToken): Bank = Bank(addedToken :: sortedTokens)
    def +(addedTokens: List[MoneyToken]): Bank = this + Bank(addedTokens)

    override def toString = s"[Coin, total=$total, coffer=$sortedTokens]"

    /**
     * Tries to deposit the specified target amount to this bank by providing a bunch of money tokens.
     * If the operation succeeds, the operation returns the updated Bank plus maybe some change.
     * Otherwise (e.g. there's not enough change in the bank), we simply return None
     */
    def deposit(added: Bank, targetAmount: Int): Try[(Bank, Bank)] =
      if (added.total < targetAmount)
        Failure(
          new IllegalArgumentException(
            s"value of provided coin and notes (${added.total}) is lower then target amount: $targetAmount"
          )
        )

      else {

        @tailrec
        def _deposit(toVisit: Bank, discarded: Bank, change: Bank, remainingToPay: Int): Try[(Bank, Bank)] = {

          if (remainingToPay == 0)
            Success((toVisit + discarded, change))

          else {

            val (moreDiscarded, nextCoins) = toVisit.tokens.partition(_.value > remainingToPay)

            nextCoins match {
              case Nil => Failure(
                new IllegalStateException("Currently enable to provide change for the specified deposit")
              )
              case returnedCoin :: rest => _deposit(
                toVisit = Bank(rest),
                discarded = discarded + moreDiscarded,
                change = change + returnedCoin,
                remainingToPay = remainingToPay - returnedCoin.value
              )
            }
          }

        }

        val toPayBack = added.total - targetAmount

        _deposit(this + added, Bank.empty, Bank.empty, toPayBack)

      }
  }

}
