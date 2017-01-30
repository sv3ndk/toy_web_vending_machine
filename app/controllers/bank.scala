package controllers

import java.util.concurrent.Executors
import javax.inject.Inject
import javax.inject.Singleton

import model.MoneyToken
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.Logger
import vending.Bank

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

@Singleton
class BankState {
  private var bank = Bank(200)

  // hackish way to make this idempotent that will beautifully OOM at some point ^^
  private var knownTxIds = Map[Int, Either[String, DepositOkReponse]]()

  def total = bank.total

  /**
   * Non thread-safe idempotent state update
   */
  def deposit(txid: Int, added: Bank, targetAmount: Int): Either[String, DepositOkReponse] = {

    if (!(knownTxIds contains txid)) {
      val result = bank.deposit(added, targetAmount) match {
        case Right((updatedBank, change)) =>
          bank = updatedBank
          Right(DepositOkReponse(
            txid = txid, message = "deposit accepted",
            change = Change(change.coinsValue, change.notesValue)
          ))

        case Left(errorMsg) => Left(errorMsg)
      }

      knownTxIds += (txid -> result)
    }

    knownTxIds(txid)
  }
}

case class BalanceResponse(balance: Int)

case class DepositRequest(txid: Int, target: Int, coins: Seq[Int], notes: Seq[Int])
case class Change(coins: List[Int], notes: List[Int])
case class DepositOkReponse(txid: Int, message: String, change: Change)

/**
 * Entry point for the bank service. Mostly only json parsing and validation
 * logic is here, the actual bank and change logic is delegated to Bank
 */
class BankService @Inject() (state: BankState) extends Controller {

  import BankService._

  /**
   * Return the current balance of the bank
   */
  def balance() = Action {
    Ok(Json.toJson(BalanceResponse(state.total)))
  }

  /**
   * Deposit can be used for any kind of payment.
   *
   * This method accepts requests formatted as follows
   *
   * {
   * "txid": 1,
   * "target_amount": 78,
   * "tokens": {
   * "coins": [1,1,1,2],
   * "notes": [5,20]
   * }
   * }
   *
   */
  def deposit() = Action.async(BodyParsers.parse.json) { request =>

    Future {
      request.body.validate[DepositRequest] match {

        case e: JsError => BadRequest(errorJsonResponse(JsError.toJson(e).toString()))

        case success: JsSuccess[DepositRequest] =>
          val req = success.get

          val maybeTokens = for {
            coins <- parseCoins(req.coins)
            notes <- parseNotes(req.notes)
          } yield Bank(coins ++ notes)

          maybeTokens match {
            case f: Failure[_] => BadRequest(errorResponse(f.exception))

            case Success(tokens) =>
              state.deposit(req.txid, tokens, req.target) match {
                case Right(response) => Ok(Json.toJson(response))
                case Left(errorMsg) => InternalServerError(errorResponse(errorMsg))
              }
          }
      }
    }(monothreadEc) // hackingly forcing sequencial execution since state update is not threadsafe ^^

  }
}

/**
 * This companion object is mostly a container for json handling methods
 */
object BankService {

  val monothreadEc = ExecutionContext.fromExecutor(
    Executors.newFixedThreadPool(1)
  )
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
      .foldLeft(Success(List.empty[MoneyToken]): Try[List[MoneyToken]]) { (maybeAgg, nextRawCoin) =>
        maybeAgg match {
          case meh: Failure[_] => meh
          case Success(okSoFar) => parse(nextRawCoin) match {
            case Success(coin) => Success(coin :: okSoFar)
            case f: Failure[MoneyToken] => Failure(f.exception)
          }
        }
      }.map(_.reverse)

  /**
   * parser of an inbound Json deposit request into the corresponding case class
   */
  implicit val bankRead: Reads[DepositRequest] = (
    (JsPath \ "txid").read[Int](Reads.min(0)) and
    (JsPath \ "target_amount").read[Int](Reads.min(0)) and
    (JsPath \ "tokens" \ "coins").read[Seq[Int]] and
    (JsPath \ "tokens" \ "notes").read[Seq[Int]]
  )(DepositRequest.apply _)

  // maybe a simpler solution was to simply use Json.format[BalanceResponse] here...
  implicit val balanceResponseWrite: Writes[BalanceResponse] =
    (JsPath \ "balance").write[Int].contramap(unlift(BalanceResponse.unapply))

  implicit val changeWrite: Writes[Change] = (
    (JsPath \ "coins").write[List[Int]] and
    (JsPath \ "notes").write[List[Int]]
  )(unlift(Change.unapply))

  implicit val depositResponseWrite: Writes[DepositOkReponse] = (
    (JsPath \ "txid").write[Int] and
    (JsPath \ "message").write[String] and
    (JsPath \ "change").write[Change]
  )(unlift(DepositOkReponse.unapply))

  def errorResponse(ex: Throwable): JsValue = errorResponse(ex.getMessage)

  def errorResponse(details: String): JsValue = errorJsonResponse(s""" "$details" """)

  /**
   * Builds a JSON error response with the provided details, which must be valid json element
   */
  def errorJsonResponse(jsonDetails: String): JsValue =
    Json.parse(
      s"""{
         |"error": "invalid deposit request",
         |"detail": $jsonDetails
         |}""".stripMargin
    )

}