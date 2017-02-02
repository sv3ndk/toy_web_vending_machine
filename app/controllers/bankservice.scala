package controllers

import java.util.concurrent.Executors
import javax.inject.Inject
import javax.inject.Singleton
import model.Web

import model.MoneyToken
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.Logger
import vending.Bank

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

@Singleton
class BankServiceState {
  private var bank = Bank(200)

  def total = bank.total

  private def _deposit(added: Bank, targetAmount: Int, txid: Int): Try[DepositOkReponse] =
    bank.deposit(added, targetAmount) match {
      case Success((updatedBank, change)) =>
        bank = updatedBank
        Success(DepositOkReponse(
          txid = txid,
          message = "deposit accepted",
          change = Change(change.coinsValue, change.notesValue)
        ))

      case f: Failure[_] => Failure[DepositOkReponse](f.exception)
    }

  var deposit = Idempotent(_deposit _)

}

case class BalanceResponse(balance: Int)

case class DepositRequest(txid: Int, target: Int, coins: Seq[Int], notes: Seq[Int])
case class Change(coins: List[Int], notes: List[Int])
case class DepositOkReponse(txid: Int, message: String, change: Change)

/**
 * Entry point for the bank service. Mostly only json parsing and validation
 * logic is here, the actual bank and change logic is delegated to Bank
 */
class BankService @Inject() (state: BankServiceState) extends Controller {

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

        case e: JsError => BadRequest(Web.errorJsonResponse(JsError.toJson(e).toString()))

        case success: JsSuccess[DepositRequest] =>
          val req = success.get

          Web.parseTokens(req.coins, req.notes) match {
            case f: Failure[_] => BadRequest(Web.errorResponse(f.exception))

            case Success(tokens) =>
              state.deposit(req.txid, (tokens, req.target)) match {
                case Success(response) => Ok(Json.toJson(response))
                case f: Failure[DepositOkReponse] => InternalServerError(Web.errorResponse(f.exception))
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

}
