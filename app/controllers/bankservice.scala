package controllers

import java.util.concurrent.Executors
import javax.inject.Inject
import javax.inject.Singleton
import model.Web

import model.Web._
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

  private def _deposit(added: Bank, targetAmount: Int, txid: Int): Try[DepositOkResponse] =
    bank.deposit(added, targetAmount) match {
      case Success((updatedBank, change)) =>
        bank = updatedBank
        Success(DepositOkResponse(
          txid = txid,
          message = "deposit accepted",
          change = Change(change.coinsValue, change.notesValue)
        ))

      case f: Failure[_] => Failure[DepositOkResponse](f.exception)
    }

  var deposit = Idempotent(_deposit _)

}

case class BalanceResponse(balance: Int)

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

        case e: JsError => BadRequest(Web.jsonErrorResponse("invalid deposit json request", e))

        case success: JsSuccess[DepositRequest] =>
          val req = success.get

          Web.parseTokens(req.coins, req.notes) match {
            case f: Failure[_] => BadRequest(Web.errorResponse("invalid tokens in deposit json request", f.exception))

            case Success(tokens) =>
              state.deposit(req.txid, (tokens, req.targetAmount)) match {
                case Success(response) => Ok(Json.toJson(response))
                case f: Failure[DepositOkResponse] =>
                  InternalServerError(Web.errorResponse("error while applying deposit", f.exception))
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

  // maybe a simpler solution was to simply use Json.format[BalanceResponse] here...
  implicit val balanceResponseWrite: Writes[BalanceResponse] =
    (JsPath \ "balance").write[Int].contramap(unlift(BalanceResponse.unapply))

}
