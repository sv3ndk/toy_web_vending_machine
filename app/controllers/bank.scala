package controllers

import java.util.concurrent.Executors
import javax.inject.Inject
import javax.inject.Singleton

import model.MoneyToken
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.Logger
import play.api.libs.concurrent.Akka
import vending.Bank

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

@Singleton
class BankState {
  var bank = Bank(200)

  def deposit(added: Bank, targetAmount: Int): Either[String, Bank] = {

    // not threadsafe,
    bank.deposit(added, targetAmount) match {
      case Right((updatedBank, change)) =>
        bank = updatedBank
        Right(change)

      case Left(errorMsg) => Left(errorMsg)
    }

  }
}

case class DepositRequest(txId: Int, target: Int, coins: Seq[Int], notes: Seq[Int])

class BankService @Inject() (state: BankState) extends Controller {

  import BankService._

  /**
   * Return the current balance of the bank
   */
  def balance() = Action {
    Ok(Json.parse(s"""{"balance": ${state.bank.total} }"""))
  }

  /**
   * Deposit can be used for any kind of payment.
   *
   * This method accepts requests formatted as follows and delegates the deposit
   * action to the inner bank object
   *
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

              state.deposit(tokens, req.target) match {
                case Left(errorMsg) => BadRequest(errorResponse(errorMsg))

                case Right(change) =>

                  Ok(Json.parse(
                    s"""{
                       |"txid": ${req.txId},
                       |"message": "deposit accepted",
                       |"change" :  {
                       |  "coins": [${change.coinsValue.mkString(",")}],
                       |  "notes": [${change.notesValue.mkString(",")}]
                       |}
                   |}""".stripMargin
                  ))
              }
          }
      }
    }(monothreadEc) // hackingly forcing sequencial execution since state update is not threadsafe ^^

  }
}

/**
 * This companion object is mostly for JSOn handling
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