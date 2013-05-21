package controllers

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def luhnTest(number: String): Boolean = {
    val digits = number.reverse.map {
      _.toString.toInt
    }
    val s = digits.grouped(2) map {
      t => t(0) +
        (if (t.length > 1) (t(1) * 2) % 10 + t(1) / 5 else 0)
    }
    s.sum % 10 == 0
  }

  case class Payment(service: String, number: String, security: String)

  def creditCardNumberReads(implicit r: Reads[String]): Reads[String] = Reads.filter(ValidationError("validate.error.luhn-test"))(luhnTest(_))

  implicit val paymentReads: Reads[Payment] = (
    (__ \ 'service).read[String](minLength[String](4)) and
      (__ \ 'number).read[String](creditCardNumberReads) and
      (__ \ 'security).read[String](minLength[String](4))
    )(Payment.apply _)

  def insertPayment =
    Action(parse.json) {
      implicit request =>
        val json = request.body
        json.validate[Payment].fold(
          valid = (res => Ok(json)),
          invalid = (e => BadRequest(e.toString))
        )
    }


}