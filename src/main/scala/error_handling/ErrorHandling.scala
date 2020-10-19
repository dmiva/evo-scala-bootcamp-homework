package error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object ErrorHandling {

  case class PaymentCard(
                          name: CardName, // 2-26 chars
                          number: CardNumber, // 12 - 19 digits
                          expirationDate: CardExpirationDate,
                          securityCode: CardSecurityCode //
                        )
  final case class CardName(name: String) extends AnyVal
  final case class CardNumber(number: String) extends AnyVal
  final case class CardExpirationDate(month: Int, year: Int)
  final case class CardSecurityCode(securityCode: Int) extends AnyVal

  sealed trait ValidationError
  object ValidationError {
    final case object CardNameLengthIsInvalid extends ValidationError {
      override def toString: String = "Cardholder's name must be between 2 and 26 characters"
    }
    final case object Dummy extends ValidationError {
      override def toString: String = "dummy error"
    }
  }

  object PaymentCardValidator {
    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateCardName(name: String): AllErrorsOr[CardName] = Dummy.invalidNec
    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = Dummy.invalidNec
    private def validateCardExpirationDate(expirationDate: String): AllErrorsOr[CardExpirationDate] = Dummy.invalidNec
    private def validateCardSecurityCode(securityCode: String): AllErrorsOr[CardSecurityCode] = Dummy.invalidNec


    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = (
                  validateCardName(name),
                  validateCardNumber(number),
                  validateCardExpirationDate(expirationDate),
                  validateCardSecurityCode(securityCode)
                ).mapN(PaymentCard)
  }
}
