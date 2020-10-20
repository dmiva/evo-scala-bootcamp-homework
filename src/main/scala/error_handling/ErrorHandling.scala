package error_handling

import java.time.YearMonth
import cats.data.ValidatedNec
import cats.syntax.all._

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object ErrorHandling {

  case class PaymentCard (
                          name: CardName, // 2-26 chars
                          number: CardNumber, // 15 - 16 digits
                          expirationDate: CardExpirationDate, // "MM/YY"
                          securityCode: CardSecurityCode // 3 digits
                        )

  final case class CardName(name: String) extends AnyVal
  final case class CardNumber(number: String) extends AnyVal
  final case class CardExpirationDate(month: Int, year: Int)
  final case class CardSecurityCode(securityCode: Int) extends AnyVal

  sealed trait ValidationError
  object ValidationError {
    final case object CardNameLengthIsInvalid extends ValidationError {
      override def toString: String = "Card name must be between 2 and 26 characters"
    }
    final case object CardNameHasInvalidCharacters extends ValidationError {
      override def toString: String = "Card name must contain only alphabetical letters and space"
    }
    final case object CardNumberHasInvalidCharacters extends ValidationError {
      override def toString: String = "Card number must contain only digits"
    }
    final case object CardNumberLengthIsInvalid extends ValidationError {
      override def toString: String = "Card number's length must be 15 or 16 characters"
    }
    final case object CardNumberIssuerIsNotAccepted extends ValidationError {
      override def toString: String = "Card number does not belong to accepted list of issuers"
    }
    final case object CardNumberChecksumIsInvalid extends ValidationError {
      override def toString: String = "Specified string of digits is not a valid credit card number"
    }
    final case object CardNumberExpirationDateIsInvalidFormat extends ValidationError {
      override def toString: String = "Expiration date must be formatted as 'MM/YY'"
    }
    final case object CardMonthOutOfBounds extends ValidationError {
      override def toString: String = "Card expiration month is out of bounds"
    }
    final case object CardYearOutOfBounds extends ValidationError {
      override def toString: String = "Card expiration year is out of bounds"
    }
    final case object CardDateOutOfBounds extends ValidationError {
      override def toString: String = "Card expiration date is out of bounds"
    }
    final case object CardHasExpired extends ValidationError {
      override def toString: String = "Card has expired"
    }
    final case object CardSecurityCodeNotNumeris extends ValidationError {
      override def toString: String = "Security code must be a number"
    }
    final case object CardSecurityCodeNotInBounds extends ValidationError {
      override def toString: String = "Security code must be between 100 and 999"
    }
  }

  object PaymentCardIssuers {
    val issuers = Map(
      "VISA" -> "^4[0-9]{6,}$",
      "MasterCard" -> "^5[1-5][0-9]{5,}$",
      "AmEx" -> "^3[47][0-9]{5,}$"
    )
  }

  object PaymentCardValidator {
    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateCardName(name: String): AllErrorsOr[CardName] = {

      def validateCardNameLength: AllErrorsOr[CardName] =
        if (2 <= name.length && name.length <= 26) CardName(name).validNec
        else CardNameLengthIsInvalid.invalidNec

      def validateCardNameContents: AllErrorsOr[CardName] = {
        if (name.matches("^[a-zA-Z ]+$")) CardName(name).validNec
        else CardNameHasInvalidCharacters.invalidNec
      }

      validateCardNameLength.productR(validateCardNameContents)
    }

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {

      def validateCardNumberLength: AllErrorsOr[String] =
        if (number.length >= 15 && number.length <= 16) number.validNec
        else CardNumberLengthIsInvalid.invalidNec

      def validateCardNumberContents(number: String): AllErrorsOr[String] =
        if (number.matches("^\\d+$")) number.validNec
        else CardNumberHasInvalidCharacters.invalidNec

      def validateCardNumberIssuer(number: String): AllErrorsOr[String] =
        if (PaymentCardIssuers.issuers.values.exists(issuer => number.matches(issuer))) number.validNec
        else CardNumberIssuerIsNotAccepted.invalidNec

      def validateCardNumberChecksum(number: String): AllErrorsOr[CardNumber] =
        if (isCardNumberChecksumValid(number)) CardNumber(number).validNec
        else CardNumberChecksumIsInvalid.invalidNec

      def isCardNumberChecksumValid(number: String): Boolean = {
        // https://gist.github.com/mdp/9691528
        var sum = 0
        var alternate = false
        for (i <- number.length - 1 to 0 by -1) {
          var n = number.substring(i, i + 1).toInt
          if (alternate) {
            n *= 2
            if (n > 9) n = (n % 10) + 1
          }
          sum += n
          alternate = !alternate
        }
        sum % 10 == 0
      }

      validateCardNumberLength.andThen(validateCardNumberContents).andThen(validateCardNumberIssuer).andThen(validateCardNumberChecksum)
    }

    private def validateCardExpirationDate(expirationDate: String): AllErrorsOr[CardExpirationDate] = {

      // I put this here, because I did not find out how I can pass second argument
      // to validateCardExpirationDateBounds, and pass this value in method chaining
      val dateSeparator = "/"

      def validateCardExpirationDateContents(separator: String): AllErrorsOr[String] = {
        if (expirationDate.matches("^\\d{2}\\" + separator + "\\d{2}$")) expirationDate.validNec
        else CardNumberExpirationDateIsInvalidFormat.invalidNec
      }

      def validateCardExpirationDateBounds(in: String): AllErrorsOr[CardExpirationDate] = {
        // This was the hardest part for me (to make a safe conversion from String -> Int, Int)

        // Are we allowed to trust the input argument, that at this point
        // we have string of format "MM/YY" because this validation is after regex validation?
        val date = in.split(dateSeparator).toSeq

        // Pretty obscure code
        (date(0).toIntOption,date(1).toIntOption) match {
          case (Some(month), Some(year)) if
            (month >= 1 && month <= 12 && year >= 0 && year <= 99)  => CardExpirationDate(month, year).validNec
          case (Some(month), _) if month < 1 || month > 12          => CardMonthOutOfBounds.invalidNec
          case (_, Some(year)) if year < 0 || year > 99             => CardYearOutOfBounds.invalidNec
          case _                                                    => CardDateOutOfBounds.invalidNec
        }
      }

      def checkIfCardAlreadyExpired(date: CardExpirationDate): AllErrorsOr[CardExpirationDate] = {
        val month = date.month
        val year = date.year
        val cardDate = YearMonth.of(2000+year, month)
        if (cardDate.isBefore(YearMonth.now())) CardHasExpired.invalidNec
        else CardExpirationDate(month, year).validNec
      }

      validateCardExpirationDateContents(dateSeparator).andThen(validateCardExpirationDateBounds).andThen(checkIfCardAlreadyExpired)
    }

    private def validateCardSecurityCode(securityCode: String): AllErrorsOr[CardSecurityCode] = {

      def validateSecurityCodeNumeric: AllErrorsOr[Int] =
        securityCode.toIntOption match {
          case Some(value) => value.validNec
          case None => CardSecurityCodeNotNumeris.invalidNec
        }

      def validateSecurityCodeBounds(code: Int): AllErrorsOr[CardSecurityCode] =
        if (code >= 100 && code <= 999) CardSecurityCode(code).validNec
        else CardSecurityCodeNotInBounds.invalidNec

      validateSecurityCodeNumeric.andThen(validateSecurityCodeBounds)
    }

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
