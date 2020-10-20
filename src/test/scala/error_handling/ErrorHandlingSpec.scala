package error_handling

import ErrorHandling._
import cats.syntax.all._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ErrorHandlingSpec
  extends AnyFlatSpec
    with Matchers {

  import ValidationError._

  def checkInvalid(name: String, number: String, expirationDate: String, securityCode: String, errors: Set[ValidationError]): Assertion =
    PaymentCardValidator.validate(
      name = name,
      number = number,
      expirationDate = expirationDate,
      securityCode = securityCode
    ).leftMap(_.toList.toSet) shouldBe errors.invalid

  "PaymentCardValidator" should "handle valid payment card" in {
    PaymentCardValidator.validate(
      name = "NAME SURNAME",
      number = "4485480053850111",
      expirationDate = "12/31",
      securityCode = "115"
    ) shouldBe PaymentCard(
        CardName("NAME SURNAME"),
        CardNumber("4485480053850111"),
        CardExpirationDate(12, 31),
        CardSecurityCode(115)
    ).validNec
  }

  it should "handle invalid card name length" in {
    checkInvalid(
      name = "Mr Very Long Name and Surname",
      number = "4485480053850111",
      expirationDate = "12/31",
      securityCode = "115",
      errors = Set(CardNameLengthIsInvalid)
    )
  }

  it should "handle invalid card name characters" in {
    checkInvalid(
      name = "X Æ A-12 Musk",
      number = "4485480053850111",
      expirationDate = "12/31",
      securityCode = "115",
      errors = Set(CardNameHasInvalidCharacters)
    )
  }

  it should "handle invalid card number characters" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "448548005385011g",
      expirationDate = "12/31",
      securityCode = "115",
      errors = Set(CardNumberHasInvalidCharacters)
    )
  }

  it should "handle invalid card number length" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "23553",
      expirationDate = "12/31",
      securityCode = "115",
      errors = Set(CardNumberLengthIsInvalid)
    )
  }

  it should "handle invalid card issuer" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "3542575317149607",
      expirationDate = "12/31",
      securityCode = "115",
      errors = Set(CardNumberIssuerIsNotAccepted)
    )
  }

  it should "handle invalid card number checksum" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "4485480053850112",
      expirationDate = "12/31",
      securityCode = "115",
      errors = Set(CardNumberChecksumIsInvalid)
    )
  }

  it should "handle invalid card date expiration format" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "4485480053850111",
      expirationDate = "10/2031",
      securityCode = "115",
      errors = Set(CardNumberExpirationDateIsInvalidFormat)
    )
  }

  it should "handle invalid card date month number" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "4485480053850111",
      expirationDate = "13/31",
      securityCode = "115",
      errors = Set(CardMonthOutOfBounds)
    )
  }

  // With current implementation, will never happen
//  it should "handle invalid card date year number" in {
//    checkInvalid(
//      name = "NAME SURNAME",
//      number = "4485480053850111",
//      expirationDate = "12/3",
//      securityCode = "115",
//      errors = Set(CardYearOutOfBounds)
//    )
//  }

  // With current implementation, will never happen
//  it should "handle invalid card date number" in {
//    checkInvalid(
//      name = "NAME SURNAME",
//      number = "4485480053850111",
//      expirationDate = "/22",
//      securityCode = "115",
//      errors = Set(CardYearOutOfBounds)
//    )
//  }

  it should "handle cards that are already expired" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "4485480053850111",
      expirationDate = "09/20",
      securityCode = "115",
      errors = Set(CardHasExpired)
    )
  }

  it should "handle invalid card sercurity code characters" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "4485480053850111",
      expirationDate = "09/31",
      securityCode = "1a5",
      errors = Set(CardSecurityCodeNotNumeris)
    )
  }

  it should "handle invalid card sercurity code length" in {
    checkInvalid(
      name = "NAME SURNAME",
      number = "4485480053850111",
      expirationDate = "09/31",
      securityCode = "1345",
      errors = Set(CardSecurityCodeNotInBounds)
    )
  }

}
