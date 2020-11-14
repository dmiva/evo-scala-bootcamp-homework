package effects

import java.io.ByteArrayOutputStream
import java.time.Instant
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.ExecutionContext
import scala.util.Try
//import cats.effect.IO
import effects.EffectsHomework1.IO

class EffectsHomework1Spec extends AnyFlatSpec with Matchers  {

  "map" should "work" in {
    val a = IO("aaa")
    val c = a.map(_ => "ccc")
    c.unsafeRunSync() shouldEqual "ccc"
  }

  "flatMap" should "work" in {
    val a = IO("aaa")
    val b = IO("bbb")
    val c = a.flatMap(_ => b)
    c.unsafeRunSync() shouldEqual "bbb"
  }

  "*>" should "work" in {
    val a = IO("aaa")
    val b = IO("bbb")
    val c = a *> b
    c.unsafeRunSync() shouldEqual "bbb"
  }

  "as" should "work" in {
    val a = IO("aaa")
    val b = IO("bbb")
    val c = a as b
    c.unsafeRunSync() shouldEqual b
  }

  "void" should "return ()" in {
    val input = IO("aaa").void
    input.unsafeRunSync() shouldEqual ()
  }

  "attempt" should "materialize an exception" in {
    val ex = new NumberFormatException
    val b = IO.raiseError(ex).attempt
    b.unsafeRunSync() shouldEqual Left(ex)
  }

  it should "work with value" in {
    val b = IO(42).attempt
    b.unsafeRunSync() shouldEqual Right(42)
  }

  "option" should "work with valid data" in {
    val input = IO(42).option
    input.unsafeRunSync() shouldEqual Some(42)
  }

  it should "return None with invalid data" in {
    val input = IO("ttt".toInt).option
    input.unsafeRunSync() shouldEqual None
  }

  "handleErrorWith" should "handle error" in {
    val input = IO(5/0).handleErrorWith(_ => IO(-1))
    input.unsafeRunSync() shouldEqual -1
  }

  it should "handle valid value" in {
    val input = IO(5/1).handleErrorWith(_ => IO(-1))
    input.unsafeRunSync() shouldEqual 5
  }

  "redeem" should "handle error" in {
    val input = IO(5/0).redeem(_ => -1, _ => 1)
    input.unsafeRunSync() shouldEqual -1
  }

  it should "handle valid value" in {
    val input = IO(5/1).redeem(_ => -1, _ => 1)
    input.unsafeRunSync() shouldEqual 1
  }

  "redeemWith" should "handle error" in {
    val input = IO(5/0).redeemWith(_ => IO(-1), _ => IO(1))
    input.unsafeRunSync() shouldEqual -1
  }

  it should "handle valid value" in {
    val input = IO(5/1).redeemWith(_ => IO(-1), _ => IO(1))
    input.unsafeRunSync() shouldEqual 1
  }

  "unsafeToFuture" should "handle valid value" in {
    implicit val ec = ExecutionContext.global
    val input = IO(1+2).unsafeToFuture()
    input.onComplete {
      case scala.util.Success(value) => value shouldEqual 3
      case scala.util.Failure(exception) => fail(s"3 expected, but $exception found")
    }
  }

  it should "handle error" in {
    implicit val ec = ExecutionContext.global
    val ex = new NumberFormatException
    val input = IO.raiseError(ex).unsafeToFuture()
    input.onComplete {
      case scala.util.Success(value) => fail(s"$ex expected, but $value found")
      case scala.util.Failure(exception) => exception shouldBe ex
    }
  }

  "suspend" should "suspend the side effect (1)" in {
    val input = IO.suspend(IO(Instant.now()))
    Thread.sleep(20)
    input.unsafeRunSync() shouldEqual Instant.now()
  }

  it should "suspend the side effect (2)" in {
    val baos = new ByteArrayOutputStream
    val hello = "Hello"
    Console.withOut(baos) {
      val input = IO.suspend({ IO(print(hello)) })
      baos.toString shouldEqual ""
      input.unsafeRunSync()
      baos.toString shouldEqual hello
    }
  }

  "delay" should "suspend the side effect (1)" in {
    val input = IO.delay(Instant.now())
    Thread.sleep(20)
    input.unsafeRunSync() shouldEqual Instant.now()
  }

  it should "suspend the side effect (2)" in {
    val baos = new ByteArrayOutputStream
    val hello = "Hello"
    Console.withOut(baos) {
      val input = IO.delay({ print(hello) })
      baos.toString shouldEqual ""
      input.unsafeRunSync()
      baos.toString shouldEqual hello
    }
  }

  "pure" should "suspend pure value" in {
    IO.pure(42).unsafeRunSync() shouldEqual 42
  }

  "fromEither" should "convert from Right" in {
    val input = IO.fromEither(Right(4)).unsafeRunSync()
    input shouldEqual 4
  }

  it should "convert from Left" in {
    val ex = new NumberFormatException
    val input = IO.fromEither(Left(ex))
    a [NumberFormatException] should be thrownBy input.unsafeRunSync()
  }

  "fromOption" should "convert from Some" in {
    val ex = new NumberFormatException
    val input = IO.fromOption(Some(5))(ex).unsafeRunSync()
    input shouldEqual 5
  }

  it should "throw an exception in case of None" in {
    val ex = new NumberFormatException
    val input = IO.fromOption(None)(ex)
    a [NumberFormatException] should be thrownBy input.unsafeRunSync()
  }

  "fromTry" should "convert from Success" in {
    val input = IO.fromTry(Try(5/1)).unsafeRunSync()
    input shouldEqual 5
  }

  it should "throw an exception in case of Failure" in {
    val input = IO.fromTry(Try(5/0))
    a [ArithmeticException] should be thrownBy input.unsafeRunSync()
  }

  "none" should "return None" in {
    val input = IO.none
    input.unsafeRunSync() shouldEqual None
  }

  "raiseError" should "throw an exception" in {
    val ex = new NumberFormatException
    val b = IO.raiseError(ex)
    a [NumberFormatException] should be thrownBy b.unsafeRunSync()
  }

  "raiseUnless" should "handle true condition" in {
    val ex = new NumberFormatException
    val input = IO.raiseUnless(true)(ex)
    input.unsafeRunSync() shouldEqual ()
  }

  it should "handle false condition" in {
    val ex = new NumberFormatException
    val input = IO.raiseUnless(false)(ex)
    a [NumberFormatException] should be thrownBy input.unsafeRunSync()
  }

  "raiseWhen" should "handle true condition" in {
    val ex = new NumberFormatException
    val input = IO.raiseWhen(true)(ex)
    a [NumberFormatException] should be thrownBy input.unsafeRunSync()
  }

  it should "handle false condition" in {
    val ex = new NumberFormatException
    val input = IO.raiseWhen(false)(ex)
    input.unsafeRunSync() shouldEqual ()
  }

  "unlessA" should "handle true condition" in {
    val baos = new ByteArrayOutputStream()
    Console.withOut(baos) {
      val input = IO.unlessA(true)({ IO(print("Hello")) })
      baos.toString shouldEqual ""
      input.unsafeRunSync() shouldEqual ()
    }
  }

  it should "handle false condition" in {
    val baos = new ByteArrayOutputStream()
    Console.withOut(baos) {
      val input = IO.unlessA(false)({ IO(print("Hello")) }).unsafeRunSync()
      baos.toString shouldEqual "Hello"
    }
  }

  "whenA" should "handle true condition" in {
    val baos = new ByteArrayOutputStream()
    Console.withOut(baos) {
      val input = IO.unlessA(true)({ IO(print("Hello")) })
      input.unsafeRunSync() shouldEqual ()
    }
  }

  it should "handle false condition" in {
    val baos = new ByteArrayOutputStream()
    Console.withOut(baos) {
      val input = IO.unlessA(false)({ IO(print("Hello")) }).unsafeRunSync()
      baos.toString shouldEqual "Hello"
    }
  }

}
