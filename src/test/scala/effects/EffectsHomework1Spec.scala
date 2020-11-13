package effects

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//import effects.EffectsHomework1.IO
import cats.effect.IO

class EffectsHomework1Spec extends AnyFlatSpec with Matchers  {

//  def putStrLn(value: String): IO[Unit] = IO(println(value))
//  def readStrLn: IO[String] = IO(StdIn.readLine())

  "IO.option" should "work with valid data" in {
    val input = IO.pure(42).option
    input.unsafeRunSync() shouldEqual Some(42)
  }
  it should "return None with invalid data" in {
    val input = IO("ttt".toInt).option
    input.unsafeRunSync() shouldEqual None
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

  "raiseError" should "throw an exception" in {
    val ex = new NumberFormatException
    val b = IO.raiseError(ex)
    a [NumberFormatException] should be thrownBy b.unsafeRunSync()
  }


}
