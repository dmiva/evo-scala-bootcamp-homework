package effects

import effects.SharedStateHomework.Cache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import cats.effect.{ContextShift, IO, Timer}

import scala.concurrent.ExecutionContext

// Test does not work, it just pass
class SharedStateHomeworkSpec extends AnyFlatSpec with Matchers {

  implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global
  implicit val timer: Timer[IO] = IO.timer(executionContext)
  implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)

  "RefCache" should "work" in {
    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      value1 <- cache.put(1, "value1")
      value2 <- cache.put(2, "value2")
      value11 <- cache.get(1)
      value21 <- cache.get(2)
      _ <- IO.sleep(8.seconds)
      value12 <- cache.get(1)
      value22 <- cache.get(2)
      _ <- IO.sleep(3.seconds)
      value13 <- cache.get(1)
      value23 <- cache.get(2)
      _ <- IO.sleep(2.seconds) // Should evict during this sleep
      value14 <- cache.get(1)
      value24 <- cache.get(2)
    } yield {
      value11 shouldEqual value1
      value12 shouldEqual value1
      value13 shouldEqual value1
      value14 shouldEqual None
      value21 shouldEqual value2
      value22 shouldEqual value2
      value23 shouldEqual value2
      value24 shouldEqual None
    }
  }

}
