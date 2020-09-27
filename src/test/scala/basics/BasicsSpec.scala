package basics

import Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BasicsSpec extends AnyFlatSpec with Matchers {
  "A gcd of 48 and 18" should "be 6" in {
    gcd(48,18) shouldEqual 6
  }

  "A gcd of 48 and 180" should "be 12" in {
    gcd(48,180) shouldEqual 12
  }

  "A lcm of 48 and 180" should "be 720" in {
    lcm(48,180) shouldEqual 720
  }

  "A lcm of 0 and 0" should "be 0" in {
    lcm(0,0) shouldEqual 0
  }
}
