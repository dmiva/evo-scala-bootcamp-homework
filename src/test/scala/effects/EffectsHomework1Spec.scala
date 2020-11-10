package effects

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import effects.EffectsHomework1.IO

class EffectsHomework1Spec extends AnyFlatSpec with Matchers  {

  "IO.option" should "work with valid data" in {
    val input = IO(42)
    val output = IO(Some(42))
    input.option shouldEqual output
  }

}
