package basics
import basics.DataStructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataStructuresSpec extends AnyFlatSpec with Matchers {

  "sortConsideringEqualValues" should "return correct list (1)" in {
    val input = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
    val expectedOutput = List(Set("e") -> 0, Set("a", "d") -> 1, Set("f", "b", "g") -> 2, Set("c") -> 4)
    val actualOutput = sortConsideringEqualValues(input)
    actualOutput shouldEqual expectedOutput
  }

  it should "return correct list (2)" in {
    val input = Map(2.2 -> 1, 6.5 -> 2, 7.7 -> 4, 5.9 -> 1, 1.5 -> 0, 3.1 -> 2, 9.0 -> 2)
    val expectedOutput = List(Set(1.5) -> 0, Set(2.2, 5.9) -> 1, Set(3.1, 6.5, 9.0) -> 2, Set(7.7) -> 4)
    val actualOutput = sortConsideringEqualValues(input)
    actualOutput shouldEqual expectedOutput
  }

  it should "return correct list (3)" in {
    val input = Map("aaa" -> 1)
    val expectedOutput = List(Set("aaa") -> 1)
    val actualOutput = sortConsideringEqualValues(input)
    actualOutput shouldEqual expectedOutput
  }

  it should "return correct list (4)" in {
    val input = Map(1 -> 2)
    val expectedOutput = List((Set(1),2))
    val actualOutput = sortConsideringEqualValues(input)
    actualOutput shouldEqual expectedOutput
  }

  it should "return correct list (5)" in {
    val input = Map('1' -> 2)
    val expectedOutput = List((Set('1'),2))
    val actualOutput = sortConsideringEqualValues(input)
    actualOutput shouldEqual expectedOutput
  }

  it should "not crash if empty input is supplied (1)" in {
    val input = Map("a" -> 1).empty
    val expectedOutput = List()
    val actualOutput = sortConsideringEqualValues(input)
    actualOutput shouldEqual expectedOutput
  }

  it should "not crash if empty input is supplied (2)" in {
    val expectedOutput = List()
    val actualOutput = sortConsideringEqualValues(Map())
    actualOutput shouldEqual expectedOutput
  }

}
