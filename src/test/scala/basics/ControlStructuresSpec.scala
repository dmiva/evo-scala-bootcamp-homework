package basics
import basics.ControlStructuresHomework._
import basics.ControlStructuresHomework.Command._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ControlStructuresSpec extends AnyFlatSpec with Matchers {

  // parseCommand divide
  "parseCommand divide" should "parse a valid input 1" in {
    val command = "divide 4 5"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Divide(4.0,5.0))
  }

  it should "parse a valid input 2" in {
    val command = "divide 0 0"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Divide(0.0,0.0))
  }

  it should "parse a valid input 3" in {
    val command = "divide 3 0"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Divide(3.0,0.0))
  }

 it should "parse a valid input with different whitespaces" in {
    val command = "   divide\t 4   \r\n    3   "
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INVALID_INPUT))
  }

  it should "parse an invalid input with char in arguments" in {
    val command = "divide 4 e"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INVALID_INPUT))
  }

  it should "parse an invalid input with 3 arguments" in {
    val command = "divide 4 5 6"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_NOT_EQUAL_TO_TWO))
  }

  it should "parse an invalid input with 1 argument" in {
    val command = "divide 4"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_NOT_EQUAL_TO_TWO))
  }

  it should "parse an invalid input with 0 arguments" in {
    val command = "divide "
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_ARE_MISSING))
  }

  it should "parse an empty input 1" in {
    val command = " "
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.EMPTY_INPUT))
  }

  it should "parse an empty input 2" in {
    val command = ""
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.EMPTY_INPUT))
  }

  it should "parse an invalid input with incorrect command" in {
    val command = "dividee 4 7"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INCORRECT_CMD_NAME))
  }

  // parseCommand sum
  "parseCommand sum" should "parse a valid input" in {
    val command = "sum 5 5 6 8.5"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Sum(List(5.0, 5.0, 6.0, 8.5)))
  }

  it should "parse an invalid input with 0 arguments" in {
    val command = "sum"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_ARE_MISSING))
  }

  it should "parse an invalid input with char in arguments" in {
    val command = "sum 4 e"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INVALID_INPUT))
  }

  // parseCommand average
  "parseCommand average" should "parse a valid input" in {
    val command = "average 4 3 8.5 4"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Average(List(4.0, 3.0, 8.5, 4.0)))
  }

  it should "parse an invalid input with char in arguments" in {
    val command = "average 4 3 8.5 4 rr3"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INVALID_INPUT))
  }

  it should "parse an invalid input with 0 arguments" in {
    val command = "average"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_ARE_MISSING))
  }

  // Because check for INPUT_ARGS_ARE_MISSING is before check for INCORRECT_CMD_NAME, this test fails.
  // But it only fails to correctly describe the error.
  // Otherwise, it works as expected
  it should "parse an invalid command with input with 0 arguments" in {
    val command = "averageeew"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INCORRECT_CMD_NAME))
  }

  // parseCommand min
  "parseCommand min" should "parse a valid input" in {
    val command = "min 4 -3 -17"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Min(List(4.0, -3.0, -17.0)))
  }

  it should "parse an invalid input with 0 arguments" in {
    val command = "min"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_ARE_MISSING))
  }

  // parseCommand max
  "parseCommand max" should "parse a valid input" in {
    val command = "max 4 -3 -17"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Max(List(4.0, -3.0, -17.0)))
  }

  it should "parse an invalid input with 0 arguments" in {
    val command = "max"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_ARE_MISSING))
  }

  // process
  "process" should "be correct (divide) 1" in {
    val command = "divide 4 5"
    val output = "4 divided by 5 is 0.8"
    val result = process(command)
    result shouldEqual output
  }

  "process" should "be correct (divide) 2" in {
    val command = "divide 4 0"
    val output = ErrorMessage.DIVISION_BY_ZERO
    val result = process(command)
    result shouldEqual output
  }

  "process" should "be correct (divide) 3" in {
    val command = "divide 4"
    val output = ErrorMessage.INPUT_ARGS_NOT_EQUAL_TO_TWO
    val result = process(command)
    result shouldEqual output
  }

  "process" should "be correct (divide) 4" in {
    val command = "divide"
    val output = ErrorMessage.INPUT_ARGS_ARE_MISSING
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (sum) 1" in {
    val command = "sum 5 5 6 8.5"
    val output = "the sum of 5 5 6 8.5 is 24.5"
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (sum) 2" in {
    val command = "sum 5"
    val output = "the sum of 5 is 5"
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (sum) 3" in {
    val command = "sum"
    val output = ErrorMessage.INPUT_ARGS_ARE_MISSING
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (sum) 4" in {
    val command = "sum erewr w rew rwer "
    val output = ErrorMessage.INVALID_INPUT
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (average) 1" in {
    val command = "average 4 3 8.5 4"
    val output = "the average of 4 3 8.5 4 is 4.875"
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (average) 2" in {
    val command = "average -1 (3/3)"
    val output = ErrorMessage.INVALID_INPUT
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (min) 1" in {
    val command = "min 4 -3 -17"
    val output = "the minimum of 4 -3 -17 is -17"
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (min) 2" in {
    val command = "min -1 -2 rrr"
    val output = ErrorMessage.INVALID_INPUT
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (min) 3" in {
    val command = "min "
    val output = ErrorMessage.INPUT_ARGS_ARE_MISSING
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct (max) 1" in {
    val command = "max 4 -3 -17"
    val output = "the maximum of 4 -3 -17 is 4"
    val result = process(command)
    result shouldEqual output
  }

  it should "be correct 1" in {
    val command = ""
    val output = ErrorMessage.EMPTY_INPUT
    val result = process(command)
    result shouldEqual output
  }
}
