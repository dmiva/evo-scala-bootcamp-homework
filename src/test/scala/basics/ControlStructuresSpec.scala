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
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INPUT_ARGS_NOT_EQUAL_TO_TWO))
  }

  it should "parse an invalid input with incorrect command" in {
    val cmdName = "dividee"
    val command = s"$cmdName 4 7"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Left(ErrorMessage(ErrorMessage.INCORRECT_CMD_NAME + cmdName))
  }

  // parseCommand sum
  "parseCommand sum" should "parse a valid input" in {
    val command = "sum 5 5 6 8.5"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Sum(List(5.0, 5.0, 6.0, 8.5)))
  }

  // parseCommand average
  "parseCommand average" should "parse a valid input" in {
    val command = "average 4 3 8.5 4"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Average(List(4.0, 3.0, 8.5, 4.0)))
  }

  // parseCommand min
  "parseCommand min" should "parse a valid input" in {
    val command = "min 4 -3 -17"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Min(List(4.0, -3.0, -17.0)))
  }

  // parseCommand max
  "parseCommand max" should "parse a valid input" in {
    val command = "max 4 -3 -17"
    val parsedCommand = parseCommand(command)
    parsedCommand shouldEqual Right(Max(List(4.0, -3.0, -17.0)))
  }

}
