package basics

import scala.io.Source

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command

    def render(x: Command): String = x match {
      case Divide(dividend, divisor)  => s"${formatValue(dividend)} divided by ${formatValue(divisor)}"
      case Sum(numbers)               => s"the sum of ${numbers.map(formatValue).mkString(" ")}"
      case Average(numbers)           => s"the average of ${numbers.map(formatValue).mkString(" ")}"
      case Min(numbers)               => s"the minimum of ${numbers.map(formatValue).mkString(" ")}"
      case Max(numbers)               => s"the maximum of ${numbers.map(formatValue).mkString(" ")}"
    }
  }

  final case class ErrorMessage(value: String)
  object ErrorMessage {
    val INVALID_INPUT               = "Error: Invalid input"
    val EMPTY_INPUT                 = "Error: Empty input"
    val INCORRECT_CMD_NAME          = "Error: Incorrect command name"
    val INPUT_ARGS_ARE_MISSING      = "Error: Input arguments are missing"
    val INPUT_ARGS_NOT_EQUAL_TO_TWO = "Error: Division must have 2 input arguments"
    val DIVISION_BY_ZERO            = "Error: Division by zero"
  }

  sealed trait Result // adjust Result as required to match requirements
  object Result {
    final case class Divide(x: Command, value: String) extends Result
    final case class Sum(x: Command, value: String) extends Result
    final case class Average(x: Command, value: String) extends Result
    final case class Min(x: Command, value: String) extends Result
    final case class Max(x: Command, value: String) extends Result

    def render(x: Result): String = x match {
      case Divide(x, value)   => s"${Command.render(x)} is $value"
      case Sum(x, value)      => s"${Command.render(x)} is $value"
      case Average(x, value)  => s"${Command.render(x)} is $value"
      case Min(x, value)      => s"${Command.render(x)} is $value"
      case Max(x, value)      => s"${Command.render(x)} is $value"
    }
  }

  // formats round doubles as integers
  val formatValue: Double => String = { (d: Double) =>
    if (d == d.toLong) f"$d%.0f"
    else s"$d"
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???
    // Consider how to handle extra whitespace gracefully (without errors).
    val command = x.split("\\s+")
    val head = command.toList.headOption

    if (head.isEmpty || (command.size == 1 && command(0) == "")) Left(ErrorMessage(ErrorMessage.EMPTY_INPUT))
    else {

      val tail = command.toList.tail.map(_.toDoubleOption)
      if (tail.contains(None)) Left(ErrorMessage(ErrorMessage.INVALID_INPUT))
      else if (tail.isEmpty) Left(ErrorMessage(ErrorMessage.INPUT_ARGS_ARE_MISSING))
      else {
        head match {
          case Some("divide")   => if (tail.size != 2) Left(ErrorMessage(ErrorMessage.INPUT_ARGS_NOT_EQUAL_TO_TWO))
          else Right(Command.Divide(tail.head.getOrElse(0), tail.last.getOrElse(0)))
          case Some("sum")      => Right(Command.Sum(tail.flatten))
          case Some("average")  => Right(Command.Average(tail.flatten))
          case Some("min")      => Right(Command.Min(tail.flatten))
          case Some("max")      => Right(Command.Max(tail.flatten))
          case _                => Left(ErrorMessage(ErrorMessage.INCORRECT_CMD_NAME))
        }
      }
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(dividend, divisor)  => if (divisor == 0) Left(ErrorMessage(ErrorMessage.DIVISION_BY_ZERO))
                                                 else Right(Result.Divide(x, formatValue(dividend/divisor)))
      case Command.Sum(numbers)               => Right(Result.Sum(x, formatValue(numbers.sum)))
      case Command.Average(numbers)           => Right(Result.Average(x, formatValue(numbers.sum/numbers.length)))
      case Command.Min(numbers)               => Right(Result.Min(x, formatValue(numbers.min)))
      case Command.Max(numbers)               => Right(Result.Max(x, formatValue(numbers.max)))
    }
  }

  def renderResult(x: Result): String = {
    Result.render(x)
  }

  def process(x: String): String = {
    // import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    // implement using a for-comprehension
    val result = for {
      parsedInput <- parseCommand(x)
      calculatedResult <- calculate(parsedInput)
    } yield renderResult(calculatedResult)

    result match {
      case Left(error) => error.value
      case Right(value) => value
    }

  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}

