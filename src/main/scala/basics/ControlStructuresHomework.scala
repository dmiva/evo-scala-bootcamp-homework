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
      case Divide(dividend, divisor) => s"$dividend divided by $divisor"
      case Sum(numbers) => s"the sum of ${numbers.mkString(" ")}"
      case Average(numbers) => s"the average of ${numbers.mkString(" ")}"
      case Min(numbers) => s"the minimum of ${numbers.mkString(" ")}"
      case Max(numbers) => s"the maximum of ${numbers.mkString(" ")}"
    }
  }

  final case class ErrorMessage(value: String)
  object ErrorMessage {
    val INVALID_INPUT = "Error: Invalid input"
    val INCORRECT_CMD_NAME = "Error: Incorrect command name: "
    val INPUT_ARGS_NOT_EQUAL_TO_TWO = "Error: There must be 2 input arguments"
  }

  sealed trait Result // adjust Result as required to match requirements
  object Result {
    final case class Divide(x: Command, value: String) extends Result
    final case class Sum(x: Command, value: String) extends Result
    final case class Average(x: Command, value: String) extends Result
    final case class Min(x: Command, value: String) extends Result
    final case class Max(x: Command, value: String) extends Result

    def render(x: Result): String = x match {
      case Divide(x, value) => s"${Command.render(x)} is $value"
      case Sum(x, value) =>s"${Command.render(x)} is $value}"
      case Average(x, value) =>s"${Command.render(x)} is $value}"
      case Min(x, value) =>s"${Command.render(x)} is $value}"
      case Max(x, value) =>s"${Command.render(x)} is $value}"
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???
    val command = x.split("\\s+").toList

    val head = command.headOption
    
    val tail = command.tail.map(_.toDoubleOption)
    if (tail.contains(None)) Left(ErrorMessage(ErrorMessage.INVALID_INPUT))
    else {
      head match {
        case Some("divide") =>
          if (tail.size != 2) Left(ErrorMessage(ErrorMessage.INPUT_ARGS_NOT_EQUAL_TO_TWO))
          else Right(Command.Divide(tail.head.getOrElse(0), tail.last.getOrElse(0)))
        case Some("sum") => Right(Command.Sum(tail.flatten))
        case Some("average") => Right(Command.Average(tail.flatten))
        case Some("min") => Right(Command.Min(tail.flatten))
        case Some("max") => Right(Command.Max(tail.flatten))
        case other => Left(ErrorMessage(ErrorMessage.INCORRECT_CMD_NAME + s"${other.getOrElse("Unknown")}"))
      }
    }


    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    ??? // implement this method

  }

  def renderResult(x: Result): String = {
    Result.render(x)
  }

  def process(x: String): String = {
    // import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    ??? // implement using a for-comprehension
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
//    def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
    def main(args: Array[String]): Unit = {
      val cmd1 = parseCommand("divide 4 5")
      val cmd2 = parseCommand("sum 5 5 6 8.5")
      val cmd3 = parseCommand("average 4 3 8.5 4")
      val cmd4 = parseCommand("min 4 -3 -17")
      val cmd5 = parseCommand("max 4 -3 -17")

      println(cmd1)
      println(cmd2)
      println(cmd3)
      println(cmd4)
      println(cmd5)

      val res1 = Result.Divide(Command.Divide(4,5), "0.8")
      println(Result.render(res1))
      val res2 = Result.Sum(Command.Sum(List(5,5,6,8.5)), "24.5")
      println(Result.render(res2))
    }
}

