package http

import cats.effect.{ExitCode, IO, IOApp}
import http.Protocol.NewGame
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.
object GuessServer extends IOApp {

  final case class Game(guessedNumber: Int, attempts: Int)



  private val newGameRoutes = {

    import io.circe.generic.auto._
    import io.circe.syntax._
    import org.http4s.circe.CirceEntityCodec._

    object MinNumberMatcher extends QueryParamDecoderMatcher[Int](name = "min")
    object MaxNumberMatcher extends QueryParamDecoderMatcher[Int](name = "max")
    object AttemptsMatcher extends QueryParamDecoderMatcher[Int](name = "attempts")

//    HttpRoutes.of[IO] {
//      case req @ GET -> Root / "newgame" =>
//        req.as[NewGame].flatMap { game =>
//          val newGame = NewGame(min = game.min, max = game.max, attempts = game.attempts)
//
//          Ok("You can start guessing!".asJson).map(_.addCookie("clientId", "1"))
//        }
//    }

    HttpRoutes.of[IO] {
      case req @ GET -> Root / "newgame" :? MinNumberMatcher(min) :? MaxNumberMatcher(max) :? AttemptsMatcher(attempts) =>
      Ok(s"Min: $min, Max: $max, Att: $attempts !").map(_.addCookie("clientId", "1"))
    }
  }

  private val httpApp = {
    newGameRoutes
  }.orNotFound

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(9000, "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

}

object Protocol {
  final case class NewGame(min: Int, max: Int, attempts: Int)

//  trait Guess
//  final case class Lower()
}