package http

import java.util.UUID

import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxFlatMapOps, toSemigroupKOps}
import org.http4s.{HttpRoutes, Request, Response}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.util.Random

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

  val cache = new TrieMap[String, Game]()

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(9000, "localhost")
      .withHttpApp(httpApp(cache))
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  private def httpApp(cache: TrieMap[String, Game]) = {
    newGameRoute(cache) <+> guessRoute(cache)
  }.orNotFound

  private def newGameRoute(cache: TrieMap[String, Game]): HttpRoutes[IO] = {

    object MinNumberMatcher extends QueryParamDecoderMatcher[Int](name = "min")
    object MaxNumberMatcher extends QueryParamDecoderMatcher[Int](name = "max")
    object AttemptsMatcher extends QueryParamDecoderMatcher[Int](name = "attempts")

    HttpRoutes.of[IO] {
      case GET -> Root / "newgame" :? MinNumberMatcher(min) :? MaxNumberMatcher(max) :? AttemptsMatcher(attempts) => for {
        clientId      <- IO(UUID.randomUUID().toString)
        guessedNumber <- IO(Random.between(min,max))
        _             <- IO(cache.put(clientId, Game(guessedNumber, min, max, attempts)))
        response      <- Ok(s"Use curl --cookie clientId=$clientId localhost:9000/game?guess=N for guessing${System.lineSeparator}").map(_.addCookie("clientId", clientId))
      } yield response
    }
  }

  private def guessRoute(cache: TrieMap[String, Game]): HttpRoutes[IO] = {

    object GuessMatcher extends QueryParamDecoderMatcher[Int](name = "guess")

    HttpRoutes.of[IO] {
      case req @ GET -> Root / "game" :? GuessMatcher(guess) => {
        val cookie = req.cookies.find(_.name == "clientId")

        cookie.map(_.content) match {
          case Some(id) => {
            cache.get(id) match {
              case Some(game) => processGuess(cache, id, game, guess)
              case None       => BadRequest(s"Game for clientId=$id has not been started")
            }
          }
          case None           => BadRequest(s"ClientId not present in request")
        }
      }
    }
  }

  def processGuess(cache: TrieMap[String, Game], clientId: String, game: Game, guess: Int): IO[Response[IO]] = {
    val attemptsLeft = game.attemptsLeft - 1

    if (attemptsLeft == 0) {
      IO(cache.remove(clientId)) *> Ok(s"You have lost! The guessed number was ${game.guessedNumber}${System.lineSeparator}")
    } else {
      game.checkGuess(guess) match {
        case Equal => {
          IO(cache.remove(clientId)) *> Ok(s"You have guessed the number! Thanks for playing!")
        }
        case Higher => {
          val updatedGame = game.copy(attemptsLeft = attemptsLeft)
          IO(cache.put(clientId, updatedGame)) *> Ok(s"Number is higher! You have $attemptsLeft attempts left!${System.lineSeparator}")
        }
        case Lower =>{
          val updatedGame = game.copy(attemptsLeft = attemptsLeft)
          IO(cache.put(clientId, updatedGame)) *> Ok(s"Number is lower! You have $attemptsLeft attempts left!${System.lineSeparator}")
        }
      }
    }
  }

  trait GuessResult
  final case object Lower extends GuessResult
  final case object Higher extends GuessResult
  final case object Equal extends GuessResult

  final case class Game(guessedNumber: Int, min: Int, max: Int, attemptsLeft: Int) {
    def checkGuess(clientGuess: Int): GuessResult = {
      if (clientGuess < guessedNumber) Higher
      else if (clientGuess > guessedNumber) Lower
      else Equal
    }
  }

}