package http

import org.http4s.implicits.http4sLiteralsSyntax
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import http.GuessServer.{Higher, JsonResponse, Lost, Lower, Equal}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._

import scala.concurrent.ExecutionContext
import scala.util.Random
import org.http4s.client._
import org.http4s.dsl.io.GET

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
object GuessClient extends IOApp {

  private val uri = uri"http://localhost:9000"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, blocker) =>
      for {
        game <- createNewGame(client)
        _ <- makeGuess(client, game.clientId, game.guess, game.min, game.max)
      } yield ()
    }.as(ExitCode.Success)

  private def createNewGame(client: Client[IO]): IO[Game] = {

    val min = Random.between(1, 100)
    val max = Random.between(101, 200)
    val attempts = Random.between(3, 10)

    client.get((uri / "newgame").withQueryParams(Map[String, Int](
      "min" -> min,
      "max" -> max,
      "attempts" -> attempts
    )))(_.cookies.find(_.name == "clientId") match {
      case Some(id) => printLine(s"Starting game - Min:$min, Max:$max, Nr of attempts: $attempts") *>
        IO(Game(id.content, Random.between(min, max), min, max))
      case None => IO(Game("0",0,0,0))
    })
  }

  private def makeGuess(client: Client[IO], clientId: String, guess: Int, min: Int, max: Int): IO[Unit] = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    client.expect[JsonResponse](GET((uri / "game").withQueryParam("guess", guess))
      .map(_.addCookie("clientId", clientId))).flatMap { response =>
      response.guessResult match {
        case Equal => printLine(s"Number is correctly guessed: $guess") *> IO.unit
        case Higher => printLine(s"Trying to guess: $guess") *>
          makeGuess(client, clientId, Random.between(guess, max), guess, max)
        case Lost => printLine(s"Number was not guessed: $guess") *> IO.unit
        case Lower => printLine(s"Trying to guess: $guess") *>
          makeGuess(client, clientId, Random.between(min, guess), min, guess)
        case _ => IO.unit
      }
    }
  }

  final case class Game(clientId: String, guess: Int, min: Int, max: Int)
}
