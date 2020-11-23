package http

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import http.Protocol.NewGame
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.headers.`Content-Type`
import org.http4s._
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.multipart.{Multipart, Part}
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s._
import org.http4s.client.Client

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
object GuessClient extends IOApp {

  private val uri = uri"http://localhost:9000"
  private def printLine(string: String = ""): IO[Unit] = IO(println(string))


  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, blocker) =>
      for {
//        _ <- client.expect[String](uri / "hello" / "world") >>= printLine
//
//        _ <- printLine(string = "Executing requests with path and query parameters:")
//        _ <- client.expect[String](uri / "params" / "2020-11-10") >>= printLine
//        _ <- client.expect[String]((uri / "params").withQueryParam(key = "date", value = "2020-11-10")) >>= printLine

        // Exercise 4. Call HTTP endpoint, implemented in scope of Exercise 1.
        // curl "localhost:9001/params/validate?timestamp=2020-11-04T14:19:54.736Z"

//        _ <- client.expect[String](Method.GET(uri / "headers", Header("Request-Header", "Request header value"))) >>= printLine

//        _ <- client.expect[String](Method.GET(uri / "cookies").map(_.addCookie("counter", "9")))

        _ <- createNewGame(client)

      } yield ()
    }.as(ExitCode.Success)

  private def createNewGame(client: Client[IO]) = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    val min = Random.between(1, 100)
    val max = Random.between(101, 200)
    val attempts = Random.between(3, 10)

    client.expect[String]((uri / "newgame").withQueryParams(Map[String, Int](
      "min" -> min,
      "max" -> max,
      "attempts" -> attempts
    ))) >>= printLine
//    client.expect[String](Method.GET(NewGame(min = min, max = max, attempts = attempts), uri / "newgame")) >>= printLine
  }

}
