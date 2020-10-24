package json

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.either._
import io.circe
import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.generic.JsonCodec
import io.circe.generic.extras.{Configuration, ConfiguredJsonCodec, JsonKey}
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

/**
 * HOMEWORK:
 *
 * Some classes and generated JSON codecs are provided for NBA API.
 * Unfortunately, they don't work as expected out of the box.
 * The task is to fix (rewrite) some of the codecs to make tests pass.
 * You are not supposed to change anything in _class_ HomeworkSpec,
 * instead of it you are supposed to change whatever you want inside _companion object_ for HomeworkSpec.
 *
 * It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names
 */
class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }

  "TeamTotals class instance" should {
    "deserialize correctly using Java serialization" in {
      val expected = TeamTotals("32", "1", "20")
      val serialized: Array[Byte] = Array(-84, -19, 0, 5, 115, 114, 0, 28, 106, 115, 111, 110, 46, 72, 111,
        109, 101, 119, 111, 114, 107, 83, 112, 101, 99, 36, 84, 101, 97, 109, 84, 111, 116, 97, 108, 115, -29, -52, 43,
        56, 8, -39, -60, -120, 2, 0, 3, 76, 0, 7, 97, 115, 115, 105, 115, 116, 115, 116, 0, 18, 76, 106, 97, 118, 97,
        47, 108, 97, 110, 103, 47, 83, 116, 114, 105, 110, 103, 59, 76, 0, 20, 102, 117, 108, 108, 84, 105, 109, 101,
        111, 117, 116, 82, 101, 109, 97, 105, 110, 105, 110, 103, 113, 0, 126, 0, 1, 76, 0, 9, 112, 108, 117, 115, 77,
        105, 110, 117, 115, 113, 0, 126, 0, 1, 120, 112, 116, 0, 2, 51, 50, 116, 0, 1, 49, 116, 0, 2, 50, 48 )
      val ois = new ObjectInputStream(new ByteArrayInputStream(serialized))
      val actual = ois.readObject.asInstanceOf[TeamTotals]
      ois.close()
      actual mustEqual expected
    }
  }

  "PrevMatchup class instance" should {
    "deserialize correctly using Java serialization" in {
      val expected = PrevMatchup(LocalDate.of(2019,2,15), "0031800002")
      val serialized: Array[Byte] = Array(-84, -19, 0, 5, 115, 114, 0, 29, 106, 115, 111, 110, 46, 72, 111,
        109, 101, 119, 111, 114, 107, 83, 112, 101, 99, 36, 80, 114, 101, 118, 77, 97, 116, 99, 104, 117, 112, 34, -96,
        -112, -57, -57, 33, -31, -114, 2, 0, 2, 76, 0, 8, 103, 97, 109, 101, 68, 97, 116, 101, 116, 0, 21, 76, 106, 97,
        118, 97, 47, 116, 105, 109, 101, 47, 76, 111, 99, 97, 108, 68, 97, 116, 101, 59, 76, 0, 6, 103, 97, 109, 101,
        73, 100, 116, 0, 18, 76, 106, 97, 118, 97, 47, 108, 97, 110, 103, 47, 83, 116, 114, 105, 110, 103, 59, 120, 112,
        115, 114, 0, 13, 106, 97, 118, 97, 46, 116, 105, 109, 101, 46, 83, 101, 114, -107, 93, -124, -70, 27, 34, 72,
        -78, 12, 0, 0, 120, 112, 119, 7, 3, 0, 0, 7, -29, 2, 15, 120, 116, 0, 10, 48, 48, 51, 49, 56, 48, 48, 48, 48, 50)
      val ois = new ObjectInputStream(new ByteArrayInputStream(serialized))
      val actual = ois.readObject.asInstanceOf[PrevMatchup]
      ois.close()
      actual mustEqual expected
    }
  }

}

object HomeworkSpec {

  implicit val config: Configuration = Configuration.default

  @ConfiguredJsonCodec final case class TeamTotals(assists: String, @JsonKey("full_timeout_remaining") fullTimeoutRemaining: String, plusMinus: String)
  @JsonCodec final case class TeamBoxScore(totals: TeamTotals)
  @JsonCodec final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore, activePlayers: List[Player])
  @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)

  val dateString = DateTimeFormatter.BASIC_ISO_DATE
  implicit val encodeLocalDate: Encoder[LocalDate] = Encoder.encodeString.contramap[LocalDate](_.format(dateString))
  implicit val decodeLocalDate: Decoder[LocalDate] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(LocalDate.parse(str, dateString)).leftMap(err => "LocalDate: " + err.getMessage)
  }
  @JsonCodec final case class Player(
                                    personId: String,
                                    firstName: String,
                                    lastName: String,
                                    jersey: String,
                                    teamId: String,
                                    points: String,
                                    assists: String,
                                    blocks: String,
                                    plusMinus: String,
                                    )
  @JsonCodec final case class BoxScore(
                                        basicGameData: Game,
                                        previousMatchup: PrevMatchup,
                                        stats: Option[GameStats],
                                      )
  @JsonCodec final case class JustScore(score: String)
  @JsonCodec final case class TeamStats(
                                         linescore: List[JustScore],
                                         loss: String,
                                         score: String,
                                         teamId: String,
                                         triCode: String
                                       )
  @JsonCodec final case class GameDuration(hours: String, minutes: String)
  @JsonCodec final case class Arena(
                                     city: String,
                                     country: String,
                                     isDomestic: Boolean,
                                     name: String,
                                     stateAbbr: String
                                   )
  @JsonCodec final case class Game(
                                    arena: Arena,
                                    attendance: String,
                                    endTimeUTC: Option[ZonedDateTime],
                                    gameDuration: GameDuration,
                                    gameId: String,
                                    gameUrlCode: String,
                                    hTeam: TeamStats,
                                    isBuzzerBeater: Boolean,
                                    startTimeUTC: ZonedDateTime,
                                    vTeam: TeamStats,
                                    watch: Watch
                                  )
  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  @JsonCodec final case class Watch(broadcast: TypeOfWatch)
  @JsonCodec final case class TypeOfWatch(broadcasters: GameBroadcaster)
  @JsonCodec final case class GameBroadcaster(
                                               national: List[Broadcaster],
                                               canadian: List[Broadcaster],
                                               vTeam: List[Broadcaster],
                                               hTeam: List[Broadcaster]
                                             )
  @JsonCodec final case class Broadcaster(shortName: String, longName: String)


  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json").asString.body
    decode[Scoreboard](body)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json").asString.body
    decode[BoxScore](body)
  }
}

