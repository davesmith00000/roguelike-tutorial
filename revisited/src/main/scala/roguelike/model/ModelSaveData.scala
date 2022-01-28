package roguelike.model

import indigo.shared.datatypes.Size
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.parser.decode
import indigo.shared.IndigoLogger

import io.indigoengine.roguelike.starterkit.*

final case class ModelSaveData(
    screenSize: Size,
    player: Player,
    stairsPosition: Point,
    gameMap: GameMap,
    messageLog: MessageLog,
    currentFloor: Int
):
  def toJsonString: String =
    this.asJson.noSpaces

object ModelSaveData:

  def fromJsonString(json: String): Option[ModelSaveData] =
    decode[ModelSaveData](json) match
      case Left(e) =>
        IndigoLogger.debug(e.toString)
        None

      case Right(d) =>
        Option(d)

  val saveKey: String = "indigo_roguelike"

  import SharedCodecs.given

  given Encoder[ModelSaveData] = new Encoder[ModelSaveData] {
    final def apply(data: ModelSaveData): Json = Json.obj(
      ("screenSize", data.screenSize.asJson),
      ("player", data.player.asJson),
      ("stairsPosition", data.stairsPosition.asJson),
      ("gameMap", data.gameMap.asJson),
      ("messageLog", data.messageLog.asJson),
      ("currentFloor", data.currentFloor.asJson)
    )
  }

  given Decoder[ModelSaveData] = new Decoder[ModelSaveData] {
    final def apply(c: HCursor): Decoder.Result[ModelSaveData] =
      for {
        screenSize     <- c.downField("screenSize").as[Size]
        player         <- c.downField("player").as[Player]
        stairsPosition <- c.downField("stairsPosition").as[Point]
        gameMap        <- c.downField("gameMap").as[GameMap]
        messageLog     <- c.downField("messageLog").as[MessageLog]
        currentFloor   <- c.downField("currentFloor").as[Int]
      } yield ModelSaveData(
        screenSize,
        player,
        stairsPosition,
        gameMap,
        messageLog,
        currentFloor
      )
  }

object SharedCodecs:

  given Encoder[Size] = new Encoder[Size] {
    final def apply(data: Size): Json = Json.obj(
      ("width", Json.fromInt(data.width)),
      ("height", Json.fromInt(data.height))
    )
  }

  given Decoder[Size] = new Decoder[Size] {
    final def apply(c: HCursor): Decoder.Result[Size] =
      for {
        w <- c.downField("width").as[Int]
        h <- c.downField("height").as[Int]
      } yield Size(w, h)
  }

  given Encoder[Point] = new Encoder[Point] {
    final def apply(data: Point): Json = Json.obj(
      ("x", Json.fromInt(data.x)),
      ("y", Json.fromInt(data.y))
    )
  }

  given Decoder[Point] = new Decoder[Point] {
    final def apply(c: HCursor): Decoder.Result[Point] =
      for {
        x <- c.downField("x").as[Int]
        y <- c.downField("y").as[Int]
      } yield new Point(x, y)
  }

  given Encoder[RGB] = new Encoder[RGB] {
    final def apply(data: RGB): Json =
      Json.obj(
        ("r", Json.fromDoubleOrNull(data.r)),
        ("g", Json.fromDoubleOrNull(data.g)),
        ("b", Json.fromDoubleOrNull(data.b))
      )
  }

  given Decoder[RGB] = new Decoder[RGB] {
    final def apply(c: HCursor): Decoder.Result[RGB] =
      for {
        r <- c.downField("r").as[Double]
        g <- c.downField("g").as[Double]
        b <- c.downField("b").as[Double]
      } yield new RGB(r, g, b)
  }
