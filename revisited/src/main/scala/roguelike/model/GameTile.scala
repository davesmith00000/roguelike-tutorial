package roguelike.model

import indigo._

import io.indigoengine.roguelike.starterkit.*

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

sealed trait GameTile:
  def lightMapTile: MapTile
  def darkMapTile: MapTile
  def blocked: Boolean
  def blockSight: Boolean
  def isBlocked: Boolean = blocked

object GameTile:
  case object Wall extends GameTile:
    val lightMapTile: MapTile = MapTile(Tile.DARK_SHADE, RGB(0.9, 0.1, 0.1))
    val darkMapTile: MapTile  = MapTile(Tile.DARK_SHADE, RGB(0.4, 0.1, 0.1))
    val blocked: Boolean      = true
    val blockSight: Boolean   = true

  case object Ground extends GameTile:
    val lightMapTile: MapTile =
      MapTile(Tile.LIGHT_SHADE, RGB(1.0, 1.0, 0.0), RGBA(0.75, 0.6, 0.3, 1.0))
    val darkMapTile: MapTile =
      MapTile(Tile.LIGHT_SHADE, RGB(0.0, 0.4, 1.0), RGBA(0.0, 0.0, 0.5, 1.0))
    val blocked: Boolean    = false
    val blockSight: Boolean = false

  case object DownStairs extends GameTile:
    val lightMapTile: MapTile =
      MapTile(
        Tile.`>`,
        RGB.fromColorInts(255, 255, 255),
        RGBA.fromColorInts(200, 180, 50)
      )
    val darkMapTile: MapTile = MapTile(
      Tile.`>`,
      RGB.fromColorInts(0, 0, 100),
      RGBA.fromColorInts(50, 50, 150)
    )
    val blocked: Boolean    = false
    val blockSight: Boolean = false

  val scoreAs: GameTile => Int = {
    case Ground     => 1
    case DownStairs => 5
    case Wall       => Int.MaxValue
  }

  given Encoder[GameTile] = new Encoder[GameTile] {
    final def apply(data: GameTile): Json =
      data match
        case Wall =>
          Json.obj(
            ("tileType", Json.fromString("w"))
          )

        case Ground =>
          Json.obj(
            ("tileType", Json.fromString("g"))
          )

        case DownStairs =>
          Json.obj(
            ("tileType", Json.fromString("s"))
          )
  }

  given Decoder[GameTile] = new Decoder[GameTile] {
    final def apply(c: HCursor): Decoder.Result[GameTile] =
      c.downField("tileType").as[String].flatMap {
        case "w" =>
          Right(Wall)

        case "g" =>
          Right(Ground)

        case "s" =>
          Right(DownStairs)
      }
  }
