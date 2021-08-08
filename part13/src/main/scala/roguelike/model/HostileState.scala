package roguelike.model

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

enum HostileState:
  case Normal extends HostileState
  case Confused(remaining: Int) extends HostileState

  def isConfused: Boolean =
    this match
      case Normal      => false
      case Confused(_) => true

  def next: HostileState =
    this match
      case Normal      => Normal
      case Confused(0) => Normal
      case Confused(i) => Confused(i - 1)

object HostileState:

  given Encoder[HostileState] = new Encoder[HostileState] {
    final def apply(data: HostileState): Json =
      data match
        case Normal =>
          Json.obj(
            ("state", Json.fromString("normal"))
          )

        case Confused(remaining) =>
          Json.obj(
            ("state", Json.fromString("confused")),
            ("remaining", Json.fromInt(remaining))
          )
  }

  given Decoder[HostileState] = new Decoder[HostileState] {
    final def apply(c: HCursor): Decoder.Result[HostileState] =
      c.downField("state").as[String].flatMap {
        case "normal" =>
          Right(Normal)

        case "confused" =>
          c.downField("remaining").as[Int].map(Confused.apply)
      }
  }
