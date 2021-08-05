package roguelike.model

import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import roguelike.terminal.TerminalEntity
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import roguelike.terminal.TerminalEmulator
import roguelike.terminal.MapTile
import roguelike.DfTiles
import roguelike.Assets
import roguelike.RogueLikeGame
import roguelike.ColorScheme

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class Message(text: String, fgColor: RGB, count: Int, stackable: Boolean):
  def fullText: String =
    if count > 1 then s"$text (x$count)"
    else text

  def withText(newText: String): Message =
    this.copy(text = newText)

  def withFgColor(newColor: RGB): Message =
    this.copy(fgColor = newColor)

  def withCount(newCount: Int): Message =
    this.copy(count = newCount)
  def increaseCount: Message =
    withCount(count + 1)
  def decreaseCount: Message =
    withCount(count - 1)

  def makeStackable: Message =
    this.copy(stackable = true)

  def unStackable: Message =
    this.copy(stackable = false)

object Message:

  import SharedCodecs.given

  given Encoder[Message] = new Encoder[Message] {
    final def apply(data: Message): Json = Json.obj(
      ("text", Json.fromString(data.text)),
      ("fgColor", data.fgColor.asJson),
      ("count", Json.fromInt(data.count)),
      ("stackable", Json.fromBoolean(data.stackable))
    )
  }

  given Decoder[Message] = new Decoder[Message] {
    final def apply(c: HCursor): Decoder.Result[Message] =
      for {
        text      <- c.downField("text").as[String]
        fgColor   <- c.downField("fgColor").as[RGB]
        count     <- c.downField("count").as[Int]
        stackable <- c.downField("stackable").as[Boolean]
      } yield Message(text, fgColor, count, stackable)
  }

  def apply(text: String, fgColor: RGB): Message =
    Message(text, fgColor, 1, true)

  val thatWayIsBlocked: Message =
    Message("That way is blocked.", ColorScheme.impossible)

  val thereIsNothingHereToPickUp: Message =
    Message("There is nothing here to pick up.", ColorScheme.impossible)

final case class MessageLog(messages: List[Message], maxLength: Option[Int]):

  def logLength: Int =
    messages.length

  def addMessage(message: Message): MessageLog =
    if message.stackable then
      this.copy(
        messages = messages.headOption
          .map { m =>
            val msgs =
              if m.text == message.text then m.increaseCount :: messages.tail
              else message :: m :: messages.tail

            maxLength match
              case None      => msgs
              case Some(max) => msgs.take(max)
          }
          .getOrElse(List(message))
      )
    else this.copy(messages = message :: messages)

  def withMaxLength(newMax: Int): MessageLog =
    this.copy(maxLength = Option(newMax))
  def noLimit: MessageLog =
    this.copy(maxLength = None)

  def toTerminal(size: Size, reversed: Boolean, startOffset: Int, fadeOut: Boolean): TerminalEmulator =
    MessageLog.logToTerminal(size, messages, reversed, startOffset, fadeOut)

  def render(position: Point, size: Size, reversed: Boolean, startOffset: Int): TerminalEntity =
    MessageLog.renderMessages(
      position * RogueLikeGame.charSize.toPoint,
      size * RogueLikeGame.charSize,
      messages,
      reversed,
      startOffset
    )

object MessageLog:

  given Encoder[MessageLog] = new Encoder[MessageLog] {
    final def apply(data: MessageLog): Json = Json.obj(
      ("messages", data.messages.asJson),
      ("maxLength", data.maxLength.asJson)
    )
  }

  given Decoder[MessageLog] = new Decoder[MessageLog] {
    final def apply(c: HCursor): Decoder.Result[MessageLog] =
      for {
        messages  <- c.downField("messages").as[List[Message]]
        maxLength <- c.downField("maxLength").as[Option[Int]]
      } yield MessageLog(messages, maxLength)
  }

  def Unlimited: MessageLog =
    MessageLog(Nil, None)

  def Limited(maxLength: Int): MessageLog =
    MessageLog(Nil, Option(maxLength))

  def logToTerminal(
      size: Size,
      messages: List[Message],
      reversed: Boolean,
      startOffset: Int,
      fadeOut: Boolean
  ): TerminalEmulator =
    val msgs = (if reversed then messages.reverse else messages).drop(startOffset)
    msgs
      .take(size.height)
      .foldLeft((TerminalEmulator(size), 0)) { case ((t, r), m) =>
        val darkenAmount = if fadeOut then (0.8 * (r.toDouble / size.height.toDouble)) + 0.2 else 0.0
        (t.putLine(Point(0, r), m.fullText, m.fgColor.mix(RGB.Black, darkenAmount), RGBA.Black), r + 1)
      }
      ._1

  def renderMessages(
      position: Point,
      size: Size,
      messages: List[Message],
      reversed: Boolean,
      startOffset: Int
  ): TerminalEntity =
    logToTerminal(size, messages, reversed, startOffset, true)
      .draw(Assets.tileMap, RogueLikeGame.charSize, MapTile(DfTiles.Tile.SPACE))
      .moveTo(position)
