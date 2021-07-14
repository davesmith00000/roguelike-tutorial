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
  def apply(text: String, fgColor: RGB): Message =
    Message(text, fgColor, 1, true)

final case class MessageLog(messages: List[Message], maxLength: Int):

  def addMessage(message: Message): MessageLog =
    if message.stackable then
      this.copy(
        messages = messages.headOption
          .map { m =>
            val msgs =
              if m.text == message.text then m.increaseCount :: messages.tail
              else message :: m :: messages.tail

            msgs.take(maxLength)
          }
          .getOrElse(List(message))
      )
    else this.copy(messages = message :: messages)

  def toTerminal(size: Size): TerminalEmulator =
    MessageLog.logToTerminal(size, messages)

  def render(position: Point, size: Size): TerminalEntity =
    MessageLog.renderMessages(position * RogueLikeGame.charSize.toPoint, size * RogueLikeGame.charSize, messages)

object MessageLog:
  def apply(maxLength: Int): MessageLog =
    MessageLog(Nil, maxLength)

  def logToTerminal(size: Size, messages: List[Message]): TerminalEmulator =
    messages
      .take(size.height)
      .foldLeft((TerminalEmulator(size), 0)) { case ((t, r), m) =>
        val darkenAmount = (0.8 * (r.toDouble / size.height.toDouble)) + 0.2
        (t.putLine(Point(0, r), m.fullText, m.fgColor.mix(RGB.Black, darkenAmount), RGBA.Black), r + 1)
      }
      ._1

  def renderMessages(position: Point, size: Size, messages: List[Message]): TerminalEntity =
    logToTerminal(size, messages)
      .draw(Assets.tileMap, RogueLikeGame.charSize, MapTile(DfTiles.Tile.SPACE))
      .moveTo(position)
