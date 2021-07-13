package roguelike

import indigo._
import roguelike.model.Player
import roguelike.terminal.TerminalText

object View:

  def formatStatus(player: Player): String =
    s"HP: ${Math.max(0, player.fighter.hp)}/${player.fighter.maxHp}"

  val statusLine: Text[TerminalText] =
    Text("", DfTiles.Fonts.fontKey, TerminalText(Assets.tileMap, RGB.White, RGBA.Zero))
    .moveTo(1, 2)

  val consoleLine: TextBox =
    TextBox("> ")
      .withColor(RGBA.Green)
      .withFontFamily(FontFamily.monospace)
      .withFontSize(Pixels(RogueLikeGame.charSize.height))
      .withSize((RogueLikeGame.screenSize - 21) * RogueLikeGame.charSize)
      .moveTo(Point(21, 45) * RogueLikeGame.charSize.toPoint)

  def renderBar(player: Player, totalWidth: Int, position: Point): Group =
    val height   = RogueLikeGame.charSize.height + 3
    val width    = RogueLikeGame.charSize.width * totalWidth
    val barWidth = (player.fighter.hp.toFloat / player.fighter.maxHp.toFloat * width).toInt

    Group(
      Shape.Box(Rectangle(0, 0, width, height), Fill.Color(ColorScheme.barEmpty.toRGBA)),
      Shape.Box(Rectangle(0, 0, barWidth, height), Fill.Color(ColorScheme.barFilled.toRGBA)),
      statusLine.withText(formatStatus(player))
    ).moveTo(position * RogueLikeGame.charSize.toPoint)
