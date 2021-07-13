package roguelike

import indigo._
import roguelike.model.Player

object View:

  def formatStatus(player: Player): String =
    s"HP: ${Math.max(0, player.fighter.hp)}/${player.fighter.maxHp}"

  val statusLine: TextBox =
    TextBox("")
      .withColor(ColorScheme.barText.toRGBA)
      .withFontFamily(FontFamily.monospace)
      .withFontSize(Pixels(RogueLikeGame.charSize.height))
      .withSize(RogueLikeGame.screenSize * RogueLikeGame.charSize)
      .moveTo(1, -1)

  val consoleLine: TextBox =
    TextBox("> ")
      .withColor(RGBA.Green)
      .withFontFamily(FontFamily.monospace)
      .withFontSize(Pixels((RogueLikeGame.charSize.height * 2) - 4))
      .withSize(RogueLikeGame.screenSize * RogueLikeGame.charSize)
      .moveTo(2, ((RogueLikeGame.screenSize.height - 2) * RogueLikeGame.charSize.height) + 1)

  def renderBar(player: Player, totalWidth: Int, position: Point): Group =
    val height   = RogueLikeGame.charSize.height + 2
    val width    = RogueLikeGame.charSize.width * totalWidth
    val barWidth = (player.fighter.hp.toFloat / player.fighter.maxHp.toFloat * width).toInt

    Group(
      Shape.Box(Rectangle(0, 0, width, height), Fill.Color(ColorScheme.barEmpty.toRGBA)),
      Shape.Box(Rectangle(0, 0, barWidth, height), Fill.Color(ColorScheme.barFilled.toRGBA)),
      statusLine.withText(formatStatus(player))
    ).moveTo(position * RogueLikeGame.charSize.toPoint)
