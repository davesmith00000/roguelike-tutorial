package roguelike

import indigo._
import roguelike.model.Player
import io.indigoengine.roguelike.starterkit.*
import roguelike.model.Entity
import roguelike.model.Hostile

object View:

  def formatStatus(player: Player): String =
    s"HP: ${Math.max(0, player.fighter.hp)}/${player.fighter.maxHp}"

  val statusLine: Text[TerminalText] =
    Text("", RoguelikeTiles.Size10x10.Fonts.fontKey, TerminalText(Assets.tileMap, RGB.White, RGBA.Zero))
      .moveTo(1, 2)

  val toolTipNeutral: Text[TerminalText] =
    Text("", RoguelikeTiles.Size10x10.Fonts.fontKey, TerminalText(Assets.tileMap, RGB.White, RGBA(0.3, 0.3, 0.3, 1.0)))
  val toolTipAlive: Text[TerminalText] =
    Text("", RoguelikeTiles.Size10x10.Fonts.fontKey, TerminalText(Assets.tileMap, RGB.White, RGBA.Blue))
  val toolTipDead: Text[TerminalText] =
    Text("", RoguelikeTiles.Size10x10.Fonts.fontKey, TerminalText(Assets.tileMap, RGB.White, RGBA.Red))

  def renderBar(player: Player, totalWidth: Int, position: Point): Group =
    val height   = RogueLikeGame.charSize.height + 4
    val width    = RogueLikeGame.charSize.width * totalWidth
    val barWidth = (player.fighter.hp.toFloat / player.fighter.maxHp.toFloat * width).toInt

    Group(
      Shape.Box(Rectangle(0, 0, width, height), Fill.Color(ColorScheme.barEmpty.toRGBA)),
      Shape.Box(Rectangle(0, 0, barWidth, height), Fill.Color(ColorScheme.barFilled.toRGBA)),
      statusLine.withText(formatStatus(player))
    ).moveTo(position * RogueLikeGame.charSize.toPoint)

  def renderNameHints(charSize: Size, mousePosition: Point, entities: List[Entity]): Group =
    val pos    = mousePosition / charSize.toPoint
    val offset = mousePosition + Point(10)

    Group(
      entities.filter(_.position == pos).zipWithIndex.map {
        case (entity: Hostile, row) =>
          val tt = if entity.isAlive then toolTipAlive else toolTipDead
          tt.withText(entity.name.capitalize).moveTo(Point(0, row * charSize.height) + offset)

        case (entity, row) =>
          val tt = toolTipNeutral
          tt.withText(entity.name.capitalize).moveTo(Point(0, row * charSize.height) + offset)
      }
    )
