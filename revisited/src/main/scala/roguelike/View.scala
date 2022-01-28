package roguelike

import indigo._
import roguelike.model.Player
import io.indigoengine.roguelike.starterkit.*
import roguelike.model.Entity
import roguelike.model.Hostile
import roguelike.model.GameState

object View:

  def formatStatus(player: Player): String =
    s"HP: ${Math.max(0, player.fighter.hp)}/${player.fighter.maxHp}"

  val healthStatusLine: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.tileMap, RGB.White, RGBA.Zero)
    )
      .moveTo(1, 2)

  val dungeonLevelLine: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.tileMap, RGB.White, RGBA.Zero)
    )
      .moveTo(1, 2)

  val toolTipNeutral: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.tileMap, RGB.White, RGBA(0.3, 0.3, 0.3, 1.0))
    )
  val toolTipAlive: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.tileMap, RGB.White, RGBA.Blue)
    )
  val toolTipDead: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.tileMap, RGB.White, RGBA.Red)
    )

  def renderBar(player: Player, totalWidth: Int, position: Point): Group =
    val height = RogueLikeGame.charSize.height + 4
    val width  = RogueLikeGame.charSize.width * totalWidth
    val barWidth =
      (player.fighter.hp.toFloat / player.fighter.maxHp.toFloat * width).toInt

    Group(
      Shape.Box(
        Rectangle(0, 0, width, height),
        Fill.Color(ColorScheme.barEmpty.toRGBA)
      ),
      Shape.Box(
        Rectangle(0, 0, barWidth, height),
        Fill.Color(ColorScheme.barFilled.toRGBA)
      ),
      healthStatusLine.withText(formatStatus(player))
    ).moveTo(position * RogueLikeGame.charSize.toPoint)

  def renderLevel(position: Point, currentFloor: Int): Text[TerminalText] =
    healthStatusLine
      .withText("Dungeon level: " + currentFloor.toString)
      .moveTo(position * RogueLikeGame.charSize.toPoint)

  def renderNameHints(
      charSize: Size,
      mousePosition: Point,
      entities: List[Entity],
      stairsPosition: Point
  ): Group =
    val pos    = mousePosition / charSize.toPoint
    val offset = mousePosition + Point(10)

    val tips =
      entities.filter(_.position == pos).zipWithIndex.map {
        case (entity: Hostile, row) =>
          val tt = if entity.isAlive then toolTipAlive else toolTipDead
          tt.withText(entity.name.capitalize)
            .moveTo(Point(0, row * charSize.height) + offset)

        case (entity, row) =>
          val tt = toolTipNeutral
          tt.withText(entity.name.capitalize)
            .moveTo(Point(0, row * charSize.height) + offset)
      }

    val stairs =
      if pos == stairsPosition then
        List(
          toolTipNeutral
            .withText("Down stairs.")
            .moveTo(Point(0, tips.length * charSize.height) + offset)
        )
      else Nil

    Group(tips ++ stairs)

  def renderAreaOfEffect(
      charSize: Size,
      target: Point,
      gameState: GameState
  ): Group =
    gameState match
      case GameState.LookAround(0) =>
        Group.empty

      case GameState.LookAround(radius) =>
        val pos = (target - Point(radius)) * charSize.toPoint
        val size =
          if radius * 2 % 2 == 0 then Size((radius * 2) + 1)
          else Size(radius * 2)
        Group(
          Shape
            .Box(
              Rectangle(pos, size * charSize),
              Fill.None,
              Stroke(2, RGBA.Green)
            )
        )

      case _ =>
        Group.empty
