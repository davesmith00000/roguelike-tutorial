package roguelike.model

import indigo._

import indigoextras.trees.QuadTree

import io.indigoengine.roguelike.starterkit.*

final case class Model(screenSize: Size, player: Player, entities: List[Entity], gameMap: GameMap):
  def entitiesList: List[Entity] =
    player :: entities.filter(e => gameMap.visible.contains(e.position))

  def moveUp: Model =
    val p = player.moveUp(gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )
  def moveDown: Model =
    val p = player.moveDown(gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )
  def moveLeft: Model =
    val p = player.moveLeft(gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )
  def moveRight: Model =
    val p = player.moveRight(gameMap)
    this.copy(
      player = p,
      gameMap = gameMap.update(p.position)
    )

object Model:
  def initial(screenSize: Size): Model =
    Model(
      screenSize,
      Player(Point.zero),
      Nil,
      GameMap.initial(screenSize)
    )

  def gen(dice: Dice, screenSize: Size): Model =
    val dungeon =
      DungeonGen.makeMap(dice, 30, 6, 10, screenSize)

    Model(
      screenSize,
      Player(dungeon.playerStart),
      List(
        NPC((screenSize.toPoint / 2) + Point(-5))
      ),
      GameMap.gen(screenSize, dungeon).update(dungeon.playerStart)
    )
