package roguelike.model

import indigo._

import indigoextras.trees.QuadTree

final case class Model(screenSize: Size, player: Player, gameMap: GameMap, message: String):
  def entitiesList: List[Entity] =
    player :: gameMap.entitiesList

  def moveUp: Model =
    val p = player.bump(Point(0, -1), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )
  def moveDown: Model =
    val p = player.bump(Point(0, 1), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )
  def moveLeft: Model =
    val p = player.bump(Point(-1, 0), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )
  def moveRight: Model =
    val p = player.bump(Point(1, 0), gameMap)
    this.copy(
      player = p.player,
      gameMap = gameMap.update(p.player.position),
      message = p.message
    )

object Model:
  def initial(screenSize: Size): Model =
    Model(
      screenSize,
      Player(Point.zero),
      GameMap.initial(screenSize, Nil),
      ""
    )

  def gen(dice: Dice, screenSize: Size): Model =
    val dungeon =
      DungeonGen.makeMap(
        dice,
        DungeonGen.MaxRooms,
        DungeonGen.RoomMinSize,
        DungeonGen.RoomMaxSize,
        screenSize,
        DungeonGen.MaxMonstersPerRoom
      )

    Model(
      screenSize,
      Player(dungeon.playerStart),
      GameMap.gen(screenSize, dungeon).update(dungeon.playerStart),
      ""
    )
