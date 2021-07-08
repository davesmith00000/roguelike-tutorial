package roguelike.model

import indigo._
import roguelike.terminal.MapTile
import roguelike.DfTiles

sealed trait Entity:
  def position: Point
  def tile: MapTile

  def moveBy(amount: Point, gameMap: GameMap): Entity
  def moveBy(x: Int, y: Int, gameMap: GameMap): Entity =
    moveBy(Point(x, y), gameMap)

final case class Player(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.`@`, RGB.Magenta)

  def moveBy(amount: Point, gameMap: GameMap): Player =
    gameMap.lookUp(position + amount) match
      case None =>
        this

      case Some(tile) if tile.isBlocked =>
        this

      case Some(tile) =>
        this.copy(position = position + amount)

  def moveUp(gameMap: GameMap): Player =
    moveBy(Point(0, -1), gameMap)
  def moveDown(gameMap: GameMap): Player =
    moveBy(Point(0, 1), gameMap)
  def moveLeft(gameMap: GameMap): Player =
    moveBy(Point(-1, 0), gameMap)
  def moveRight(gameMap: GameMap): Player =
    moveBy(Point(1, 0), gameMap)

final case class NPC(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.WHITE_SMILING_FACE, RGB.Cyan)

  def moveBy(amount: Point, gameMap: GameMap): NPC =
    this.copy(position = position + amount)
