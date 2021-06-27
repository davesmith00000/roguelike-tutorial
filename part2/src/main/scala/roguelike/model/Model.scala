package roguelike.model

import indigo._
import roguelike.terminal.MapTile
import roguelike.DfTiles

final case class Model(screen: Size, player: Player)
object Model:
  def initial(screenSize: Size): Model =
    Model(screenSize, Player.initial(screenSize))

sealed trait Entity:
  def position: Point
  def tile: MapTile

final case class Player(position: Point) extends Entity:
  val tile: MapTile = MapTile(DfTiles.Tile.`@`, RGB.Magenta)

  def moveUp: Player =
    this.copy(position = position + Point(0, -1))
  def moveDown: Player =
    this.copy(position = position + Point(0, 1))
  def moveLeft: Player =
    this.copy(position = position + Point(-1, 0))
  def moveRight: Player =
    this.copy(position = position + Point(1, 0))

object Player:
  def initial(screenSize: Size): Player =
    Player(screenSize.toPoint / 2)
