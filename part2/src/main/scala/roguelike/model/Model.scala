package roguelike.model

import indigo._

import indigoextras.trees.QuadTree
import indigoextras.trees.QuadTree.{QuadBranch, QuadEmpty, QuadLeaf}
import indigoextras.geometry.Vertex
import scala.annotation.tailrec

import io.indigoengine.roguelike.starterkit.*

final case class Model(screen: Size, player: Player, entities: List[Entity], gameMap: GameMap):
  def entitiesList: List[Entity] =
    player :: entities
object Model:
  def initial(screenSize: Size): Model =
    Model(
      screenSize,
      Player.initial(screenSize),
      List(
        NPC((screenSize.toPoint / 2) + Point(-5))
      ),
      GameMap.initial(screenSize)
    )

sealed trait Entity:
  def position: Point
  def tile: MapTile

  def moveBy(amount: Point, gameMap: GameMap): Entity
  def moveBy(x: Int, y: Int, gameMap: GameMap): Entity =
    moveBy(Point(x, y), gameMap)

final case class Player(position: Point) extends Entity:
  val tile: MapTile = MapTile(Tile.`@`, RGB.Magenta)

  def moveBy(amount: Point, gameMap: GameMap): Player =
    gameMap.lookUp(position + amount) match
      case None =>
        this.copy(position = position + amount)

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

object Player:
  def initial(screenSize: Size): Player =
    Player(screenSize.toPoint / 2)

final case class NPC(position: Point) extends Entity:
  val tile: MapTile = MapTile(Tile.WHITE_SMILING_FACE, RGB.Cyan)

  def moveBy(amount: Point, gameMap: GameMap): NPC =
    this.copy(position = position + amount)

sealed trait GameTile:
  def mapTile: MapTile
  def blocked: Boolean
  def blockSight: Boolean
  def isBlocked: Boolean = blocked

object GameTile:
  case object DarkWall extends GameTile:
    val mapTile: MapTile    = MapTile(Tile.DARK_SHADE, RGB(0.9, 0, 0.0))
    val blocked: Boolean    = true
    val blockSight: Boolean = true

  case object Ground extends GameTile:
    val mapTile: MapTile    = MapTile(Tile.LIGHT_SHADE, RGB(0.1, 0.1, 0.4))
    val blocked: Boolean    = false
    val blockSight: Boolean = false

final case class GameMap(size: Size, tileMap: QuadTree[GameTile]):
  def insert(coords: Point, tile: GameTile): GameMap =
    this.copy(
      tileMap = tileMap.insertElement(tile, Vertex.fromPoint(coords))
    )

  def lookUp(at: Point): Option[GameTile] =
    tileMap.fetchElementAt(Vertex.fromPoint(at))

  def toPositionedTiles: List[(Point, MapTile)] =
    @tailrec
    def rec(open: List[QuadTree[GameTile]], acc: List[(Point, MapTile)]): List[(Point, MapTile)] =
      open match
        case Nil =>
          acc

        case x :: xs =>
          x match {
            case _: QuadEmpty[GameTile] =>
              rec(xs, acc)

            case l: QuadLeaf[GameTile] =>
              rec(xs, (l.exactPosition.toPoint, l.value.mapTile) :: acc)

            case b: QuadBranch[GameTile] if b.isEmpty =>
              rec(xs, acc)

            case QuadBranch(_, a, b, c, d) =>
              val next =
                (if a.isEmpty then Nil else List(a)) ++
                  (if b.isEmpty then Nil else List(b)) ++
                  (if c.isEmpty then Nil else List(c)) ++
                  (if d.isEmpty then Nil else List(d))

              rec(xs ++ next, acc)
          }

    rec(List(tileMap), Nil)

object GameMap:
  def initial(size: Size): GameMap =
    GameMap(
      size,
      QuadTree.empty(size.width, size.height)
    ).insert(Point(30, 22), GameTile.DarkWall)
      .insert(Point(31, 22), GameTile.DarkWall)
      .insert(Point(32, 22), GameTile.DarkWall)
