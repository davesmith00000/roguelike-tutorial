package roguelike.model

import indigo._

import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent

import indigoextras.trees.QuadTree
import indigoextras.trees.QuadTree.{QuadBranch, QuadEmpty, QuadLeaf}
import indigoextras.geometry.Vertex
import indigoextras.geometry.BoundingBox

import scala.annotation.tailrec

final case class GameMap(
    size: Size,
    tileMap: QuadTree[GameTile],
    visible: List[Point],
    explored: Set[Point],
    entities: List[Entity]
):
  def entitiesList: List[Entity] =
    entities.filter(e => visible.contains(e.position)).sortBy { case e: Actor =>
      e.isAlive
    }

  def damageEntity(id: Int, damage: Int): Outcome[GameMap] =
    Outcome
      .sequence(
        entities.map {
          case e: Hostile if e.id == id && e.isAlive =>
            e.takeDamage(damage)

          case e => Outcome(e)
        }
      )
      .map(es => this.copy(entities = es))

  private def updateMap(tm: QuadTree[GameTile], coords: Point, f: GameTile => GameTile): QuadTree[GameTile] =
    val vtx = Vertex.fromPoint(coords)
    tm.fetchElementAt(vtx).map(f) match
      case None       => tm
      case Some(tile) => tm.insertElement(tile, vtx)

  def updateEntities(dice: Dice, playerPosition: Point, pause: Boolean): Outcome[GameMap] =
    val newVisible = GameMap.calculateFOV(15, playerPosition, tileMap)
    val updatedEntities =
      if !pause then
        Outcome.sequence(
          entities.map {
            case entity: Hostile =>
              entity.nextMove(dice, playerPosition, this, newVisible)

            case entity =>
              Outcome(entity)
          }
        )
      else Outcome(entities)

    updatedEntities.map { es =>
      this.copy(
        visible = newVisible,
        explored = explored ++ newVisible,
        entities = es
      )
    }

  def update(dice: Dice, playerPosition: Point, pause: Boolean): GlobalEvent => Outcome[GameMap] =
    case e: GameEvent.MoveEntity =>
      val updatedEntities =
        Outcome.sequence(entities.map(_.update(dice, playerPosition, this)(e)))

      updatedEntities.map { es =>
        this.copy(
          entities = es
        )
      }

    case _ =>
      Outcome(this)

  def visibleTiles: List[(Point, MapTile)] =
    visible
      .map(pt => lookUp(pt).map(t => (pt, t.lightMapTile)))
      .collect { case Some(mt) => mt }

  def insert(coords: Point, tile: GameTile): GameMap =
    this.copy(
      tileMap = tileMap.insertElement(tile, Vertex.fromPoint(coords))
    )

  def insert(tiles: List[(Point, GameTile)]): GameMap =
    this.copy(
      tileMap = tileMap.insertElements(tiles.map(p => (p._2, Vertex.fromPoint(p._1))))
    )
  def insert(tiles: (Point, GameTile)*): GameMap =
    insert(tiles.toList)

  def lookUp(at: Point): Option[GameTile] =
    tileMap.fetchElementAt(Vertex.fromPoint(at))

  def toExploredTiles: List[(Point, MapTile)] =
    @tailrec
    def rec(open: List[QuadTree[GameTile]], acc: List[(Point, MapTile)]): List[(Point, MapTile)] =
      open match
        case Nil =>
          acc

        case x :: xs =>
          x match {
            case _: QuadEmpty[GameTile] =>
              rec(xs, acc)

            case l: QuadLeaf[GameTile] if explored.contains(l.exactPosition.toPoint) =>
              rec(xs, (l.exactPosition.toPoint, l.value.darkMapTile) :: acc)

            case l: QuadLeaf[GameTile] =>
              rec(xs, acc)

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

  def getPathTo(dice: Dice, from: Point, to: Point, additionalBlocked: List[Point]): List[Point] =
    val area = Rectangle.fromTwoPoints(from, to).expand(2)
    val filter: GameTile => Boolean = {
      case GameTile.Ground => true
      case _               => false
    }
    val walkable = GameMap.searchByBounds(tileMap, area, filter).map(_._2).filterNot(additionalBlocked.contains)

    GameMap.getPathTo(dice, from, to, walkable, area)

object GameMap:
  def initial(size: Size, entities: List[Entity]): GameMap =
    GameMap(
      size,
      QuadTree.empty(size.width, size.height),
      Nil,
      Set(),
      entities
    )

  def gen(size: Size, dungeon: Dungeon): GameMap =
    initial(size, dungeon.entities).insert(dungeon.positionedTiles)

  def calculateFOV(radius: Int, center: Point, tileMap: QuadTree[GameTile]): List[Point] =
    val bounds: Rectangle =
      Rectangle(
        (center - radius).max(0),
        (Size(center.x, center.y) + radius).max(1)
      )

    val tiles =
      searchByBounds(tileMap, bounds, _ => true).filter(t => center.distanceTo(t._2) <= radius)

    @tailrec
    def visibleTiles(remaining: List[(GameTile, Point)], acc: List[Point]): List[Point] =
      remaining match
        case Nil =>
          acc

        case (t, pt) :: pts =>
          val lineOfSight = FOV.bresenhamLine(pt, center).dropRight(1)

          if lineOfSight.forall(pt => tiles.exists(t => t._2 == pt && !t._1.blockSight)) then
            visibleTiles(
              pts,
              pt :: acc
            )
          else visibleTiles(pts, acc)

    visibleTiles(tiles, Nil)

  def searchByBounds(
      quadTree: QuadTree[GameTile],
      bounds: Rectangle,
      filter: GameTile => Boolean
  ): List[(GameTile, Point)] =
    val boundingBox: BoundingBox = BoundingBox.fromRectangle(bounds)

    @tailrec
    def rec(remaining: List[QuadTree[GameTile]], acc: List[(GameTile, Point)]): List[(GameTile, Point)] =
      remaining match
        case Nil =>
          acc

        case x :: xs =>
          x match
            case QuadBranch(bounds, a, b, c, d) if boundingBox.overlaps(bounds) =>
              rec(a :: b :: c :: d :: xs, acc)

            case QuadLeaf(_, exactPosition, value) if boundingBox.contains(exactPosition) && filter(value) =>
              rec(xs, (value -> exactPosition.toPoint) :: acc)

            case _ =>
              rec(xs, acc)

    rec(List(quadTree), Nil)

  def getPathTo(dice: Dice, from: Point, to: Point, walkable: List[Point], area: Rectangle): List[Point] =
    PathFinder
      .fromWalkable(area.size, walkable.map(_ - area.position))
      .locatePath(dice, from - area.position, to - area.position, _ => 1)
      .map(_ + area.position)
