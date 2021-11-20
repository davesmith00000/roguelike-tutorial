package roguelike.model

import indigo._

import io.indigoengine.roguelike.starterkit.*

import indigoextras.trees.QuadTree
import indigoextras.trees.QuadTree.{QuadBranch, QuadEmpty, QuadLeaf}
import indigoextras.geometry.Vertex

import scala.annotation.tailrec
import indigoextras.geometry.BoundingBox

final case class GameMap(size: Size, tileMap: QuadTree[GameTile], visible: List[Point], explored: Set[Point], entities: List[Entity]):
  def entitiesList: List[Entity] =
    entities.filter(e => visible.contains(e.position))

  private def updateMap(tm: QuadTree[GameTile], coords: Point, f: GameTile => GameTile): QuadTree[GameTile] =
    val vtx = Vertex.fromPoint(coords)
    tm.fetchElementAt(vtx).map(f) match
      case None       => tm
      case Some(tile) => tm.insertElement(tile, vtx)

  def update(playerPosition: Point): GameMap =
    val newVisible = GameMap.calculateFOV(15, playerPosition, tileMap)
    this.copy(
      visible = newVisible,
      explored = explored ++ newVisible
    )

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
      searchByBounds(tileMap, bounds).filter(pt => center.distanceTo(pt) <= radius)

    @tailrec
    def visibleTiles(remaining: List[Point], acc: List[Point]): List[Point] =
      remaining match
        case Nil =>
          acc

        case pt :: pts =>
          val lineOfSight = bresenhamLine(pt, center)

          if lineOfSight.forall(tiles.contains) then
            visibleTiles(
              pts,
              pt :: acc
            )
          else visibleTiles(pts, acc)

    visibleTiles(tiles, Nil)

  def searchByBounds(quadTree: QuadTree[GameTile], bounds: Rectangle): List[Point] =
    val boundingBox: BoundingBox = BoundingBox.fromRectangle(bounds)

    @tailrec
    def rec(remaining: List[QuadTree[GameTile]], acc: List[Point]): List[Point] =
      remaining match
        case Nil =>
          acc

        case x :: xs =>
          x match
            case QuadBranch(bounds, a, b, c, d) if boundingBox.overlaps(bounds) =>
              rec(a :: b :: c :: d :: xs, acc)

            case QuadLeaf(_, exactPosition, value) if boundingBox.contains(exactPosition) =>
              rec(xs, exactPosition.toPoint :: acc)

            case _ =>
              rec(xs, acc)

    rec(List(quadTree), Nil)

  def bresenhamLine(from: Point, to: Point): List[Point] =
    val x0: Int = from.x
    val y0: Int = from.y
    val x1: Int = to.x
    val y1: Int = to.y
    val dx      = Math.abs(x1 - x0)
    val dy      = Math.abs(y1 - y0)
    val sx      = if x0 < x1 then 1 else -1
    val sy      = if y0 < y1 then 1 else -1

    @tailrec
    def rec(x: Int, y: Int, err: Int, acc: List[Point]): List[Point] =
      val next = Point(x, y)
      if (x == x1 && y == y1) || next.distanceTo(to) <= 1 then next :: acc
      else
        var e  = err
        var x2 = x
        var y2 = y

        if err > -dx then
          e -= dy
          x2 += sx

        if e < dy then
          e += dx
          y2 += sy

        rec(x2, y2, e, next :: acc)

    rec(x0, y0, (if dx > dy then dx else -dy) / 2, Nil)
