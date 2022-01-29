package roguelike.model

import indigo._
import indigoextras.geometry.BoundingBox
import indigoextras.geometry.Vertex
import indigoextras.trees.QuadTree
import indigoextras.trees.QuadTree.QuadBranch
import indigoextras.trees.QuadTree.QuadEmpty
import indigoextras.trees.QuadTree.QuadLeaf
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe._
import io.circe.syntax._
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent

import scala.annotation.tailrec

final case class GameMap(
    size: Size,
    tileMap: QuadTree[GameTile],
    visible: List[Point],
    explored: Set[Point],
    hostiles: List[Hostile],
    items: List[Item]
):
  def entitiesList: List[Entity] =
    (items ++ hostiles.sortBy { case e: Actor =>
      e.isAlive
    }).filter(e => visible.contains(e.position))

  def visibleHostiles: List[Hostile] =
    hostiles.filter(e => e.isAlive && visible.contains(e.position))

  def damageHostile(id: Int, damage: Int): Outcome[GameMap] =
    Outcome
      .sequence(
        hostiles.map {
          case e if e.id == id && e.isAlive =>
            e.takeDamage(damage)

          case e => Outcome(e)
        }
      )
      .map(es => this.copy(hostiles = es))

  def confuseHostile(id: Int, numberOfTurns: Int): Outcome[GameMap] =
    Outcome
      .sequence(
        hostiles.map {
          case e if e.id == id && e.isAlive =>
            e.confuseFor(numberOfTurns)

          case e => Outcome(e)
        }
      )
      .map(es => this.copy(hostiles = es))

  private def updateMap(
      tm: QuadTree[GameTile],
      coords: Point,
      f: GameTile => GameTile
  ): QuadTree[GameTile] =
    val vtx = Vertex.fromPoint(coords)
    tm.fetchElementAt(vtx).map(f) match
      case None       => tm
      case Some(tile) => tm.insertElement(tile, vtx)

  def updateHostiles(
      dice: Dice,
      playerPosition: Point,
      pause: Boolean
  ): Outcome[GameMap] =
    val newVisible = GameMap.calculateFOV(15, playerPosition, tileMap)

    @tailrec
    def rec(
        remaining: List[Hostile],
        events: List[GameEvent],
        acc: List[Hostile]
    ): Outcome[List[Hostile]] =
      remaining match
        case Nil =>
          Outcome(acc).addGlobalEvents(events)

        case x :: xs if !x.isAlive || !newVisible.contains(x.position) =>
          // Filter out the dead and the unseen
          rec(xs, events, x :: acc)

        case x :: xs if playerPosition.distanceTo(x.position) <= 1 =>
          // Close enough to attack!
          val event = GameEvent.HostileMeleeAttack(x.name, x.fighter.power)
          rec(xs, event :: events, x :: acc)

        case x :: xs if x.isConfused =>
          // Is confused!
          val randomMove = getRandomDirection(dice, x.position)
          rec(xs, events, x.nextState.moveTo(randomMove) :: acc)

        case x :: xs =>
          // Otherwise, move a little closer...
          val entityPositions = (xs ++ acc).flatMap(e =>
            if e.blocksMovement then List(e.position) else Nil
          )
          val path =
            getPathTo(dice, x.position, playerPosition, entityPositions)

          // First path result is current location, we want the next one if it exists.
          path.drop(1).headOption match
            case Some(nextPosition) =>
              rec(xs, events, x.moveTo(nextPosition) :: acc)

            case None =>
              rec(xs, events, x :: acc)

    val updatedEntities =
      if !pause then rec(hostiles, Nil, Nil)
      else Outcome(hostiles)

    updatedEntities.map { es =>
      this.copy(
        visible = newVisible,
        explored = explored ++ newVisible,
        hostiles = es
      )
    }

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
      tileMap =
        tileMap.insertElements(tiles.map(p => (p._2, Vertex.fromPoint(p._1))))
    )
  def insert(tiles: (Point, GameTile)*): GameMap =
    insert(tiles.toList)

  def lookUp(at: Point): Option[GameTile] =
    tileMap.fetchElementAt(Vertex.fromPoint(at))

  def dropItem(item: Item): GameMap =
    this.copy(
      items = item :: items
    )

  def toExploredTiles: List[(Point, MapTile)] =
    @tailrec
    def rec(
        open: List[QuadTree[GameTile]],
        acc: List[(Point, MapTile)]
    ): List[(Point, MapTile)] =
      open match
        case Nil =>
          acc

        case x :: xs =>
          x match {
            case _: QuadEmpty[GameTile] =>
              rec(xs, acc)

            case l: QuadLeaf[GameTile]
                if explored.contains(l.exactPosition.toPoint) =>
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

  def toPositionedTiles: List[(Point, GameTile)] =
    @tailrec
    def rec(
        open: List[QuadTree[GameTile]],
        acc: List[(Point, GameTile)]
    ): List[(Point, GameTile)] =
      open match
        case Nil =>
          acc

        case x :: xs =>
          x match {
            case _: QuadEmpty[GameTile] =>
              rec(xs, acc)

            case l: QuadLeaf[GameTile] =>
              rec(xs, (l.exactPosition.toPoint, l.value) :: acc)

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

  def getPathTo(
      dice: Dice,
      from: Point,
      to: Point,
      additionalBlocked: List[Point]
  ): List[Point] =
    val area = Rectangle.fromTwoPoints(from, to).expand(2)
    val filter: GameTile => Boolean = {
      case GameTile.Ground     => true
      case GameTile.DownStairs => true
      case _                   => false
    }
    val walkable = GameMap
      .searchByBounds(tileMap, area, filter)
      .map(_._2)
      .filterNot(additionalBlocked.contains)

    GameMap.getPathTo(dice, from, to, walkable, area)

  def getRandomDirection(dice: Dice, position: Point): Point =
    val up    = Point(0, -1)
    val down  = Point(0, 1)
    val left  = Point(-1, 0)
    val right = Point(1, 0)

    val positions =
      List(
        position + up,
        position + down,
        position + left,
        position + right
      ).filter { pt =>
        tileMap.fetchElementAt(Vertex.fromPoint(pt)) match
          case None =>
            false

          case Some(t) if t.blocked =>
            false

          case _ =>
            true
      }

    if positions.isEmpty then position
    else positions(dice.rollFromZero(positions.length - 1))

object GameMap:

  import SharedCodecs.given

  given Encoder[GameMap] = new Encoder[GameMap] {
    final def apply(data: GameMap): Json = Json.obj(
      ("size", data.size.asJson),
      ("tiles", data.toPositionedTiles.asJson),
      ("visible", data.visible.asJson),
      ("explored", data.explored.asJson),
      ("hostiles", data.hostiles.asJson),
      ("items", data.items.asJson)
    )
  }

  given Decoder[GameMap] = new Decoder[GameMap] {
    final def apply(c: HCursor): Decoder.Result[GameMap] =
      for {
        size     <- c.downField("size").as[Size]
        tiles    <- c.downField("tiles").as[List[(Point, GameTile)]]
        visible  <- c.downField("visible").as[List[Point]]
        explored <- c.downField("explored").as[Set[Point]]
        hostiles <- c.downField("hostiles").as[List[Hostile]]
        items    <- c.downField("items").as[List[Item]]
      } yield GameMap(
        size,
        QuadTree.empty(size.width, size.height),
        visible,
        explored,
        hostiles,
        items
      ).insert(tiles)
  }

  def initial(size: Size, hostiles: List[Hostile], items: List[Item]): GameMap =
    GameMap(
      size,
      QuadTree.empty(size.width, size.height),
      Nil,
      Set(),
      hostiles,
      items
    )

  def gen(size: Size, dungeon: Dungeon): GameMap =
    initial(size, dungeon.hostiles, dungeon.items).insert(
      dungeon.positionedTiles
    )

  def calculateFOV(
      radius: Int,
      center: Point,
      tileMap: QuadTree[GameTile]
  ): List[Point] =
    val bounds: Rectangle =
      Rectangle(
        (center - radius).max(0),
        (Size(center.x, center.y) + radius).max(1)
      )

    val tiles =
      searchByBounds(tileMap, bounds, _ => true).filter(t =>
        center.distanceTo(t._2) <= radius
      )

    @tailrec
    def visibleTiles(
        remaining: List[(GameTile, Point)],
        acc: List[Point]
    ): List[Point] =
      remaining match
        case Nil =>
          acc

        case (t, pt) :: pts =>
          val lineOfSight = FOV.bresenhamLine(pt, center).dropRight(1)

          if lineOfSight.forall(pt =>
              tiles.exists(t => t._2 == pt && !t._1.blockSight)
            )
          then
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
    def rec(
        remaining: List[QuadTree[GameTile]],
        acc: List[(GameTile, Point)]
    ): List[(GameTile, Point)] =
      remaining match
        case Nil =>
          acc

        case x :: xs =>
          x match
            case QuadBranch(bounds, a, b, c, d)
                if boundingBox.overlaps(bounds) =>
              rec(a :: b :: c :: d :: xs, acc)

            case QuadLeaf(_, exactPosition, value)
                if boundingBox.contains(exactPosition) && filter(value) =>
              rec(xs, (value -> exactPosition.toPoint) :: acc)

            case _ =>
              rec(xs, acc)

    rec(List(quadTree), Nil)

  def getPathTo(
      dice: Dice,
      from: Point,
      to: Point,
      walkable: List[Point],
      area: Rectangle
  ): List[Point] =
    PathFinder
      .fromWalkable(area.size, walkable.map(_ - area.position))
      .locatePath(dice, from - area.position, to - area.position, _ => 1)
      .map(_ + area.position)
