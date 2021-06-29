package roguelike.model

import indigo._

import scala.annotation.tailrec

object DungeonGen:

  val roomMaxSize: Int = 10
  val roomMinSize: Int = 6
  val maxRooms: Int    = 30

  def createRoom(rect: Rectangle): List[(Point, GameTile)] =
    (rect.top + 1 until rect.bottom).flatMap { y =>
      (rect.left + 1 until rect.right).map { x =>
        (Point(x, y), GameTile.Ground(false))
      }
    }.toList
  def createRoom(x: Int, y: Int, width: Int, height: Int): List[(Point, GameTile)] =
    createRoom(Rectangle(x, y, width, height))

  def createHorizontalTunnel(x1: Int, x2: Int, y: Int): List[(Point, GameTile)] =
    (Math.min(x1, x2) to Math.max(x1, x2)).map { x =>
      (Point(x, y), GameTile.Ground(false))
    }.toList

  def createVerticalTunnel(y1: Int, y2: Int, x: Int): List[(Point, GameTile)] =
    (Math.min(y1, y2) to Math.max(y1, y2)).map { y =>
      (Point(x, y), GameTile.Ground(false))
    }.toList

  def makeMap(dice: Dice, maxRooms: Int, roomMinSize: Int, roomMaxSize: Int, mapSize: Size): Dungeon =
    @tailrec
    def rec(
        numOfRooms: Int,
        lastRoomCenter: Option[Point],
        rooms: List[Rectangle],
        tiles: List[(Point, GameTile)],
        playerStart: Point
    ): Dungeon =
      if numOfRooms == maxRooms then Dungeon(playerStart, tiles)
      else
        val w = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val h = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val x = dice.rollFromZero(mapSize.width - w - 1)
        val y = dice.rollFromZero(mapSize.height - h - 1)

        val newRoom = Rectangle(x, y, w, h)

        if rooms.exists(_.overlaps(newRoom)) then rec(numOfRooms + 1, lastRoomCenter, rooms, tiles, playerStart)
        else
          val roomTiles  = createRoom(newRoom)
          val roomCenter = newRoom.center

          val tunnel =
            lastRoomCenter match
              case None =>
                Nil

              case Some(prev) =>
                if dice.roll(2) == 1 then
                  createHorizontalTunnel(prev.x, roomCenter.x, prev.y) ++
                    createVerticalTunnel(prev.y, roomCenter.y, roomCenter.x)
                else
                  createVerticalTunnel(prev.y, roomCenter.y, prev.x) ++
                    createHorizontalTunnel(prev.x, roomCenter.x, roomCenter.y)

          rec(
            numOfRooms + 1,
            Option(roomCenter),
            newRoom :: rooms,
            tiles ++ roomTiles ++ tunnel,
            if numOfRooms == 0 then roomCenter else playerStart
          )

    rec(0, None, Nil, Nil, Point.zero)

final case class Dungeon(playerStart: Point, positionedTiles: List[(Point, GameTile)])
