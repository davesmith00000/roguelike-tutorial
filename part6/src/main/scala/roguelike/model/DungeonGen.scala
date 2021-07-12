package roguelike.model

import indigo._

import scala.annotation.tailrec

object DungeonGen:

  val RoomMaxSize: Int   = 10
  val RoomMinSize: Int   = 6
  val MaxRooms: Int      = 30
  val MaxMonstersPerRoom = 2

  def placeEntities(entityCount: Int, dice: Dice, room: Rectangle, maxMonstersPerRoom: Int): List[Entity] =
    (0 until dice.roll(maxMonstersPerRoom)).toList.map { i =>
      val x = dice.roll(room.width - 2) + room.left + 1
      val y = dice.roll(room.height - 2) + room.top + 1

      if dice.rollDouble < 0.8 then Orc.spawn(entityCount + i, Point(x, y))
      else Troll.spawn(entityCount + i, Point(x, y))

    }.distinct

  def createRoom(rect: Rectangle): List[(Point, GameTile)] =
    (rect.top + 1 until rect.bottom).flatMap { y =>
      (rect.left + 1 until rect.right).map { x =>
        (Point(x, y), GameTile.Ground)
      }
    }.toList
  def createRoom(x: Int, y: Int, width: Int, height: Int): List[(Point, GameTile)] =
    createRoom(Rectangle(x, y, width, height))

  def createHorizontalTunnel(x1: Int, x2: Int, y: Int): List[(Point, GameTile)] =
    (Math.min(x1, x2) to Math.max(x1, x2)).map { x =>
      (Point(x, y), GameTile.Ground)
    }.toList

  def createVerticalTunnel(y1: Int, y2: Int, x: Int): List[(Point, GameTile)] =
    (Math.min(y1, y2) to Math.max(y1, y2)).map { y =>
      (Point(x, y), GameTile.Ground)
    }.toList

  def makeMap(
      dice: Dice,
      maxRooms: Int,
      roomMinSize: Int,
      roomMaxSize: Int,
      mapSize: Size,
      maxMonstersPerRoom: Int
  ): Dungeon =
    @tailrec
    def rec(
        numOfRooms: Int,
        lastRoomCenter: Option[Point],
        rooms: List[Rectangle],
        tiles: List[(Point, GameTile)],
        entities: List[Entity],
        playerStart: Point
    ): Dungeon =
      if numOfRooms == maxRooms then Dungeon(playerStart, tiles, entities)
      else
        val w = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val h = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val x = dice.rollFromZero(mapSize.width - w - 1)
        val y = dice.rollFromZero(mapSize.height - h - 1)

        val newRoom = Rectangle(x, y, w, h)

        if rooms.exists(_.overlaps(newRoom)) then
          rec(numOfRooms + 1, lastRoomCenter, rooms, tiles, entities, playerStart)
        else
          val roomTiles    = createRoom(newRoom)
          val roomCenter   = newRoom.center
          val roomEntities = if numOfRooms == 0 then Nil else placeEntities(entities.length, dice, newRoom, MaxMonstersPerRoom)

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
            entities ++ roomEntities,
            if numOfRooms == 0 then roomCenter else playerStart
          )

    rec(0, None, Nil, Nil, Nil, Point.zero)

final case class Dungeon(playerStart: Point, positionedTiles: List[(Point, GameTile)], entities: List[Entity])
