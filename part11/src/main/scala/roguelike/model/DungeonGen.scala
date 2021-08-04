package roguelike.model

import indigo._

import scala.annotation.tailrec

object DungeonGen:

  val RoomMaxSize: Int   = 10
  val RoomMinSize: Int   = 6
  val MaxRooms: Int      = 30
  val MaxMonstersPerRoom = 2
  val MaxItemsPerRoom    = 2

  def placeEntities(entityCount: Int, dice: Dice, room: Rectangle, maxMonstersPerRoom: Int): List[Hostile] =
    (0 until dice.roll(maxMonstersPerRoom)).toList.map { i =>
      val x = dice.roll(room.width - 4) + room.left + 2
      val y = dice.roll(room.height - 4) + room.top + 2

      if dice.rollDouble < 0.8 then Orc.spawn(entityCount + i, Point(x, y))
      else Troll.spawn(entityCount + i, Point(x, y))

    }.distinct

  def placeItems(
      entityCount: Int,
      dice: Dice,
      room: Rectangle,
      maxItemsPerRoom: Int,
      hostiles: List[Hostile]
  ): List[Item] =
    (0 until dice.roll(maxItemsPerRoom)).toList.flatMap { i =>
      val x   = dice.roll(room.width - 4) + room.left + 2
      val y   = dice.roll(room.height - 4) + room.top + 2
      val pos = Point(x, y)

      val itemChance = dice.rollDouble

      if hostiles.contains(pos) then Nil
      else if itemChance < 0.7 then List(Item(pos, Consumable.HealthPotion(4)))
      else if itemChance < 0.8 then List(Item(pos, Consumable.FireBallScroll(12, 3)))
      else if itemChance < 0.9 then List(Item(pos, Consumable.ConfusionScroll(10)))
      else List(Item(pos, Consumable.LightningScroll(20, 5)))

    }.distinct

  def createRoom(rect: Rectangle): List[(Point, GameTile)] =
    (rect.top + 1 until rect.bottom).flatMap { y =>
      (rect.left + 1 until rect.right).map { x =>
        val tile =
          if x == rect.left + 1 || x == rect.right - 1 then GameTile.Wall
          else if y == rect.top + 1 || y == rect.bottom - 1 then GameTile.Wall
          else GameTile.Ground

        (Point(x, y), tile)
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
      maxMonstersPerRoom: Int,
      maxItemsPerRoom: Int,
      currentFloor: Int
  ): Dungeon =
    @tailrec
    def rec(
        numOfRooms: Int,
        lastRoomCenter: Option[Point],
        rooms: List[Rectangle],
        roomTiles: List[(Point, GameTile)],
        tunnelTiles: List[(Point, GameTile)],
        hostiles: List[Hostile],
        items: List[Item],
        playerStart: Point,
        stairsPosition: Point
    ): Dungeon =
      if numOfRooms == maxRooms then
        lastRoomCenter match
          case None =>
            Dungeon(playerStart, stairsPosition, roomTiles ++ tunnelTiles, hostiles, items)

          case Some(center) =>
            Dungeon(
              playerStart,
              center,
              roomTiles ++ tunnelTiles ++ List((center, GameTile.DownStairs)),
              hostiles,
              items
            )
      else
        val w = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val h = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val x = dice.rollFromZero(mapSize.width - w - 1)
        val y = dice.rollFromZero(mapSize.height - h - 1)

        val newRoom = Rectangle(x, y, w, h)

        if rooms.exists(_.overlaps(newRoom)) then
          rec(
            numOfRooms + 1,
            lastRoomCenter,
            rooms,
            roomTiles,
            tunnelTiles,
            hostiles,
            items,
            playerStart,
            stairsPosition
          )
        else
          val newRoomTiles = createRoom(newRoom)
          val roomCenter   = newRoom.center
          val roomHostiles =
            if numOfRooms == 0 then Nil else placeEntities(hostiles.length, dice, newRoom, maxMonstersPerRoom)
          val roomItems =
            if numOfRooms == 0 then Nil else placeItems(items.length, dice, newRoom, maxItemsPerRoom, roomHostiles)

          val newTunnelTiles =
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
            roomTiles ++ newRoomTiles,
            tunnelTiles ++ newTunnelTiles,
            hostiles ++ roomHostiles,
            items ++ roomItems,
            if numOfRooms == 0 then roomCenter else playerStart,
            stairsPosition
          )

    rec(0, None, Nil, Nil, Nil, Nil, Nil, Point.zero, Point.zero)

final case class Dungeon(
    playerStart: Point,
    stairsPosition: Point,
    positionedTiles: List[(Point, GameTile)],
    hostiles: List[Hostile],
    items: List[Item]
)
