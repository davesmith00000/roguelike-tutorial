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
/*
def make_map(self, max_rooms, room_min_size, room_max_size, map_width, map_height, player, entities):
    rooms = []
    num_rooms = 0

    center_of_last_room_x = None
    center_of_last_room_y = None

    for r in range(max_rooms):
        # random width and height
        w = randint(room_min_size, room_max_size)
        h = randint(room_min_size, room_max_size)
        # random position without going out of the boundaries of the map
        x = randint(0, map_width - w - 1)
        y = randint(0, map_height - h - 1)

        # "Rect" class makes rectangles easier to work with
        new_room = Rect(x, y, w, h)

        # run through the other rooms and see if they intersect with this one
        for other_room in rooms:
            if new_room.intersect(other_room):
                break
        else:
            # this means there are no intersections, so this room is valid

            # "paint" it to the map's tiles
            self.create_room(new_room)

            # center coordinates of new room, will be useful later
            (new_x, new_y) = new_room.center()

            center_of_last_room_x = new_x
            center_of_last_room_y = new_y

            if num_rooms == 0:
                # this is the first room, where the player starts at
                player.x = new_x
                player.y = new_y
            else:
                # all rooms after the first:
                # connect it to the previous room with a tunnel

                # center coordinates of previous room
                (prev_x, prev_y) = rooms[num_rooms - 1].center()

                # flip a coin (random number that is either 0 or 1)
                if randint(0, 1) == 1:
                    # first move horizontally, then vertically
                    self.create_h_tunnel(prev_x, new_x, prev_y)
                    self.create_v_tunnel(prev_y, new_y, new_x)
                else:
                    # first move vertically, then horizontally
                    self.create_v_tunnel(prev_y, new_y, prev_x)
                    self.create_h_tunnel(prev_x, new_x, new_y)

            // self.place_entities(new_room, entities)

            # finally, append the new room to the list
            rooms.append(new_room)
            num_rooms += 1
 */

final case class Dungeon(playerStart: Point, positionedTiles: List[(Point, GameTile)])
