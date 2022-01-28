package roguelike.model

import indigo._

import scala.annotation.tailrec

object DungeonGen:

  val RoomMaxSize: Int = 10
  val RoomMinSize: Int = 6
  val MaxRooms: Int    = 30

  final case class Limit(floor: Int, amount: Int)
  final case class Chance(entity: String, weight: Int)

  val maxItemsByFloor: List[Limit] = List(
    Limit(0, 1),
    Limit(3, 2)
  )

  val maxMonstersByFloor: List[Limit] = List(
    Limit(0, 2),
    Limit(3, 3),
    Limit(5, 5)
  )

  def maxItemsPerRoom(floor: Int): Int =
    maxItemsByFloor.foldLeft(2) { case (num, limit) =>
      if limit.floor <= floor then limit.amount else num
    }

  def maxMonstersPerRoom(floor: Int): Int =
    maxMonstersByFloor.foldLeft(2) { case (num, limit) =>
      if limit.floor <= floor then limit.amount else num
    }

  def itemChances: Map[Int, List[Chance]] = Map(
    0 -> List(Chance(Consumable.HealthPotion.name, 35)),
    2 -> List(Chance(Consumable.ConfusionScroll.name, 10)),
    4 -> List(
      Chance(Consumable.LightningScroll.name, 25),
      Chance(Consumable.Sword.name, 5)
    ),
    6 -> List(
      Chance(Consumable.FireBallScroll.name, 25),
      Chance(Consumable.ChainMail.name, 15)
    )
  )

  def enemyChances: Map[Int, List[Chance]] = Map(
    0 -> List(Chance(Orc.name, 80)),
    3 -> List(Chance(Troll.name, 15)),
    5 -> List(Chance(Troll.name, 30)),
    7 -> List(Chance(Troll.name, 60))
  )

  def randomChoices(
      dice: Dice,
      count: Int,
      floor: Int,
      chances: Map[Int, List[Chance]]
  ): List[String] =
    @tailrec
    def select(
        remaining: List[(Int, List[Chance])],
        acc: Map[String, Chance]
    ): List[Chance] =
      remaining match
        case Nil =>
          acc.toList.map(_._2)

        case (flr, _) :: xs if flr > floor =>
          select(Nil, acc)

        case (_, cs) :: xs =>
          select(xs, acc ++ cs.map(c => (c.entity, c)).toMap)

    val possibilities: List[Chance] = select(chances.toList, Map())

    val normalised: List[(String, Double)] =
      val total = possibilities.map(_.weight).sum

      val l = possibilities
        .map(p => (p.entity, p.weight.toDouble / total.toDouble))
        .sortBy(_._2)
        .foldLeft((0.0d, List.empty[(String, Double)])) {
          case ((total, acc), next) =>
            (total + next._2, acc :+ (next._1, total + next._2))
        }
        ._2

      l.dropRight(1) ++ l.reverse.headOption.map(e => (e._1, 1.0)).toList

    @tailrec
    def pick(remaining: List[(String, Double)], roll: Double): String =
      remaining match
        case Nil =>
          "" // shouldn't happen...

        case (name, chance) :: xs if roll <= chance =>
          name

        case _ :: xs =>
          pick(xs, roll)

    (0 until count).toList.map { _ =>
      pick(normalised, dice.rollDouble)
    }

  def placeEntities(
      floor: Int,
      entityCount: Int,
      dice: Dice,
      room: Rectangle,
      maxMonstersPerRoom: Int
  ): List[Hostile] =
    randomChoices(
      dice,
      maxMonstersPerRoom,
      floor,
      enemyChances
    ).zipWithIndex.flatMap {
      case (Orc.name, i) =>
        val x = dice.roll(room.width - 4) + room.left + 2
        val y = dice.roll(room.height - 4) + room.top + 2

        List(Orc.spawn(entityCount + i, Point(x, y)))

      case (Troll.name, i) =>
        val x = dice.roll(room.width - 4) + room.left + 2
        val y = dice.roll(room.height - 4) + room.top + 2

        List(Troll.spawn(entityCount + i, Point(x, y)))

      case _ =>
        Nil
    }.distinct

  def placeItems(
      floor: Int,
      entityCount: Int,
      dice: Dice,
      room: Rectangle,
      maxItemsPerRoom: Int,
      hostiles: List[Hostile]
  ): List[Item] =
    def spawn(consumable: Consumable): List[Item] =
      val x   = dice.roll(room.width - 4) + room.left + 2
      val y   = dice.roll(room.height - 4) + room.top + 2
      val pos = Point(x, y)

      if hostiles.contains(pos) then Nil
      else List(Item(pos, consumable))

    randomChoices(dice, maxItemsPerRoom, floor, itemChances).flatMap {
      case Consumable.HealthPotion.name =>
        spawn(Consumable.HealthPotion(4))

      case Consumable.FireBallScroll.name =>
        spawn(Consumable.FireBallScroll(12, 3))

      case Consumable.ConfusionScroll.name =>
        spawn(Consumable.ConfusionScroll(10))

      case Consumable.LightningScroll.name =>
        spawn(Consumable.LightningScroll(20, 5))

      case Consumable.Dagger.name =>
        spawn(Consumable.Dagger.create(dice))

      case Consumable.Sword.name =>
        spawn(Consumable.Sword.create(dice))

      case Consumable.LeatherArmor.name =>
        spawn(Consumable.LeatherArmor.create(dice))

      case Consumable.ChainMail.name =>
        spawn(Consumable.ChainMail.create(dice))

      case _ =>
        Nil
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
  def createRoom(
      x: Int,
      y: Int,
      width: Int,
      height: Int
  ): List[(Point, GameTile)] =
    createRoom(Rectangle(x, y, width, height))

  def createHorizontalTunnel(
      x1: Int,
      x2: Int,
      y: Int
  ): List[(Point, GameTile)] =
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
            Dungeon(
              playerStart,
              stairsPosition,
              roomTiles ++ tunnelTiles,
              hostiles,
              items
            )

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
            if numOfRooms == 0 then Nil
            else
              placeEntities(
                currentFloor,
                hostiles.length,
                dice,
                newRoom,
                maxMonstersPerRoom
              )
          val roomItems =
            if numOfRooms == 0 then Nil
            else
              placeItems(
                currentFloor,
                items.length,
                dice,
                newRoom,
                maxItemsPerRoom,
                roomHostiles
              )

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
