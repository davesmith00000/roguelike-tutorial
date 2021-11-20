package roguelike.model

import indigo._

import io.indigoengine.roguelike.starterkit.*

import roguelike.GameEvent
import roguelike.ColorScheme

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

sealed trait Entity:
  def position: Point
  def tile: MapTile
  def blocksMovement: Boolean
  def name: String

sealed trait Actor extends Entity:
  def isAlive: Boolean
  def fighter: Fighter

sealed trait Hostile extends Actor:
  val name: String
  def id: Int
  def movePath: List[Point]
  def moveTo(newPosition: Point): Hostile
  def withFighter(newFighter: Fighter): Hostile
  def markAsDead(isDead: Boolean): Hostile
  def state: HostileState
  def isConfused: Boolean
  def confuseFor(turns: Int): Outcome[Hostile]
  def nextState: Hostile
  def xpGiven: Int

  def takeDamage(amount: Int): Outcome[Hostile] =
    val f = fighter.takeDamage(amount)
    Outcome(
      this
        .withFighter(f)
        .markAsDead(if f.hp > 0 then false else true)
    ).addGlobalEvents(
      if f.hp <= 0 then
        List(
          GameEvent.Log(Message(s"You killed a $name", ColorScheme.enemyDie)),
          GameEvent.HostileGiveXP(xpGiven),
          GameEvent.Redraw
        )
      else Nil
    )

object Hostile:

  import SharedCodecs.given

  given Encoder[Hostile] = new Encoder[Hostile] {
    final def apply(data: Hostile): Json = Json.obj(
      ("name", data.name.asJson),
      ("id", data.id.asJson),
      ("position", data.position.asJson),
      ("isAlive", data.isAlive.asJson),
      ("fighter", data.fighter.asJson),
      ("movePath", data.movePath.asJson),
      ("state", data.state.asJson)
    )
  }

  given Decoder[Hostile] = new Decoder[Hostile] {
    final def apply(c: HCursor): Decoder.Result[Hostile] =
      c.downField("name").as[String].flatMap {
        case Orc.name =>
          for {
            id       <- c.downField("id").as[Int]
            position <- c.downField("position").as[Point]
            isAlive  <- c.downField("isAlive").as[Boolean]
            fighter  <- c.downField("fighter").as[Fighter]
            movePath <- c.downField("movePath").as[List[Point]]
            state    <- c.downField("state").as[HostileState]
          } yield Orc(id, position, isAlive, fighter, movePath, state)

        case Troll.name =>
          for {
            id       <- c.downField("id").as[Int]
            position <- c.downField("position").as[Point]
            isAlive  <- c.downField("isAlive").as[Boolean]
            fighter  <- c.downField("fighter").as[Fighter]
            movePath <- c.downField("movePath").as[List[Point]]
            state    <- c.downField("state").as[HostileState]
          } yield Troll(id, position, isAlive, fighter, movePath, state)
      }
  }

/** Fighter class
  * @param hp
  *   hp represents the entity’s hit points
  * @param maxHp
  *   is the maximum hp allowed
  * @param defense
  *   defense is how much taken damage will be reduced
  * @param power
  *   power is the entity’s raw attack power
  */
final case class Fighter(hp: Int, maxHp: Int, defense: Int, power: Int):
  def withHp(value: Int): Fighter =
    this.copy(hp = Math.max(0, Math.min(value, maxHp)))

  def takeDamage(amount: Int): Fighter =
    this.copy(hp = hp - amount)

  def heal(amount: Int): Fighter =
    this.copy(hp = hp + amount)

object Fighter:
  def apply(hp: Int, defense: Int, power: Int): Fighter =
    Fighter(hp, hp, defense, power)

  given Encoder[Fighter] = new Encoder[Fighter] {
    final def apply(data: Fighter): Json = Json.obj(
      ("hp", Json.fromInt(data.hp)),
      ("maxHp", Json.fromInt(data.maxHp)),
      ("defense", Json.fromInt(data.defense)),
      ("power", Json.fromInt(data.power))
    )
  }

  given Decoder[Fighter] = new Decoder[Fighter] {
    final def apply(c: HCursor): Decoder.Result[Fighter] =
      for {
        hp      <- c.downField("hp").as[Int]
        maxHp   <- c.downField("maxHp").as[Int]
        defense <- c.downField("defense").as[Int]
        power   <- c.downField("power").as[Int]
      } yield Fighter(hp, maxHp, defense, power)
  }

final case class Player(position: Point, isAlive: Boolean, fighter: Fighter, inventory: Inventory, level: Int, xp: Int)
    extends Actor:
  def tile: MapTile = if isAlive then MapTile(Tile.`@`, RGB.Magenta) else MapTile(Tile.`@`, RGB.Red)
  val blocksMovement: Boolean = false
  val name: String            = "Player"

  def consume(itemAt: Int, visibleHostiles: List[Hostile]): Outcome[Player] =
    inventory
      .consume(itemAt, this, visibleHostiles)
      .map { case (inv, p) =>
        p.copy(inventory = inv)
      }

  def consumeTargetted(itemAt: Int, target: Hostile, visibleHostiles: List[Hostile]): Outcome[Player] =
    inventory
      .consumeTargeted(itemAt, this, target, visibleHostiles)
      .map { case (inv, p) =>
        p.copy(inventory = inv)
      }

  def drop(itemAt: Int, worldItems: List[Item]): Outcome[(Player, Option[Item])] =
    if worldItems.exists(_.position == position) && inventory.items.nonEmpty then
      Outcome((this, None))
        .addGlobalEvents(GameEvent.Log(Message("Cannot drop here.", ColorScheme.invalid)))
    else
      inventory
        .drop(itemAt)
        .map {
          case (inv, None) =>
            (this.copy(inventory = inv), None)

          case (inv, Some(item)) =>
            (this.copy(inventory = inv), Option(item.moveTo(position)))

        }

  def pickUp(worldItems: List[Item]): Outcome[(Player, List[Item])] =
    worldItems.find(_.position == position) match
      case None =>
        Outcome((this, worldItems))
          .addGlobalEvents(GameEvent.Log(Message.thereIsNothingHereToPickUp))

      case Some(item) =>
        inventory.add(item).map { case (inv, accepted) =>
          if accepted then (this.copy(inventory = inv), worldItems.filterNot(_.position == position))
          else (this, worldItems)
        }

  def bump(amount: Point, gameMap: GameMap): Outcome[Player] =
    gameMap.hostiles.collectFirst { case e: Hostile if e.position == position + amount && e.blocksMovement => e } match
      case None =>
        moveBy(amount, gameMap)

      case Some(target) =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.PlayerAttack(name, fighter.power, target.id),
            GameEvent.PlayerTurnEnd
          )

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Player] =
    gameMap.lookUp(position + amount) match
      case None =>
        Outcome(this).addGlobalEvents(GameEvent.Log(Message.thatWayIsBlocked))

      case Some(tile) if tile.isBlocked =>
        Outcome(this).addGlobalEvents(GameEvent.Log(Message.thatWayIsBlocked))

      case Some(tile) =>
        Outcome(this.copy(position = position + amount))
          .addGlobalEvents(GameEvent.PlayerTurnEnd)

  def takeDamage(amount: Int): Player =
    val f = fighter.takeDamage(amount)
    this.copy(
      fighter = f,
      isAlive = if f.hp > 0 then true else false
    )

  def heal(amount: Int): Player =
    val f = fighter.heal(amount)
    this.copy(
      fighter = f
    )

  val experienceToNextLevel: Int =
    Player.LevelUpBase + level * Player.LevelUpFactor

  def addXp(additionalXp: Int): Outcome[Player] =
    if xp == 0 then Outcome(this)
    else
      val next    = xp + additionalXp
      val levelUp = next > experienceToNextLevel

      Outcome(
        this.copy(
          xp = if levelUp then next - experienceToNextLevel else next,
          level = if levelUp then level + 1 else level
        )
      ).addGlobalEvents(GameEvent.Log(Message(s"You gain $additionalXp experience points.", RGB.White)))
        .createGlobalEvents(p =>
          if levelUp then List(GameEvent.Log(Message(s"You advance to level ${p.level}!", RGB.White))) else Nil
        )

  def increaseMaxHp(amount: Int): Outcome[Player] =
    Outcome(
      this.copy(
        fighter = fighter.copy(
          hp = fighter.hp + amount,
          maxHp = fighter.maxHp + amount
        )
      )
    ).addGlobalEvents(GameEvent.Log(Message("Your health improves!", RGB.White)))

  def increasePower(amount: Int): Outcome[Player] =
    Outcome(
      this.copy(
        fighter = fighter.copy(
          power = fighter.power + amount
        )
      )
    ).addGlobalEvents(GameEvent.Log(Message("You feel stronger!", RGB.White)))

  def increaseDefense(amount: Int): Outcome[Player] =
    Outcome(
      this.copy(
        fighter = fighter.copy(
          defense = fighter.defense + amount
        )
      )
    ).addGlobalEvents(GameEvent.Log(Message("Your movements are getting swifter!", RGB.White)))

object Player:
  val LevelUpBase: Int   = 200
  val LevelUpFactor: Int = 150

  def initial(start: Point): Player =
    Player(start, true, Fighter(10, 1, 5), Inventory(26, Nil), 1, LevelUpBase)

  import SharedCodecs.given

  given Encoder[Player] = new Encoder[Player] {
    final def apply(data: Player): Json = Json.obj(
      ("position", data.position.asJson),
      ("isAlive", Json.fromBoolean(data.isAlive)),
      ("fighter", data.fighter.asJson),
      ("inventory", data.inventory.asJson),
      ("level", data.level.asJson),
      ("xp", data.xp.asJson)
    )
  }

  given Decoder[Player] = new Decoder[Player] {
    final def apply(c: HCursor): Decoder.Result[Player] =
      for {
        position  <- c.downField("position").as[Point]
        isAlive   <- c.downField("isAlive").as[Boolean]
        fighter   <- c.downField("fighter").as[Fighter]
        inventory <- c.downField("inventory").as[Inventory]
        level     <- c.downField("level").as[Int]
        xp        <- c.downField("xp").as[Int]
      } yield Player(position, isAlive, fighter, inventory, level, xp)
  }

final case class Orc(
    id: Int,
    position: Point,
    isAlive: Boolean,
    fighter: Fighter,
    movePath: List[Point],
    state: HostileState
) extends Hostile:
  def tile: MapTile =
    if isAlive then MapTile(Tile.`o`, RGB.fromColorInts(63, 127, 63))
    else MapTile(Tile.`%`, RGB(1.0, 0.6, 1.0))
  val blocksMovement: Boolean = isAlive
  val name: String            = Orc.name
  val xpGiven: Int            = 35

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Orc] =
    Outcome(this.copy(position = position + amount))

  def moveTo(newPosition: Point): Orc =
    this.copy(position = newPosition)

  def withFighter(newFighter: Fighter): Orc =
    this.copy(fighter = newFighter)

  def markAsDead(isDead: Boolean): Orc =
    this.copy(isAlive = !isDead)

  def confuseFor(turns: Int): Outcome[Orc] =
    Outcome(this.copy(state = HostileState.Confused(turns)))

  def isConfused: Boolean =
    state.isConfused

  def nextState: Orc =
    this.copy(state = state.next)

object Orc:
  val name: String = "Orc"

  def spawn(id: Int, start: Point): Orc =
    Orc(id, start, true, Fighter(1, 0, 2), Nil, HostileState.Normal)

final case class Troll(
    id: Int,
    position: Point,
    isAlive: Boolean,
    fighter: Fighter,
    movePath: List[Point],
    state: HostileState
) extends Hostile:
  def tile: MapTile =
    if isAlive then MapTile(Tile.`T`, RGB.fromColorInts(0, 127, 0)) else MapTile(Tile.`%`, RGB.Magenta)
  val blocksMovement: Boolean = isAlive
  val name: String            = Troll.name
  val xpGiven: Int            = 100

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Troll] =
    Outcome(this.copy(position = position + amount))

  def moveTo(newPosition: Point): Troll =
    this.copy(position = newPosition)

  def withFighter(newFighter: Fighter): Troll =
    this.copy(fighter = newFighter)

  def markAsDead(isDead: Boolean): Troll =
    this.copy(isAlive = !isDead)

  def confuseFor(turns: Int): Outcome[Troll] =
    Outcome(this.copy(state = HostileState.Confused(turns)))

  def isConfused: Boolean =
    state.isConfused

  def nextState: Troll =
    this.copy(state = state.next)

object Troll:
  val name: String = "Troll"

  def spawn(id: Int, start: Point): Troll =
    Troll(id, start, true, Fighter(2, 0, 3), Nil, HostileState.Normal)

final case class Item(position: Point, consumable: Consumable) extends Entity:
  def tile: MapTile           = consumable.tile
  val blocksMovement: Boolean = false
  def name: String            = consumable.name

  def moveTo(newPosition: Point): Item =
    this.copy(position = newPosition)

object Item:

  import SharedCodecs.given

  given Encoder[Item] = new Encoder[Item] {
    final def apply(data: Item): Json = Json.obj(
      ("position", data.position.asJson),
      ("consumable", data.consumable.asJson)
    )
  }

  given Decoder[Item] = new Decoder[Item] {
    final def apply(c: HCursor): Decoder.Result[Item] =
      for {
        position   <- c.downField("position").as[Point]
        consumable <- c.downField("consumable").as[Consumable]
      } yield Item(position, consumable)
  }

enum HostileState:
  case Normal extends HostileState
  case Confused(remaining: Int) extends HostileState

  def isConfused: Boolean =
    this match
      case Normal      => false
      case Confused(_) => true

  def next: HostileState =
    this match
      case Normal      => Normal
      case Confused(0) => Normal
      case Confused(i) => Confused(i - 1)

object HostileState:

  given Encoder[HostileState] = new Encoder[HostileState] {
    final def apply(data: HostileState): Json =
      data match
        case Normal =>
          Json.obj(
            ("state", Json.fromString("normal"))
          )

        case Confused(remaining) =>
          Json.obj(
            ("state", Json.fromString("confused")),
            ("remaining", Json.fromInt(remaining))
          )
  }

  given Decoder[HostileState] = new Decoder[HostileState] {
    final def apply(c: HCursor): Decoder.Result[HostileState] =
      c.downField("state").as[String].flatMap {
        case "normal" =>
          Right(Normal)

        case "confused" =>
          c.downField("remaining").as[Int].map(Confused.apply)
      }
  }
