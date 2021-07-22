package roguelike.model

import indigo._
import roguelike.terminal.MapTile
import roguelike.DfTiles
import roguelike.utils.PathFinder
import roguelike.GameEvent
import roguelike.ColorScheme

sealed trait Entity:
  def position: Point
  def tile: MapTile
  def blocksMovement: Boolean
  def name: String

sealed trait Actor extends Entity:
  def isAlive: Boolean
  def fighter: Fighter

sealed trait Hostile extends Actor:
  def id: Int
  def movePath: List[Point]
  def moveTo(newPosition: Point): Hostile
  def withFighter(newFighter: Fighter): Hostile
  def markAsDead(isDead: Boolean): Hostile

  def takeDamage(amount: Int): Outcome[Hostile] =
    val f = fighter.takeDamage(amount)
    Outcome(
      this
        .withFighter(f)
        .markAsDead(if f.hp > 0 then false else true)
    ).addGlobalEvents(if f.hp <= 0 then List(GameEvent.Log(Message(s"You killed a $name", ColorScheme.enemyDie))) else Nil)

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

final case class Player(position: Point, isAlive: Boolean, fighter: Fighter, inventory: Inventory) extends Actor:
  def tile: MapTile = if isAlive then MapTile(DfTiles.Tile.`@`, RGB.Magenta) else MapTile(DfTiles.Tile.`@`, RGB.Red)
  val blocksMovement: Boolean = false
  val name: String            = "Player"

  def consume(itemAt: Int, visibleHostiles: List[Hostile]): Outcome[Player] =
    inventory
      .consume(itemAt, this, visibleHostiles)
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

object Player:
  def initial(start: Point): Player =
    Player(start, true, Fighter(10, 1, 5), Inventory(26, Nil))

final case class Orc(id: Int, position: Point, isAlive: Boolean, fighter: Fighter, movePath: List[Point])
    extends Hostile:
  def tile: MapTile =
    if isAlive then MapTile(DfTiles.Tile.`o`, RGB.fromColorInts(63, 127, 63))
    else MapTile(DfTiles.Tile.`%`, RGB(1.0, 0.6, 1.0))
  val blocksMovement: Boolean = isAlive
  val name: String            = "Orc"

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Orc] =
    Outcome(this.copy(position = position + amount))

  def moveTo(newPosition: Point): Orc =
    this.copy(position = newPosition)

  def withFighter(newFighter: Fighter): Orc =
    this.copy(fighter = newFighter)

  def markAsDead(isDead: Boolean): Orc =
    this.copy(isAlive = !isDead)

object Orc:
  def spawn(id: Int, start: Point): Orc =
    Orc(id, start, true, Fighter(1, 0, 2), Nil)

final case class Troll(id: Int, position: Point, isAlive: Boolean, fighter: Fighter, movePath: List[Point])
    extends Hostile:
  def tile: MapTile =
    if isAlive then MapTile(DfTiles.Tile.`T`, RGB.fromColorInts(0, 127, 0)) else MapTile(DfTiles.Tile.`%`, RGB.Magenta)
  val blocksMovement: Boolean = isAlive
  val name: String            = "Troll"

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Troll] =
    Outcome(this.copy(position = position + amount))

  def moveTo(newPosition: Point): Troll =
    this.copy(position = newPosition)

  def withFighter(newFighter: Fighter): Troll =
    this.copy(fighter = newFighter)

  def markAsDead(isDead: Boolean): Troll =
    this.copy(isAlive = !isDead)

object Troll:
  def spawn(id: Int, start: Point): Troll =
    Troll(id, start, true, Fighter(2, 0, 3), Nil)

final case class Item(position: Point, consumable: Consumable) extends Entity:
  def tile: MapTile           = consumable.tile
  val blocksMovement: Boolean = false
  def name: String            = consumable.name

  def moveTo(newPosition: Point): Item =
    this.copy(position = newPosition)
