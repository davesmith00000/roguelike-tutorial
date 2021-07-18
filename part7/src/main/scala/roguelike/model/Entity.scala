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
  def moveBy(amount: Point, gameMap: GameMap): Entity
  def moveBy(x: Int, y: Int, gameMap: GameMap): Entity = moveBy(Point(x, y), gameMap)
  def update(dice: Dice, playerPosition: Point, gameMap: GameMap): GameEvent => Outcome[Entity]

sealed trait Actor extends Entity:
  def isAlive: Boolean
  def fighter: Fighter

sealed trait Hostile extends Actor:
  def id: Int
  def movePath: List[Point]
  def moveTo(newPosition: Point): Hostile
  def withFighter(newFighter: Fighter): Hostile
  def markAsDead(isDead: Boolean): Hostile

  def update(dice: Dice, playerPosition: Point, gameMap: GameMap): GameEvent => Outcome[Hostile] =
    case GameEvent.MoveEntity(entityId, to) if entityId == id =>
      Outcome(moveTo(to))

    case _ =>
      Outcome(this)

  def nextMove(dice: Dice, playerPosition: Point, gameMap: GameMap, nextVisible: List[Point]): Outcome[Hostile] =
    val events =
      if isAlive && nextVisible.contains(position) then
        if playerPosition.distanceTo(position) <= 1 then List(GameEvent.MeleeAttack(name, fighter.power, None))
        else
          val entityPositions = gameMap.entities.flatMap(e => if e.blocksMovement then List(e.position) else Nil)
          val path            = gameMap.getPathTo(dice, position, playerPosition, entityPositions)

          // First path result is current location, we want the next one if it exists.
          path.drop(1).headOption match
            case Some(nextPosition) =>
              List(GameEvent.MoveEntity(id, nextPosition))

            case None =>
              Nil
      else Nil

    Outcome(this).addGlobalEvents(events)

  def takeDamage(amount: Int): Outcome[Actor] =
    val f = fighter.takeDamage(amount)
    Outcome(
      this
        .withFighter(f)
        .markAsDead(if f.hp > 0 then false else true)
    ).addGlobalEvents(if f.hp <= 0 then List(GameEvent.Log(Message(s"You killed a $name", RGB(0.75, 0, 0)))) else Nil)

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

  def heal(amount: Int): Outcome[Fighter] =
    if hp == maxHp then Outcome(this)
    else
      val newHpValue = hp + amount
      if hp + amount > maxHp then maxHp else hp + amount

      val amountRecovered = newHpValue - hp

      val events =
        if amountRecovered <= 0 then Nil
        else
          List(
            GameEvent.Log(Message(s"Player healed by ${amountRecovered.toString}!", ColorScheme.healthRecovered))
          )

      Outcome(
        this.copy(hp = newHpValue)
      ).addGlobalEvents(events)

  def takeDamage(amount: Int): Fighter =
    this.copy(hp = hp - amount)

object Fighter:
  def apply(hp: Int, defense: Int, power: Int): Fighter =
    Fighter(hp, hp, defense, power)

final case class Player(position: Point, isAlive: Boolean, fighter: Fighter) extends Actor:
  def tile: MapTile = if isAlive then MapTile(DfTiles.Tile.`@`, RGB.Magenta) else MapTile(DfTiles.Tile.`@`, RGB.Red)
  val blocksMovement: Boolean = false
  val name: String            = "Player"

  def bump(amount: Point, gameMap: GameMap): Outcome[Player] =
    gameMap.entities.collectFirst { case e: Hostile if e.position == position + amount && e.blocksMovement => e } match
      case None =>
        Outcome(moveBy(amount, gameMap))

      case Some(target) =>
        Outcome(this)
          .addGlobalEvents(GameEvent.MeleeAttack(name, fighter.power, Option(target.id)))

  def moveBy(amount: Point, gameMap: GameMap): Player =
    gameMap.lookUp(position + amount) match
      case None =>
        this

      case Some(tile) if tile.isBlocked =>
        this

      case Some(tile) =>
        this.copy(position = position + amount)

  def takeDamage(amount: Int): Player =
    val f = fighter.takeDamage(amount)
    this.copy(
      fighter = f,
      isAlive = if f.hp > 0 then true else false
    )

  def update(dice: Dice, playerPosition: Point, gameMap: GameMap): GlobalEvent => Outcome[Player] =
    _ => Outcome(this)

object Player:
  def initial(start: Point): Player =
    Player(start, true, Fighter(10, 1, 5))

final case class Orc(id: Int, position: Point, isAlive: Boolean, fighter: Fighter, movePath: List[Point])
    extends Hostile:
  def tile: MapTile =
    if isAlive then MapTile(DfTiles.Tile.`o`, RGB.fromColorInts(63, 127, 63))
    else MapTile(DfTiles.Tile.`%`, RGB(1.0, 0.6, 1.0))
  val blocksMovement: Boolean = isAlive
  val name: String            = "Orc"

  def moveBy(amount: Point, gameMap: GameMap): Orc =
    this.copy(position = position + amount)

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

  def moveBy(amount: Point, gameMap: GameMap): Troll =
    this.copy(position = position + amount)

  def moveTo(newPosition: Point): Troll =
    this.copy(position = newPosition)

  def withFighter(newFighter: Fighter): Troll =
    this.copy(fighter = newFighter)

  def markAsDead(isDead: Boolean): Troll =
    this.copy(isAlive = !isDead)

object Troll:
  def spawn(id: Int, start: Point): Troll =
    Troll(id, start, true, Fighter(2, 0, 3), Nil)
