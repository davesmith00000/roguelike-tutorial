package roguelike.model

import indigo._
import roguelike.terminal.MapTile
import roguelike.DfTiles
import roguelike.utils.PathFinder

sealed trait Entity:
  def position: Point
  def tile: MapTile
  def blocksMovement: Boolean
  def name: String

  def moveBy(amount: Point, gameMap: GameMap): Entity
  def moveBy(x: Int, y: Int, gameMap: GameMap): Entity =
    moveBy(Point(x, y), gameMap)

  def update(dice: Dice, playerPosition: Point, gameMap: GameMap): GlobalEvent => Outcome[Entity]

sealed trait Actor extends Entity:
  def isAlive: Boolean
  def fighter: Fighter
  def takeDamage(amount: Int): Actor

sealed trait Hostile extends Actor:
  def id: Int
  def movePath: List[Point]
  def moveTo(newPosition: Point): Hostile
object Hostile:
  def nextMove(entity: Hostile, dice: Dice, playerPosition: Point, gameMap: GameMap): List[GlobalEvent] =
    if gameMap.visible.contains(entity.position) then
      if playerPosition.distanceTo(entity.position) <= 1 then List(MeleeAttack(entity.name, entity.fighter.power, None))
      else
        val entityPositions = gameMap.entities.flatMap(e => if e.blocksMovement then List(e.position) else Nil)
        gameMap.getPathTo(dice, entity.position, playerPosition, entityPositions) match
          // start/current :: next :: remaining
          case _ :: nextPosition :: _ =>
            List(MoveEntity(entity.id, nextPosition))

          case _ =>
            Nil
    else Nil

final case class Player(position: Point, isAlive: Boolean, fighter: Fighter) extends Actor:
  def tile: MapTile = if isAlive then MapTile(DfTiles.Tile.`@`, RGB.Magenta) else MapTile(DfTiles.Tile.`@`, RGB.Red)
  val blocksMovement: Boolean = false
  val name: String            = "Player"

  def bump(amount: Point, gameMap: GameMap): Outcome[Player] =
    gameMap.entities.collectFirst { case e: Hostile if e.position == position + amount && e.blocksMovement => e } match
      case None         => Outcome(moveBy(amount, gameMap))
      case Some(target) => Outcome(this).addGlobalEvents(MeleeAttack(name, fighter.power, Option(target.id)))

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
    if isAlive then MapTile(DfTiles.Tile.`o`, RGB.fromColorInts(63, 127, 63)) else MapTile(DfTiles.Tile.`o`, RGB.Red)
  val blocksMovement: Boolean = isAlive
  val name: String            = "Orc"

  def moveBy(amount: Point, gameMap: GameMap): Orc =
    this.copy(position = position + amount)

  def moveTo(newPosition: Point): Orc =
    this.copy(position = newPosition)

  def update(dice: Dice, playerPosition: Point, gameMap: GameMap): GlobalEvent => Outcome[Orc] =
    case UpdateEntities if isAlive =>
      Outcome(this).addGlobalEvents(Hostile.nextMove(this, dice, playerPosition, gameMap))

    case MoveEntity(entityId, to)
        if entityId == id && !gameMap.entities.exists(e => e.blocksMovement && e.position == to) =>
      Outcome(moveTo(to))

    case _ =>
      Outcome(this)

  def takeDamage(amount: Int): Orc =
    val f = fighter.takeDamage(amount)
    this.copy(
      fighter = f,
      isAlive = if f.hp > 0 then true else false
    )

object Orc:
  def spawn(id: Int, start: Point): Orc =
    Orc(id, start, true, Fighter(1, 0, 2), Nil)

final case class Troll(id: Int, position: Point, isAlive: Boolean, fighter: Fighter, movePath: List[Point])
    extends Hostile:
  def tile: MapTile =
    if isAlive then MapTile(DfTiles.Tile.`T`, RGB.fromColorInts(0, 127, 0)) else MapTile(DfTiles.Tile.`T`, RGB.Red)
  val blocksMovement: Boolean = isAlive
  val name: String            = "Troll"

  def moveBy(amount: Point, gameMap: GameMap): Troll =
    this.copy(position = position + amount)

  def moveTo(newPosition: Point): Troll =
    this.copy(position = newPosition)

  def update(dice: Dice, playerPosition: Point, gameMap: GameMap): GlobalEvent => Outcome[Troll] =
    case UpdateEntities if isAlive =>
      Outcome(this).addGlobalEvents(Hostile.nextMove(this, dice, playerPosition, gameMap))

    case MoveEntity(entityId, to)
        if entityId == id && !gameMap.entities.exists(e => e.blocksMovement && e.position == to) =>
      Outcome(moveTo(to))

    case _ =>
      Outcome(this)

  def takeDamage(amount: Int): Troll =
    val f = fighter.takeDamage(amount)
    this.copy(
      fighter = f,
      isAlive = if f.hp > 0 then true else false
    )

object Troll:
  def spawn(id: Int, start: Point): Troll =
    Troll(id, start, true, Fighter(2, 0, 3), Nil)

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

object Fighter:
  def apply(hp: Int, defense: Int, power: Int): Fighter =
    Fighter(hp, hp, defense, power)

final case class MeleeAttack(attackerName: String, power: Int, id: Option[Int]) extends GlobalEvent
final case class MoveEntity(id: Int, to: Point)                                 extends GlobalEvent
case object UpdateEntities                                                      extends GlobalEvent
