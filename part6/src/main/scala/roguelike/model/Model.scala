package roguelike.model

import indigo._

import roguelike.GameEvent

import indigoextras.trees.QuadTree

import io.indigoengine.roguelike.starterkit.*

final case class Model(
    screenSize: Size,
    player: Player,
    gameMap: GameMap,
    status: String,
    message: String,
    paused: Boolean
):
  def entitiesList: List[Entity] =
    gameMap.entitiesList :+ player

  def update(dice: Dice): GlobalEvent => Outcome[Model] =
    case e: GameEvent.MoveEntity =>
      gameMap.update(dice, player.position, paused)(e).map { gm =>
        this.copy(
          gameMap = gm,
          status = Model.formatStatus(player)
        )
      }

    case GameEvent.MeleeAttack(name, power, None) =>
      val damage = Math.max(0, power - player.fighter.defense)

      val msg =
        if damage > 0 then s"${name.capitalize} attacks for $damage hit points."
        else s"${name.capitalize} attacks but does no damage"

      val p = player.takeDamage(damage)

      Outcome(
        this.copy(
          player = player.takeDamage(damage),
          message = if p.isAlive then msg else "You died!",
          status = Model.formatStatus(p)
        )
      )

    case GameEvent.MeleeAttack(name, power, Some(id)) =>
      gameMap.entities.collectFirst {
        case e: Hostile if id == e.id => e
      } match
        case None =>
          Outcome(
            this.copy(
              message = s"${name.capitalize} swings and misses!",
              status = Model.formatStatus(player)
            )
          )

        case Some(target) =>
          val damage = Math.max(0, power - target.fighter.defense)

          val msg =
            if damage > 0 then s"${name.capitalize} attacks for $damage hit points."
            else s"${name.capitalize} attacks but does no damage"

          Outcome(
            this.copy(
              gameMap = gameMap.damageEntity(target.id, damage),
              message = msg,
              status = Model.formatStatus(player)
            )
          )

    case _ =>
      Outcome(this)

  def performTurn(dice: Dice, by: Point): Outcome[Model] =
    for {
      p  <- player.bump(by, gameMap)
      gm <- gameMap.updateEntities(dice, p.position, paused)
    } yield this.copy(
      player = p,
      gameMap = gm
    )

  def moveUp(dice: Dice): Outcome[Model]    = performTurn(dice, Point(0, -1))
  def moveDown(dice: Dice): Outcome[Model]  = performTurn(dice, Point(0, 1))
  def moveLeft(dice: Dice): Outcome[Model]  = performTurn(dice, Point(-1, 0))
  def moveRight(dice: Dice): Outcome[Model] = performTurn(dice, Point(1, 0))

object Model:

  def formatStatus(player: Player): String =
    s"HP: ${Math.max(0, player.fighter.hp)}/${player.fighter.maxHp}"

  def initial(screenSize: Size): Model =
    val p = Player.initial(Point.zero)
    Model(
      screenSize,
      p,
      GameMap.initial(screenSize, Nil),
      formatStatus(p),
      "",
      false
    )

  def gen(dice: Dice, screenSize: Size): Model =
    val dungeon =
      DungeonGen.makeMap(
        dice,
        DungeonGen.MaxRooms,
        DungeonGen.RoomMinSize,
        DungeonGen.RoomMaxSize,
        screenSize,
        DungeonGen.MaxMonstersPerRoom
      )

    val p = Player.initial(dungeon.playerStart)

    Model(
      screenSize,
      p,
      GameMap.gen(screenSize, dungeon).updateEntities(dice, dungeon.playerStart, true).unsafeGet,
      formatStatus(p),
      "",
      false
    )
