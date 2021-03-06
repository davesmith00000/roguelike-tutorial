package roguelike.model

import indigo._

import roguelike.GameEvent

import indigoextras.trees.QuadTree
import roguelike.RogueLikeGame

import io.indigoengine.roguelike.starterkit.*

final case class Model(
    screenSize: Size,
    player: Player,
    gameMap: GameMap,
    messageLog: MessageLog,
    historyViewer: HistoryViewer,
    paused: Boolean,
    showMessageHistory: Boolean
):
  def entitiesList: List[Entity] =
    gameMap.entitiesList :+ player

  def toggleMessageHistory: Model =
    val show = !showMessageHistory
    this.copy(
      paused = if show then true else false,
      showMessageHistory = show,
      historyViewer = if show then historyViewer.withPosition(0) else historyViewer
    )

  def update(dice: Dice): GlobalEvent => Outcome[Model] =
    case GameEvent.Log(message) =>
      Outcome(
        this.copy(
          messageLog = messageLog.addMessage(message)
        )
      )

    case e: GameEvent.MoveEntity =>
      gameMap
        .update(dice, player.position, paused)(e)
        .map { gm =>
          this.copy(
            gameMap = gm
          )
        }
        .addGlobalEvents(GameEvent.Redraw)

    case GameEvent.MeleeAttack(name, power, None) =>
      val damage = Math.max(0, power - player.fighter.defense)

      val attackMessage =
        GameEvent.Log(
          if damage > 0 then Message(s"${name.capitalize} attacks for $damage hit points.", RGB.Yellow)
          else Message(s"${name.capitalize} attacks but does no damage", RGB(0.5, 0.5, 0.5))
        )

      val p = player.takeDamage(damage)

      val msgs =
        if p.isAlive then List(attackMessage)
        else GameEvent.Log(Message("You died!", RGB.Red)) :: attackMessage :: Nil

      Outcome(
        this.copy(
          player = player.takeDamage(damage)
        )
      ).addGlobalEvents(msgs)

    case GameEvent.MeleeAttack(name, power, Some(id)) =>
      gameMap.entities.collectFirst {
        case e: Hostile if id == e.id => e
      } match
        case None =>
          Outcome(this)
            .addGlobalEvents(GameEvent.Log(Message(s"${name.capitalize} swings and misses!", RGB(0.5, 0.5, 0.5))))

        case Some(target) =>
          val damage = Math.max(0, power - target.fighter.defense)

          val msg =
            if damage > 0 then Message(s"${name.capitalize} attacks for $damage hit points.", RGB(1.0, 0.8, 0.1))
            else Message(s"${name.capitalize} attacks but does no damage", RGB(0.3, 0.3, 0.3))

          val res = gameMap
            .damageEntity(target.id, damage)

          val events = GameEvent.Log(msg) :: res.globalEventsOrNil.reverse

          res.clearGlobalEvents
            .map(gm =>
              this.copy(
                gameMap = gm
              )
            )
            .addGlobalEvents(events)

    case _ =>
      Outcome(this)

  def performPlayerTurn(dice: Dice, by: Point): Outcome[Model] =
    val res = for {
      p  <- player.bump(by, gameMap)
      gm <- gameMap.updateEntities(dice, p.position, paused)
    } yield this.copy(
      player = p,
      gameMap = gm
    )

    res.addGlobalEvents(GameEvent.Redraw)

  def moveUp(dice: Dice): Outcome[Model]    = performPlayerTurn(dice, Point(0, -1))
  def moveDown(dice: Dice): Outcome[Model]  = performPlayerTurn(dice, Point(0, 1))
  def moveLeft(dice: Dice): Outcome[Model]  = performPlayerTurn(dice, Point(-1, 0))
  def moveRight(dice: Dice): Outcome[Model] = performPlayerTurn(dice, Point(1, 0))

object Model:

  val HistoryWindowSize: Size = Size(50, 36)

  def initial(screenSize: Size): Model =
    val p = Player.initial(Point.zero)
    Model(
      screenSize,
      p,
      GameMap.initial(screenSize, Nil),
      MessageLog.Unlimited,
      HistoryViewer(HistoryWindowSize),
      false,
      false
    )

  def gen(dice: Dice, screenSize: Size): Outcome[Model] =
    val dungeon =
      DungeonGen.makeMap(
        dice,
        DungeonGen.MaxRooms,
        DungeonGen.RoomMinSize,
        DungeonGen.RoomMaxSize,
        screenSize - Size(0, 5),
        DungeonGen.MaxMonstersPerRoom
      )

    val p = Player.initial(dungeon.playerStart)

    GameMap
      .gen(screenSize, dungeon)
      .updateEntities(dice, dungeon.playerStart, true)
      .map { gm =>
        Model(
          screenSize,
          p,
          gm,
          MessageLog.Unlimited,
          HistoryViewer(HistoryWindowSize),
          false,
          false
        )
      }
