package roguelike.model

import indigo._

import roguelike.GameEvent

import indigoextras.trees.QuadTree
import roguelike.RogueLikeGame
import roguelike.model.windows.HistoryViewer
import roguelike.model.windows.InventoryWindow
import roguelike.model.windows.DropWindow
import roguelike.ColorScheme

final case class Model(
    screenSize: Size,
    player: Player,
    gameMap: GameMap,
    messageLog: MessageLog,
    historyViewer: HistoryViewer,
    inventoryWindow: InventoryWindow,
    dropWindow: DropWindow,
    paused: Boolean,
    currentState: GameState
):
  def entitiesList: List[Entity] =
    gameMap.entitiesList :+ player

  def closeAllWindows: Model =
    this.copy(
      paused = false,
      currentState = GameState.Game
    )

  def toggleMessageHistory: Model =
    val show = !currentState.showingHistory
    this.copy(
      // paused = if show then true else false,
      currentState = if show then GameState.History else GameState.Game,
      historyViewer = if show then historyViewer.withPosition(0) else historyViewer
    )

  def toggleInventory: Model =
    val show = !currentState.showingInventory
    this.copy(
      // paused = if show then true else false,
      currentState = if show then GameState.Inventory else GameState.Game,
      inventoryWindow = if show then inventoryWindow.withPosition(0) else inventoryWindow
    )

  def toggleDropMenu: Model =
    val show = !currentState.showingDropMenu
    this.copy(
      // paused = if show then true else false,
      currentState = if show then GameState.Drop else GameState.Game,
      dropWindow = if show then dropWindow.withPosition(0) else dropWindow
    )

  def update(dice: Dice): GameEvent => Outcome[Model] =
    case GameEvent.Log(message) =>
      Outcome(
        this.copy(
          messageLog = messageLog.addMessage(message)
        )
      )

    case GameEvent.HostileMeleeAttack(name, power) =>
      val damage = Math.max(0, power - player.fighter.defense)

      val attackMessage =
        GameEvent.Log(
          if damage > 0 then Message(s"${name.capitalize} attacks for $damage hit points.", ColorScheme.enemyAttack)
          else Message(s"${name.capitalize} attacks but does no damage", ColorScheme.enemyAttack)
        )

      val p = player.takeDamage(damage)

      val msgs =
        if p.isAlive then List(attackMessage)
        else GameEvent.Log(Message("You died!", ColorScheme.playerDie)) :: attackMessage :: Nil

      Outcome(
        this.copy(
          player = p
        )
      ).addGlobalEvents(msgs)

    case GameEvent.PlayerAttack(name, power, id) =>
      gameMap.hostiles.collectFirst {
        case e: Hostile if id == e.id => e
      } match
        case None =>
          Outcome(this)
            .addGlobalEvents(GameEvent.Log(Message(s"${name.capitalize} swings and misses!", ColorScheme.playerAttack)))

        case Some(target) =>
          val damage = Math.max(0, power - target.fighter.defense)

          val msg =
            if damage > 0 then Message(s"${name.capitalize} attacks for $damage hit points.", ColorScheme.playerAttack)
            else Message(s"${name.capitalize} attacks but does no damage", ColorScheme.playerAttack)

          val res = gameMap
            .damageHostile(target.id, damage)

          val events = GameEvent.Log(msg) :: res.globalEventsOrNil.reverse

          res.clearGlobalEvents
            .map(gm =>
              this.copy(
                gameMap = gm
              )
            )
            .addGlobalEvents(events)

    case GameEvent.PlayerTurnEnd =>
      gameMap
        .updateHostiles(dice, player.position, paused)
        .map(gm => this.copy(gameMap = gm))
        .addGlobalEvents(GameEvent.Redraw)

    case GameEvent.Redraw | GameEvent.RegenerateLevel =>
      Outcome(this)

  def performPlayerTurn(dice: Dice, by: Point): Outcome[Model] =
    player.bump(by, gameMap).map(p => this.copy(player = p))

  def moveUp(dice: Dice): Outcome[Model]    = performPlayerTurn(dice, Point(0, -1))
  def moveDown(dice: Dice): Outcome[Model]  = performPlayerTurn(dice, Point(0, 1))
  def moveLeft(dice: Dice): Outcome[Model]  = performPlayerTurn(dice, Point(-1, 0))
  def moveRight(dice: Dice): Outcome[Model] = performPlayerTurn(dice, Point(1, 0))

  def pickUp: Outcome[Model] =
    player.pickUp(gameMap.items).map { case (p, updatedItems) =>
      this.copy(
        player = p,
        gameMap = gameMap.copy(items = updatedItems)
      )
    }

object Model:

  val HistoryWindowSize: Size   = Size(50, 36)
  val InventoryWindowSize: Size = Size(30, 10)
  val DropWindowSize: Size      = Size(30, 10)

  def initial(screenSize: Size): Model =
    val p = Player.initial(Point.zero)
    Model(
      screenSize,
      p,
      GameMap.initial(screenSize, Nil, Nil),
      MessageLog.Unlimited,
      HistoryViewer(HistoryWindowSize),
      InventoryWindow(InventoryWindowSize),
      DropWindow(DropWindowSize),
      false,
      GameState.Game
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
      .updateHostiles(dice, dungeon.playerStart, true)
      .map { gm =>
        Model(
          screenSize,
          p,
          gm,
          MessageLog.Unlimited,
          HistoryViewer(HistoryWindowSize),
          InventoryWindow(InventoryWindowSize),
          DropWindow(DropWindowSize),
          false,
          GameState.Game
        )
      }

enum GameState:
  case Game, History, Inventory, Drop

  def showingHistory: Boolean =
    this match
      case GameState.History => true
      case _                 => false

  def showingInventory: Boolean =
    this match
      case GameState.Inventory => true
      case _                   => false

  def showingDropMenu: Boolean =
    this match
      case GameState.Drop => true
      case _              => false

  def isRunning: Boolean =
    this match
      case GameState.Game => true
      case _              => false
