package roguelike.model

import indigo._

import roguelike.GameEvent

import indigoextras.trees.QuadTree
import roguelike.RogueLikeGame
import roguelike.model.windows.HistoryViewer
import roguelike.model.windows.InventoryWindow
import roguelike.model.windows.DropWindow
import roguelike.model.windows.QuitWindow
import roguelike.ColorScheme

final case class Model(
    screenSize: Size,
    player: Player,
    stairsPosition: Point,
    lookAtTarget: Point,
    gameMap: GameMap,
    messageLog: MessageLog,
    historyViewer: HistoryViewer,
    inventoryWindow: InventoryWindow,
    dropWindow: DropWindow,
    quitWindow: QuitWindow,
    paused: Boolean,
    currentState: GameState,
    targetingWithItemAt: Option[Int],
    loadInfo: GameLoadInfo,
    currentFloor: Int
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
      currentState = if show then GameState.History else GameState.Game,
      historyViewer = if show then historyViewer.withPosition(0) else historyViewer
    )

  def toggleInventory: Model =
    val show = !currentState.showingInventory
    this.copy(
      currentState = if show then GameState.Inventory else GameState.Game,
      inventoryWindow = if show then inventoryWindow.withPosition(0) else inventoryWindow
    )

  def toggleDropMenu: Model =
    val show = !currentState.showingDropMenu
    this.copy(
      currentState = if show then GameState.Drop else GameState.Game,
      dropWindow = if show then dropWindow.withPosition(0) else dropWindow
    )

  def toggleLookAround(radius: Int): Model =
    val show = !currentState.lookingAround
    this.copy(
      currentState = if show then GameState.LookAround(radius) else GameState.Game,
      lookAtTarget = player.position
    )

  def toggleQuit: Model =
    val show = !currentState.showingQuit
    this.copy(
      currentState = if show then GameState.Quit else GameState.Game
    )

  def update(dice: Dice): GameEvent => Outcome[Model] =
    case GameEvent.Log(message) =>
      Outcome(
        this.copy(
          messageLog = messageLog.addMessage(message)
        )
      )

    case GameEvent.TargetUsingItem(inventoryPosition, radius) =>
      Outcome(
        this.copy(
          currentState = GameState.LookAround(radius),
          lookAtTarget = player.position,
          targetingWithItemAt = Option(inventoryPosition)
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case GameEvent.Targeted(position) =>
      targetingWithItemAt match
        case None =>
          Outcome(this)
            .addGlobalEvents(GameEvent.Log(Message("No item selected", ColorScheme.impossible)))

        case Some(_) if position == player.position =>
          Outcome(this)
            .addGlobalEvents(GameEvent.Log(Message("You cannot target yourself!", ColorScheme.impossible)))

        case Some(_) if !gameMap.visible.contains(position) =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(Message("You cannot target an area that you cannot see!", ColorScheme.impossible))
            )

        case Some(_) if !gameMap.hostiles.map(_.position).contains(position) =>
          Outcome(this)
            .addGlobalEvents(GameEvent.Log(Message("You must select an enemy to target.", ColorScheme.impossible)))

        case Some(itemAt) =>
          gameMap.hostiles.find(_.position == position) match
            case None =>
              Outcome(this)
                .addGlobalEvents(GameEvent.Log(Message("You must select an enemy to target.", ColorScheme.impossible)))

            case Some(target) =>
              player
                .consumeTargetted(itemAt, target, gameMap.visibleHostiles)
                .map { p =>
                  this
                    .copy(
                      player = p,
                      targetingWithItemAt = None,
                      currentState = GameState.Game
                    )
                    .closeAllWindows
                }
                .addGlobalEvents(GameEvent.PlayerTurnEnd)

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
          Outcome(this.closeAllWindows)
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
              this
                .copy(
                  gameMap = gm
                )
                .closeAllWindows
            )
            .addGlobalEvents(events)

    case GameEvent.PlayerCastsConfusion(name, numberOfTurns, id) =>
      gameMap.hostiles.collectFirst {
        case e: Hostile if id == e.id => e
      } match
        case None =>
          Outcome(this)
            .addGlobalEvents(GameEvent.Log(Message(s"${name.capitalize} misses!", ColorScheme.playerAttack)))

        case Some(target) =>
          gameMap
            .confuseHostile(target.id, numberOfTurns)
            .map(gm =>
              this.copy(
                gameMap = gm
              )
            )

    case GameEvent.PlayerCastsFireball(name, damage, id) =>
      gameMap.hostiles.collectFirst {
        case e: Hostile if id == e.id => e
      } match
        case None =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(Message(s"${name.capitalize} misses!", ColorScheme.playerAttack)),
              GameEvent.PlayerTurnEnd
            )

        case Some(target) =>
          gameMap
            .damageHostile(target.id, damage)
            .map(gm =>
              this.copy(
                gameMap = gm
              )
            )
            .addGlobalEvents(GameEvent.PlayerTurnEnd)

    case GameEvent.PlayerTurnEnd =>
      gameMap
        .updateHostiles(dice, player.position, paused)
        .map(gm => this.copy(gameMap = gm))
        .addGlobalEvents(GameEvent.Redraw)

    case GameEvent.Redraw =>
      Outcome(this)

  def performPlayerTurn(dice: Dice, by: Point): Outcome[Model] =
    player.bump(by, gameMap).map(p => this.copy(player = p))

  def moveUp(dice: Dice): Outcome[Model]    = performPlayerTurn(dice, Point(0, -1))
  def moveDown(dice: Dice): Outcome[Model]  = performPlayerTurn(dice, Point(0, 1))
  def moveLeft(dice: Dice): Outcome[Model]  = performPlayerTurn(dice, Point(-1, 0))
  def moveRight(dice: Dice): Outcome[Model] = performPlayerTurn(dice, Point(1, 0))

  def performMoveLookAtTarget(by: Point): Outcome[Model] =
    Outcome(this.copy(lookAtTarget = lookAtTarget + by))

  def lookUp: Outcome[Model]    = performMoveLookAtTarget(Point(0, -1))
  def lookDown: Outcome[Model]  = performMoveLookAtTarget(Point(0, 1))
  def lookLeft: Outcome[Model]  = performMoveLookAtTarget(Point(-1, 0))
  def lookRight: Outcome[Model] = performMoveLookAtTarget(Point(1, 0))

  def pickUp: Outcome[Model] =
    player.pickUp(gameMap.items).map { case (p, updatedItems) =>
      this.copy(
        player = p,
        gameMap = gameMap.copy(items = updatedItems)
      )
    }

  def toSaveData: ModelSaveData =
    ModelSaveData(screenSize, player, gameMap, messageLog)

object Model:

  val HistoryWindowSize: Size   = Size(50, 36)
  val InventoryWindowSize: Size = Size(30, 10)
  val DropWindowSize: Size      = Size(30, 10)
  val QuitWindowSize: Size      = Size(30, 10)

  def initial(screenSize: Size): Model =
    val p = Player.initial(Point.zero)
    Model(
      screenSize,
      p,
      Point.zero,
      Point.zero,
      GameMap.initial(screenSize, Nil, Nil),
      MessageLog.Unlimited,
      HistoryViewer(HistoryWindowSize),
      InventoryWindow(InventoryWindowSize),
      DropWindow(DropWindowSize),
      QuitWindow.create,
      false,
      GameState.Game,
      None,
      GameLoadInfo(None, None),
      0
    )

  def fromSaveData(saveData: ModelSaveData): Model =
    initial(saveData.screenSize).copy(
      player = saveData.player,
      gameMap = saveData.gameMap,
      messageLog = saveData.messageLog,
      loadInfo = GameLoadInfo(None, Option(saveData))
    )

  def gen(dice: Dice, screenSize: Size): Outcome[Model] =
    val dungeon =
      DungeonGen.makeMap(
        dice,
        DungeonGen.MaxRooms,
        DungeonGen.RoomMinSize,
        DungeonGen.RoomMaxSize,
        screenSize - Size(0, 5),
        DungeonGen.MaxMonstersPerRoom,
        DungeonGen.MaxItemsPerRoom,
        0
      )

    val p = Player.initial(dungeon.playerStart)

    GameMap
      .gen(screenSize, dungeon)
      .updateHostiles(dice, dungeon.playerStart, true)
      .map { gm =>
        Model(
          screenSize,
          p,
          dungeon.stairsPosition,
          Point.zero,
          gm,
          MessageLog.Unlimited,
          HistoryViewer(HistoryWindowSize),
          InventoryWindow(InventoryWindowSize),
          DropWindow(DropWindowSize),
          QuitWindow.create,
          false,
          GameState.Game,
          None,
          GameLoadInfo(None, None),
          0
        )
      }

  def genNextFloor(dice: Dice, currentModel: Model): Outcome[Model] =
    val nextFloor = currentModel.currentFloor + 1

    val dungeon =
      DungeonGen.makeMap(
        dice,
        DungeonGen.MaxRooms,
        DungeonGen.RoomMinSize,
        DungeonGen.RoomMaxSize,
        currentModel.screenSize - Size(0, 5),
        DungeonGen.MaxMonstersPerRoom,
        DungeonGen.MaxItemsPerRoom,
        nextFloor
      )

    GameMap
      .gen(currentModel.screenSize, dungeon)
      .updateHostiles(dice, dungeon.playerStart, true)
      .map { gm =>
        currentModel.copy(
          player = currentModel.player.copy(position = dungeon.playerStart),
          stairsPosition = dungeon.stairsPosition,
          gameMap = gm,
          currentFloor = nextFloor
        )
      }

enum GameState:
  case Game extends GameState
  case History extends GameState
  case Inventory extends GameState
  case Drop extends GameState
  case LookAround(radius: Int) extends GameState
  case Quit extends GameState

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

  def lookingAround: Boolean =
    this match
      case GameState.LookAround(_) => true
      case _                       => false

  def showingQuit: Boolean =
    this match
      case GameState.Quit => true
      case _              => false

final case class GameLoadInfo(loadingTimeOut: Option[Seconds], loadedData: Option[ModelSaveData]):
  def updateTimeout(delta: Seconds): GameLoadInfo =
    this.copy(
      loadingTimeOut = loadingTimeOut.map(t => if (t - delta).toDouble <= 0.0 then Seconds.zero else t - delta)
    )
