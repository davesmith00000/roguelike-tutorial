package roguelike

import indigo._
import indigo.scenes._

import io.indigoengine.roguelike.starterkit.*

import roguelike.model.Model
import roguelike.model.ViewModel
import roguelike.model.GameTile
import roguelike.GameEvent
import roguelike.model.Message
import roguelike.model.GameState
import roguelike.model.windows.Window
import roguelike.model.ModelSaveData
import roguelike.model.GameLoadInfo

object GameScene extends Scene[Unit, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("game scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: FrameContext[Unit], model: Model): GlobalEvent => Outcome[Model] =

    // Window close keys
    case KeyboardEvent.KeyUp(Key.ESCAPE) | KeyboardEvent.KeyUp(Key.SHIFT) | KeyboardEvent.KeyUp(Key.CTRL) |
        KeyboardEvent.KeyUp(Key.ALT) if !model.currentState.isRunning || !model.currentState.showingLevelUp =>
      Outcome(model.closeAllWindows)
        .addGlobalEvents(GameEvent.Redraw)

    // Quit window
    // Save
    case KeyboardEvent.KeyUp(Key.KEY_1) if model.currentState.showingQuit && model.player.isAlive =>
      val saveData = model.toSaveData
      Outcome(model.copy(loadInfo = GameLoadInfo(None, Option(saveData))))
        .addGlobalEvents(StorageEvent.Save(ModelSaveData.saveKey, model.toSaveData.toJsonString))

    // Save and Quit
    case KeyboardEvent.KeyUp(Key.KEY_2) if model.currentState.showingQuit && model.player.isAlive =>
      val saveData = model.toSaveData
      Outcome(model.copy(loadInfo = GameLoadInfo(None, Option(saveData))))
        .addGlobalEvents(
          StorageEvent.Save(ModelSaveData.saveKey, saveData.toJsonString),
          SceneEvent.JumpTo(MainMenuScene.name)
        )

    // Quit
    case KeyboardEvent.KeyUp(Key.KEY_3) if model.currentState.showingQuit =>
      Outcome(model).addGlobalEvents(SceneEvent.JumpTo(MainMenuScene.name))

    // Level up window
    // Constitution
    case KeyboardEvent.KeyUp(Key.KEY_1) if model.currentState.showingLevelUp =>
      model.player
        .increaseMaxHp(20)
        .map { p =>
          model.copy(player = p).toggleLevelUp
        }
        .addGlobalEvents(GameEvent.Redraw)

    // Strength
    case KeyboardEvent.KeyUp(Key.KEY_2) if model.currentState.showingLevelUp =>
      model.player
        .increasePower(1)
        .map { p =>
          model.copy(player = p).toggleLevelUp
        }
        .addGlobalEvents(GameEvent.Redraw)

    // Agility
    case KeyboardEvent.KeyUp(Key.KEY_3) if model.currentState.showingLevelUp =>
      model.player
        .increaseDefense(1)
        .map { p =>
          model.copy(player = p).toggleLevelUp
        }
        .addGlobalEvents(GameEvent.Redraw)

    // Invalid level up selection
    case KeyboardEvent.KeyUp(_) if model.currentState.showingLevelUp =>
      Outcome(model).addGlobalEvents(GameEvent.Log(Message("Invalid, please press 1, 2, or 3.", ColorScheme.invalid)))

    // History window
    case KeyboardEvent.KeyDown(Key.UP_ARROW) if model.currentState.showingHistory =>
      Outcome(
        model.copy(
          historyViewer = model.historyViewer.scrollUp
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyDown(Key.DOWN_ARROW) if model.currentState.showingHistory =>
      Outcome(
        model.copy(
          historyViewer = model.historyViewer.scrollDown(model.messageLog.logLength)
        )
      ).addGlobalEvents(GameEvent.Redraw)

    // Inventory window
    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.currentState.showingInventory =>
      Outcome(
        model.copy(
          inventoryWindow = model.inventoryWindow.scrollUp
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.currentState.showingInventory =>
      Outcome(
        model.copy(
          inventoryWindow = model.inventoryWindow.scrollDown(model.player.inventory.items.length)
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(key) if model.currentState.showingInventory =>
      Window.letterPositions.get(key.key) match
        case None =>
          Outcome(model)

        case Some(keyIndex) =>
          model.player
            .consume(keyIndex, model.gameMap.visibleHostiles)
            .map { p =>
              model.copy(
                player = p
              )
            }
            .addGlobalEvents(GameEvent.Redraw)

    // Drop window
    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.currentState.showingDropMenu =>
      Outcome(
        model.copy(
          dropWindow = model.dropWindow.scrollUp
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.currentState.showingDropMenu =>
      Outcome(
        model.copy(
          dropWindow = model.dropWindow.scrollDown(model.player.inventory.items.length)
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(key) if model.currentState.showingDropMenu =>
      Window.letterPositions.get(key.key) match
        case None =>
          Outcome(model)

        case Some(keyIndex) =>
          model.player
            .drop(keyIndex, model.gameMap.items)
            .map {
              case (p, None) =>
                model.copy(
                  player = p
                )

              case (p, Some(item)) =>
                model.copy(
                  player = p,
                  gameMap = model.gameMap.dropItem(item)
                )
            }
            .addGlobalEvents(GameEvent.Redraw)

    // Looking around
    case KeyboardEvent.KeyDown(Key.UP_ARROW) if model.currentState.lookingAround =>
      model.lookUp

    case KeyboardEvent.KeyDown(Key.DOWN_ARROW) if model.currentState.lookingAround =>
      model.lookDown

    case KeyboardEvent.KeyDown(Key.LEFT_ARROW) if model.currentState.lookingAround =>
      model.lookLeft

    case KeyboardEvent.KeyDown(Key.RIGHT_ARROW) if model.currentState.lookingAround =>
      model.lookRight

    // Game controls
    case KeyboardEvent.KeyDown(Key.UP_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveUp(context.dice)

    case KeyboardEvent.KeyDown(Key.DOWN_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveDown(context.dice)

    case KeyboardEvent.KeyDown(Key.LEFT_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveLeft(context.dice)

    case KeyboardEvent.KeyDown(Key.RIGHT_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveRight(context.dice)

    case KeyboardEvent.KeyUp(Key.KEY_G) if model.currentState.isRunning && model.player.isAlive =>
      model.pickUp

    case KeyboardEvent.KeyUp(Key.PERIOD) if model.currentState.isRunning && model.player.isAlive =>
      if model.player.position == model.stairsPosition then
        Model
          .genNextFloor(context.dice, model)
          .addGlobalEvents(
            GameEvent.Log(Message("You descend the staircase.", ColorScheme.descend)),
            GameEvent.Redraw
          )
      else
        Outcome(model)
          .addGlobalEvents(GameEvent.Log(Message("There are no stairs here.", ColorScheme.impossible)))

    // Window toggles
    case KeyboardEvent.KeyUp(Key.KEY_V) if model.currentState.isRunning || model.currentState.showingHistory =>
      Outcome(model.toggleMessageHistory)
        .addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.KEY_I) if model.currentState.isRunning || model.currentState.showingInventory =>
      Outcome(model.toggleInventory)
        .addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.KEY_D) if model.currentState.isRunning || model.currentState.showingDropMenu =>
      Outcome(model.toggleDropMenu)
        .addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.KEY_Q) if model.currentState.isRunning || model.currentState.showingQuit =>
      Outcome(model.toggleQuit)
        .addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.KEY_C) if model.currentState.isRunning || model.currentState.showingCharacter =>
      Outcome(model.toggleCharacter)
        .addGlobalEvents(GameEvent.Redraw)

    // Look Around
    case KeyboardEvent.KeyUp(Key.FORWARD_SLASH) if model.currentState.isRunning || model.currentState.lookingAround =>
      Outcome(model.toggleLookAround(0))
        .addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.ENTER) if model.currentState.lookingAround =>
      Outcome(model.toggleLookAround(0))
        .addGlobalEvents(GameEvent.Targeted(model.lookAtTarget))

    // Other
    case e: GameEvent =>
      model.update(context.dice)(e)

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case _: KeyboardEvent if viewModel.lastRedraw < context.running =>
      viewModel.redrawTerminal(model, context.running)

    case GameEvent.Redraw | GameEvent.PlayerTurnEnd if viewModel.lastRedraw < context.running =>
      viewModel.redrawTerminal(model, context.running)

    case _: GameEvent.Log if viewModel.lastRedraw < context.running =>
      viewModel.redrawTerminal(model, context.running)

    case _ =>
      Outcome(viewModel)

  def present(context: FrameContext[Unit], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    viewModel.terminalEntity match
      case None =>
        Outcome(
          SceneUpdateFragment(
            Layer(
              BindingKey("game"),
              TextBox("No level", 100, 30)
                .withColor(RGBA.White)
                .withFontFamily(FontFamily.monospace)
            )
          )
        )

      case Some(entity) =>
        Outcome(
          SceneUpdateFragment(
            Layer(
              BindingKey("game"),
              entity
            ),
            Layer(
              BindingKey("log"),
              View.renderBar(model.player, 20, Point(0, 45)),
              View.renderLevel(Point(0, 47), model.currentFloor),
              View.renderNameHints(
                RogueLikeGame.charSize,
                context.mouse.position,
                model.gameMap.entitiesList,
                model.stairsPosition
              ),
              View.renderAreaOfEffect(RogueLikeGame.charSize, model.lookAtTarget, model.currentState)
            )
          )
        )
