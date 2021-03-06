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
        KeyboardEvent.KeyUp(Key.ALT) if !model.currentState.isRunning =>
      Outcome(model.closeAllWindows)
        .addGlobalEvents(GameEvent.Redraw)

    // History window
    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.currentState.showingHistory =>
      Outcome(
        model.copy(
          historyViewer = model.historyViewer.scrollUp
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.currentState.showingHistory =>
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
    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.currentState.lookingAround =>
      model.lookUp

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.currentState.lookingAround =>
      model.lookDown

    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) if model.currentState.lookingAround =>
      model.lookLeft

    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) if model.currentState.lookingAround =>
      model.lookRight

    // Game controls
    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveUp(context.dice)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveDown(context.dice)

    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveLeft(context.dice)

    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) if model.currentState.isRunning && model.player.isAlive =>
      model.moveRight(context.dice)

    case KeyboardEvent.KeyUp(Key.KEY_G) if model.currentState.isRunning && model.player.isAlive =>
      model.pickUp

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

    // Look Around
    case KeyboardEvent.KeyUp(Key.FORWARD_SLASH) if model.currentState.isRunning || model.currentState.lookingAround =>
      Outcome(model.toggleLookAround(0))
        .addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.ENTER) if model.currentState.lookingAround =>
      Outcome(model.toggleLookAround(0))
        .addGlobalEvents(GameEvent.Targeted(model.lookAtTarget))

    // Other
    case GameEvent.RegenerateLevel =>
      Model
        .gen(context.dice, model.screenSize)
        .addGlobalEvents(GameEvent.Log(Message("Welcome!", RGB.Cyan)))

    case e: GameEvent =>
      model.update(context.dice)(e)

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case _: KeyboardEvent =>
      redrawTerminal(model, viewModel)

    case GameEvent.Redraw | GameEvent.RegenerateLevel =>
      redrawTerminal(model, viewModel)

    case _: GameEvent.Log =>
      redrawTerminal(model, viewModel)

    case _ =>
      Outcome(viewModel)

  private def redrawTerminal(model: Model, viewModel: ViewModel): Outcome[ViewModel] =
    val term =
      TerminalEmulator(RogueLikeGame.screenSize)
        .put(model.gameMap.toExploredTiles)
        .put(model.gameMap.visibleTiles)
        .put(model.entitiesList.map(e => (e.position, e.tile)))

    val log =
      model.messageLog.toTerminal(Size(RogueLikeGame.screenSize.width - 21, 5), false, 0, true)

    val withWindows =
      model.currentState match
        case GameState.Game =>
          term.inset(log, Point(21, 45))

        case GameState.History =>
          term
            .inset(log, Point(21, 45))
            .inset(
              model.historyViewer.toTerminal(model.messageLog),
              ((RogueLikeGame.screenSize - model.historyViewer.size) / 2).toPoint
            )

        case GameState.Inventory =>
          term
            .inset(log, Point(21, 45))
            .inset(
              model.inventoryWindow.toTerminal(model.player.inventory),
              ((RogueLikeGame.screenSize - model.inventoryWindow.size) / 2).toPoint
            )

        case GameState.Drop =>
          term
            .inset(log, Point(21, 45))
            .inset(
              model.dropWindow.toTerminal(model.player.inventory),
              ((RogueLikeGame.screenSize - model.dropWindow.size) / 2).toPoint
            )

        case GameState.LookAround(radius) =>
          term
            .get(model.lookAtTarget)
            .map(
              _.withForegroundColor(RGB.White)
                .withBackgroundColor(RGBA(0.7, 0.7, 0.7))
            ) match
            case None =>
              term
                .inset(log, Point(21, 45))
                .put(model.lookAtTarget, MapTile(Tile.DARK_SHADE, RGB.White, RGBA(0.7, 0.7, 0.7)))

            case Some(tile) =>
              term
                .inset(log, Point(21, 45))
                .put(model.lookAtTarget, tile)

    Outcome(
      viewModel.copy(
        terminalEntity = Option(
          withWindows
            .draw(Assets.tileMap, RogueLikeGame.charSize, viewModel.shroud, 4000)
        )
      )
    )

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
              View.renderNameHints(RogueLikeGame.charSize, context.mouse.position, model.gameMap.entitiesList),
              View.renderAreaOfEffect(RogueLikeGame.charSize, model.lookAtTarget, model.currentState)
            )
          )
        )
