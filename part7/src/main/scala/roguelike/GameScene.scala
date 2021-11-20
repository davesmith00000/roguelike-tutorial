package roguelike

import indigo._
import indigo.scenes._

import io.indigoengine.roguelike.starterkit.*

import roguelike.model.Model
import roguelike.model.ViewModel
import roguelike.model.GameTile
import roguelike.GameEvent
import roguelike.model.Message

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
    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.showMessageHistory =>
      Outcome(
        model.copy(
          historyViewer = model.historyViewer.scrollUp
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.player.isAlive =>
      model.moveUp(context.dice)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.showMessageHistory =>
      Outcome(
        model.copy(
          historyViewer = model.historyViewer.scrollDown(model.messageLog.logLength)
        )
      ).addGlobalEvents(GameEvent.Redraw)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.player.isAlive =>
      model.moveDown(context.dice)

    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) if model.showMessageHistory =>
      Outcome(model)

    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) if model.player.isAlive =>
      model.moveLeft(context.dice)

    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) if model.showMessageHistory =>
      Outcome(model)

    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) if model.player.isAlive =>
      model.moveRight(context.dice)

    case KeyboardEvent.KeyUp(Key.KEY_V) =>
      Outcome(model.toggleMessageHistory)
        .addGlobalEvents(GameEvent.Redraw)

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

    val withHistory =
      if model.showMessageHistory then
        term
          .inset(log, Point(21, 45))
          .inset(
            model.historyViewer.toTerminal(model.messageLog),
            ((RogueLikeGame.screenSize - model.historyViewer.size) / 2).toPoint
          )
      else
        term
          .inset(log, Point(21, 45))

    Outcome(
      viewModel.copy(
        terminalEntity = Option(
          withHistory
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
              View.renderNameHints(RogueLikeGame.charSize, context.mouse.position, model.gameMap.entitiesList)
            )
          )
        )
