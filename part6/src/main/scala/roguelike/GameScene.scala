package roguelike

import indigo._
import indigo.scenes._

import roguelike.terminal.TerminalText
import roguelike.terminal.MapTile
import roguelike.terminal.TerminalEmulator
import roguelike.terminal.TerminalEntity

import roguelike.model.Model
import roguelike.model.ViewModel
import roguelike.model.GameTile
import roguelike.GameEvent

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
    case KeyboardEvent.KeyUp(Key.UP_ARROW) if model.player.isAlive =>
      model.moveUp(context.dice)

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) if model.player.isAlive =>
      model.moveDown(context.dice)

    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) if model.player.isAlive =>
      model.moveLeft(context.dice)

    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) if model.player.isAlive =>
      model.moveRight(context.dice)

    case RegenerateLevel =>
      Outcome(Model.gen(context.dice, model.screenSize))

    case e: GameEvent =>
      model.update(context.dice)(e)

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case KeyboardEvent.KeyUp(_) | RegenerateLevel =>
      val term =
        TerminalEmulator(RogueLikeGame.screenSize)
          .put(model.gameMap.toExploredTiles)
          .put(model.gameMap.visibleTiles)
          .put(model.entitiesList.map(e => (e.position, e.tile)))
          .draw(Assets.tileMap, RogueLikeGame.charSize, viewModel.shroud)

      Outcome(
        viewModel.copy(
          terminalEntity = Option(term)
        )
      )

    case _ =>
      Outcome(viewModel)

  val statusLine: TextBox =
    TextBox("")
      .withColor(RGBA.Green)
      .withFontFamily(FontFamily.monospace)
      .withFontSize(Pixels((RogueLikeGame.charSize.height * 2) - 4))
      .withSize(RogueLikeGame.screenSize * RogueLikeGame.charSize)
      .moveTo(2, 2)

  val consoleLine: TextBox =
    TextBox("> ")
      .withColor(RGBA.Green)
      .withFontFamily(FontFamily.monospace)
      .withFontSize(Pixels((RogueLikeGame.charSize.height * 2) - 4))
      .withSize(RogueLikeGame.screenSize * RogueLikeGame.charSize)
      .moveTo(2, ((RogueLikeGame.screenSize.height - 2) * RogueLikeGame.charSize.height) + 1)

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
              statusLine.withText(model.status),
              consoleLine.withText("> " + model.message)
            )
          )
        )
