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
import roguelike.model.Message
import roguelike.model.GameState
import roguelike.model.windows.Window

object MainMenuScene extends Scene[Unit, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("main menu scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: FrameContext[Unit], model: Model): GlobalEvent => Outcome[Model] =
    case KeyboardEvent.KeyUp(Key.KEY_N) =>
      Model
        .gen(context.dice, RogueLikeGame.screenSize)
        .addGlobalEvents(
          SceneEvent.JumpTo(GameScene.name),
          GameEvent.Log(Message("Welcome!", RGB.Cyan))
        )

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case KeyboardEvent.KeyUp(Key.KEY_N) =>
      viewModel.redrawTerminal(model)

    case _ =>
      Outcome(viewModel)

  def present(context: FrameContext[Unit], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Graphic(
          RogueLikeGame.viewportSize.width,
          RogueLikeGame.viewportSize.height,
          Material.Bitmap(Assets.menuBackground)
        )
          .scaleBy((RogueLikeGame.viewportSize / Size(160, 100)).toVector),
        TerminalEmulator(RogueLikeGame.screenSize)
          .putLine(Point(2, 20), "TOMBS OF THE ANCIENT KINGS", RGB.Yellow, RGBA.Black)
          .putLine(Point(2, 22), " [ n ] Play a new game", RGB.White, RGBA.Black)
          .putLine(Point(2, 23), " [ c ] Continue last game", RGB.White.mix(RGB.Black, 0.5), RGBA.Black)
          .putLine(Point(2, 48), "By Dave Smith", RGB.Yellow, RGBA.Black)
          .draw(Assets.tileMap, RogueLikeGame.charSize, viewModel.shroud)
      )
    )
