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
    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case _ =>
      Outcome(viewModel)

  def present(context: FrameContext[Unit], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Graphic(RogueLikeGame.viewportSize.width, RogueLikeGame.viewportSize.height, Material.Bitmap(Assets.menuBackground))
          .scaleBy((RogueLikeGame.viewportSize / Size(160, 100)).toVector)
      )
    )
