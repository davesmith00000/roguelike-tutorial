package roguelike

import indigo._
import indigo.scenes._

import io.indigoengine.roguelike.starterkit.*

import roguelike.model.Model
import roguelike.model.GameTile

object GameScene extends Scene[Unit, Model, Unit]:

  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("game scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[Unit, Unit] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: FrameContext[Unit], model: Model): GlobalEvent => Outcome[Model] =
    case KeyboardEvent.KeyUp(Key.UP_ARROW) =>
      Outcome(model.copy(player = model.player.moveUp(model.gameMap)))

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) =>
      Outcome(model.copy(player = model.player.moveDown(model.gameMap)))

    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) =>
      Outcome(model.copy(player = model.player.moveLeft(model.gameMap)))

    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) =>
      Outcome(model.copy(player = model.player.moveRight(model.gameMap)))

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  val terminal: TerminalEmulator =
    TerminalEmulator(RogueLikeGame.screenSize)

  def present(context: FrameContext[Unit], model: Model, viewModel: Unit): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        terminal
          .put(model.gameMap.toPositionedTiles)
          .put(model.entitiesList.map(e => (e.position, e.tile)))
          .draw(Assets.tileMap, RogueLikeGame.charSize, GameTile.Ground.mapTile, 4000)
      )
    )
