package roguelike

import indigo._
import indigo.scenes._

import roguelike.utils.{MapRenderer, TerminalText}
import roguelike.utils.MapTile
import roguelike.model.Model
import roguelike.utils.ConsoleEmulator

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
      Outcome(model.copy(player = model.player.moveUp))

    case KeyboardEvent.KeyUp(Key.DOWN_ARROW) =>
      Outcome(model.copy(player = model.player.moveDown))

    case KeyboardEvent.KeyUp(Key.LEFT_ARROW) =>
      Outcome(model.copy(player = model.player.moveLeft))

    case KeyboardEvent.KeyUp(Key.RIGHT_ARROW) =>
      Outcome(model.copy(player = model.player.moveRight))

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  val mapRenderer: MapRenderer =
    MapRenderer(Assets.tileMap, RogueLikeGame.screenSize, RogueLikeGame.charSize)

  val console: ConsoleEmulator =
    ConsoleEmulator(RogueLikeGame.screenSize)

  def present(context: FrameContext[Unit], model: Model, viewModel: Unit): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        mapRenderer.withMap(
          console
            .put(model.player.position, DfTiles.Tile.`@`)
            .draw(MapTile(DfTiles.Tile.SPACE))
        )
      )
    )
