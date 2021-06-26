package roguelike

import indigo._
import indigo.scenes._
import scala.scalajs.js.annotation.JSExportTopLevel

import roguelike.utils.{MapRenderer, TerminalText}

@JSExportTopLevel("IndigoGame")
object RogueLikeGame extends IndigoGame[Unit, Unit, Unit, Unit]:

  def initialScene(bootData: Unit): Option[SceneName] =
    Option(StartScene.name)

  def scenes(bootData: Unit): NonEmptyList[Scene[Unit, Unit, Unit]] =
    NonEmptyList(StartScene, GameScene)

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(
      BootResult
        .noData(
          GameConfig.default.withMagnification(2)
        )
        .withFonts(DfTiles.Fonts.fontInfo)
        .withAssets(Assets.assets)
        .withShaders(
          MapRenderer.shader(Assets.Required.mapVertShader, Assets.Required.mapFragShader),
          TerminalText.shader(Assets.Required.textFragShader)
        )
    )

  def initialModel(startupData: Unit): Outcome[Unit] =
    Outcome(())

  def initialViewModel(startupData: Unit, model: Unit): Outcome[Unit] =
    Outcome(())

  def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def updateModel(context: FrameContext[Unit], model: Unit): GlobalEvent => Outcome[Unit] =
    _ => Outcome(model)

  def updateViewModel(context: FrameContext[Unit], model: Unit, viewModel: Unit): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(context: FrameContext[Unit], model: Unit, viewModel: Unit): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
