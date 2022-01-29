package roguelike

import indigo._
import indigo.scenes._
import io.indigoengine.roguelike.starterkit.*
import roguelike.model.Model
import roguelike.model.ViewModel

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object RogueLikeGame extends IndigoGame[Unit, Unit, Model, ViewModel]:

  val screenSize: Size   = Size(80, 50)
  val charSize: Size     = Size(10, 10)
  val viewportSize: Size = screenSize * charSize

  def initialScene(bootData: Unit): Option[SceneName] =
    None

  def scenes(bootData: Unit): NonEmptyList[Scene[Unit, Model, ViewModel]] =
    NonEmptyList(LoadingScene, MainMenuScene, GameScene)

  val eventFilters: EventFilters =
    EventFilters.BlockAll

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(
      BootResult
        .noData(
          GameConfig.default
            .withViewport(viewportSize.width, viewportSize.height)
        )
        .withFonts(RoguelikeTiles.Size10x10.Fonts.fontInfo)
        .withAssets(Assets.assets)
        .withShaders(
          TerminalEntity.shader(4000),
          TerminalText.standardShader
        )
    )

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(Model.initial(Dice.fromSeed(0), screenSize))

  def initialViewModel(startupData: Unit, model: Model): Outcome[ViewModel] =
    Outcome(ViewModel.initial)

  def setup(
      bootData: Unit,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def updateModel(
      context: FrameContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Layer(BindingKey("game")),
        Layer(BindingKey("log")),
        Layer(BindingKey("fps"))
      )
    )
