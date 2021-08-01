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
import roguelike.model.ModelSaveData
import roguelike.model.GameLoadInfo

object LoadingScene extends Scene[Unit, Model, ViewModel]:

  type SceneModel     = GameLoadInfo
  type SceneViewModel = MapTile

  val name: SceneName =
    SceneName("loading scene")

  val modelLens: Lens[Model, GameLoadInfo] =
    Lens(_.loadInfo, (m, t) => m.copy(loadInfo = t))

  val viewModelLens: Lens[ViewModel, MapTile] =
    Lens.readOnly(_.shroud)

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: FrameContext[Unit],
      loadInfo: GameLoadInfo
  ): GlobalEvent => Outcome[GameLoadInfo] =
    case FrameTick =>
      loadInfo.loadingTimeOut match
        case None =>
          // Attempt to load
          Outcome(GameLoadInfo(Option(Seconds(1.5)), None))
            .addGlobalEvents(StorageEvent.Load(ModelSaveData.saveKey))

        case t @ Some(timeRemaining) if timeRemaining.toDouble <= 0.0 =>
          // Give up!
          Outcome(loadInfo).addGlobalEvents(SceneEvent.Next)

        case Some(timeRemaining) =>
          Outcome(loadInfo.updateTimeout(context.delta))

    case StorageEvent.Loaded(ModelSaveData.saveKey, data) =>
      ModelSaveData.fromJsonString(data) match
        case None =>
          IndigoLogger.error("Could not decode saved data...")
          Outcome(loadInfo).addGlobalEvents(SceneEvent.Next)

        case sd @ Some(_) =>
          Outcome(loadInfo.copy(loadedData = sd)).addGlobalEvents(SceneEvent.Next)

    case _ =>
      Outcome(loadInfo)

  def updateViewModel(
      context: FrameContext[Unit],
      loadInfo: GameLoadInfo,
      shroud: MapTile
  ): GlobalEvent => Outcome[MapTile] =
    _ => Outcome(shroud)

  def present(
      context: FrameContext[Unit],
      loadInfo: GameLoadInfo,
      shroud: MapTile
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Graphic(
          RogueLikeGame.viewportSize.width,
          RogueLikeGame.viewportSize.height,
          Material.Bitmap(Assets.menuBackground)
        )
          .scaleBy((RogueLikeGame.viewportSize / Size(160, 100)).toVector),
        TerminalEmulator(RogueLikeGame.screenSize)
          .putLine(Point(2, 48), "Loading...", RGB.White, RGBA.Black)
          .draw(Assets.tileMap, RogueLikeGame.charSize, shroud)
      )
    )
