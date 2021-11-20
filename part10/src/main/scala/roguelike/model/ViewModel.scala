package roguelike.model

import indigo._

import roguelike.RogueLikeGame
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets

final case class ViewModel(terminalEntity: Option[TerminalEntity], shroud: MapTile):

  def redrawTerminal(model: Model): Outcome[ViewModel] =
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

        case GameState.Quit =>
          term
            .inset(log, Point(21, 45))
            .inset(
              model.quitWindow.toTerminal(model.player.isAlive),
              ((RogueLikeGame.screenSize - model.quitWindow.size) / 2).toPoint
            )

    Outcome(
      this.copy(
        terminalEntity = Option(
          withWindows
            .draw(Assets.tileMap, RogueLikeGame.charSize, this.shroud, 4000)
        )
      )
    )

object ViewModel:
  def initial: ViewModel =
    ViewModel(
      None,
      MapTile(Tile.SPACE)
    )
