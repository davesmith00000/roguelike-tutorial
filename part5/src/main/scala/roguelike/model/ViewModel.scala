package roguelike.model

import indigo._

import roguelike.terminal.TerminalEntity
import roguelike.terminal.MapTile
import roguelike.DfTiles

final case class ViewModel(terminalEntity: Option[TerminalEntity], shroud: MapTile)

object ViewModel:
  def initial(screenSize: Size): ViewModel =
    ViewModel(None, MapTile(DfTiles.Tile.SPACE))
