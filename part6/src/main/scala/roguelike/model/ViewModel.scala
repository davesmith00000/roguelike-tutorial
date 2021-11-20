package roguelike.model

import indigo._

import io.indigoengine.roguelike.starterkit.*

final case class ViewModel(terminalEntity: Option[TerminalEntity], shroud: MapTile)

object ViewModel:
  def initial: ViewModel =
    ViewModel(None, MapTile(Tile.SPACE))
