package roguelike.model

import indigo._

import io.indigoengine.roguelike.starterkit.*

final case class ViewModel(background: TerminalEmulator)

object ViewModel:
  def initial(screenSize: Size): ViewModel =
    ViewModel(TerminalEmulator(screenSize))
