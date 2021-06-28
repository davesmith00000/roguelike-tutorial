package roguelike.model

import indigo._

import roguelike.terminal.TerminalEmulator

final case class ViewModel(background: TerminalEmulator)

object ViewModel:
  def initial(screenSize: Size): ViewModel =
    ViewModel(TerminalEmulator(screenSize))
