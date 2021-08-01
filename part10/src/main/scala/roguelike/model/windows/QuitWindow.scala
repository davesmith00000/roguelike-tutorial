package roguelike.model.windows

import roguelike.terminal.TerminalEmulator
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import roguelike.model.Inventory

final case class QuitWindow(size: Size, window: TerminalEmulator) extends Window:

  def toTerminal: TerminalEmulator =
    val innerSize = size - 2

    val term =
      TerminalEmulator(innerSize)
        .putLines(
          Point(0, 1),
          List(
            " [ 1 ] Save",
            " [ 2 ] Save and Quit",
            " [ 3 ] Quit"
          ),
          RGB.White,
          RGBA.Black
        )

    window.inset(term, Point(1, 1))

object QuitWindow:

  val WindowTitle: String = "┤ Quit to main menu? ├"
  val MinWindowSize: Size = Size(WindowTitle.length + 4, 7)

  def create: QuitWindow =
    val checkSize = MinWindowSize
    QuitWindow(checkSize, Window.createWindow(checkSize, WindowTitle))
