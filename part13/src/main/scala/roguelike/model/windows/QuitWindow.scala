package roguelike.model.windows

import io.indigoengine.roguelike.starterkit.*
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import roguelike.model.Inventory

final case class QuitWindow(size: Size, window: TerminalEmulator) extends Window:

  def toTerminal(playerAlive: Boolean): TerminalEmulator =
    val innerSize = size - 2

    val useable: RGB =
      if playerAlive then RGB.White
      else RGB.White.mix(RGB.Black, 0.5)

    val term =
      TerminalEmulator(innerSize)
        .putLine(Point(0, 1), "[1] Save", useable, RGBA.Black)
        .putLine(Point(0, 2), "[2] Save and Quit", useable, RGBA.Black)
        .putLine(Point(0, 3), "[3] Quit", RGB.White, RGBA.Black)

    window.inset(term, Point(1, 1))

object QuitWindow:

  val WindowTitle: String = "┤ Quit to main menu? ├"
  val MinWindowSize: Size = Size(WindowTitle.length + 4, 7)

  def create: QuitWindow =
    val checkSize = MinWindowSize
    QuitWindow(checkSize, Window.createWindow(checkSize, WindowTitle))
