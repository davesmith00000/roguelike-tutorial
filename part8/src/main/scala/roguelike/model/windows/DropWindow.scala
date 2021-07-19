package roguelike.model.windows

import roguelike.terminal.TerminalEmulator
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import roguelike.model.MessageLog

final case class DropWindow(size: Size, position: Int, window: TerminalEmulator) extends Window:

  def withPosition(newPosition: Int): DropWindow =
    this.copy(position = newPosition)
  def scrollUp: DropWindow =
    withPosition(Window.nextScrollUp(position))
  def scrollDown(lineCount: Int): DropWindow =
    withPosition(Window.nextScrollDown(size.height - 2, lineCount, position))

  def toTerminal/*(log: MessageLog)*/: TerminalEmulator =
    // val logTerm = log.toTerminal(size - 2, true, position, false)
    window//.inset(logTerm, Point(1, 1))

object DropWindow:

  val WindowTitle: String = "┤ Select an item to drop ├"
  val MinWindowSize: Size = Size(WindowTitle.length + 4, 5)

  def apply(size: Size): DropWindow =
    val checkSize = size.max(MinWindowSize)
    DropWindow(checkSize, 0, Window.createWindow(checkSize, WindowTitle))
