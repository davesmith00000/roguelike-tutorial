package roguelike.model.windows

import io.indigoengine.roguelike.starterkit.*
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import roguelike.model.MessageLog

final case class HistoryViewer(size: Size, position: Int, window: TerminalEmulator) extends ScrollingWindow:

  def withPosition(newPosition: Int): HistoryViewer =
    this.copy(position = newPosition)
  def scrollUp: HistoryViewer =
    withPosition(ScrollingWindow.nextScrollUp(position))
  def scrollDown(lineCount: Int): HistoryViewer =
    withPosition(ScrollingWindow.nextScrollDown(size.height - 2, lineCount, position))

  def toTerminal(log: MessageLog): TerminalEmulator =
    val logTerm = log.toTerminal(size - 2, true, position, false)
    window.inset(logTerm, Point(1, 1))

object HistoryViewer:

  val WindowTitle: String = "┤ Message History ├"
  val MinWindowSize: Size = Size(WindowTitle.length + 4, 5)

  def apply(size: Size): HistoryViewer =
    val checkSize = size.max(MinWindowSize)
    HistoryViewer(checkSize, 0, Window.createWindow(checkSize, WindowTitle))
