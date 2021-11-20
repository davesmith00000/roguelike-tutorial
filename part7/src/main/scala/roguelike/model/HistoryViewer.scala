package roguelike.model

import io.indigoengine.roguelike.starterkit.*
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA

final case class HistoryViewer(size: Size, position: Int, window: TerminalEmulator):

  def withPosition(newPosition: Int): HistoryViewer =
    this.copy(
      position = newPosition
    )

  def scrollUp: HistoryViewer =
    withPosition(if position - 1 >= 0 then position - 1 else position)
  def scrollDown(lineCount: Int): HistoryViewer =
    val innerHeight: Int = size.height - 2
    withPosition(
      if lineCount > innerHeight then
        if position + 1 <= lineCount - innerHeight then position + 1 else lineCount - innerHeight
      else
        position
    )

  def toTerminal(log: MessageLog): TerminalEmulator =
    val logTerm = log.toTerminal(size - 2, true, position, false)
    window.inset(logTerm, Point(1, 1))

object HistoryViewer:

  val WindowTitle: String = "┤ Message History ├"
  val MinWindowSize: Size = Size(WindowTitle.length + 4, 5)

  def apply(size: Size): HistoryViewer =
    val checkSize = size.max(MinWindowSize)
    HistoryViewer(checkSize, 0, createWindow(checkSize))

  def createWindow(size: Size): TerminalEmulator =
    val checkSize   = size.max(MinWindowSize)
    val titleBar    = List.fill(checkSize.width - 2 - WindowTitle.length)("─").mkString
    val middleSpace = List.fill(checkSize.width - 2)(" ").mkString
    val bottomBar   = List.fill(checkSize.width - 2)("─").mkString
    val header      = "┌" + WindowTitle + titleBar + "┐"
    val middle      = "│" + middleSpace + "│"
    val footer      = "└" + bottomBar + "┘"
    val bgColor     = RGBA.Black.withAlpha(0.5)

    val windowTerm =
      TerminalEmulator(checkSize)
        .putLine(Point(0, 0), header, RGB.White, bgColor)
        .putLines(Point(0, 1), List.fill(checkSize.height - 2)(middle), RGB.White, bgColor)
        .putLine(Point(0, checkSize.height - 1), footer, RGB.White, bgColor)

    windowTerm
