package roguelike.model.windows

import io.indigoengine.roguelike.starterkit.*
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA

trait Window:
  def size: Size
  def window: TerminalEmulator

object Window:
  def createWindow(size: Size, title: String): TerminalEmulator =
    val checkSize   = size
    val titleBar    = List.fill(checkSize.width - 2 - title.length)("─").mkString
    val middleSpace = List.fill(checkSize.width - 2)(" ").mkString
    val bottomBar   = List.fill(checkSize.width - 2)("─").mkString
    val header      = "┌" + title + titleBar + "┐"
    val middle      = "│" + middleSpace + "│"
    val footer      = "└" + bottomBar + "┘"
    val bgColor     = RGBA.Black.withAlpha(0.5)

    val windowTerm =
      TerminalEmulator(checkSize)
        .putLine(Point(0, 0), header, RGB.White, bgColor)
        .putLines(Point(0, 1), List.fill(checkSize.height - 2)(middle), RGB.White, bgColor)
        .putLine(Point(0, checkSize.height - 1), footer, RGB.White, bgColor)

    windowTerm

  val letters: List[String]             = ('a' to 'z').toList.map(_.toString)
  val letterPositions: Map[String, Int] = letters.zipWithIndex.toMap

trait ScrollingWindow extends Window:
  def position: Int

object ScrollingWindow:
  def nextScrollUp(position: Int): Int =
    if position - 1 >= 0 then position - 1 else position

  def nextScrollDown(maxHeight: Int, lineCount: Int, position: Int): Int =
    val innerHeight: Int = maxHeight
    if lineCount > innerHeight then
      if position + 1 <= lineCount - innerHeight then position + 1 else lineCount - innerHeight
    else position
