package roguelike.model.windows

import io.indigoengine.roguelike.starterkit.*
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import roguelike.model.Inventory

final case class DropWindow(size: Size, position: Int, window: TerminalEmulator)
    extends ScrollingWindow:

  def withPosition(newPosition: Int): DropWindow =
    this.copy(position = newPosition)
  def scrollUp: DropWindow =
    withPosition(ScrollingWindow.nextScrollUp(position))
  def scrollDown(lineCount: Int): DropWindow =
    withPosition(
      ScrollingWindow.nextScrollDown(size.height - 2, lineCount, position)
    )

  def toTerminal(inventory: Inventory): TerminalEmulator =
    val innerSize = size - 2

    val term =
      if inventory.items.length > 0 then
        inventory.items
          .map(_.name)
          .zip(Window.letters)
          .drop(position)
          .take(innerSize.height)
          .foldLeft((TerminalEmulator(innerSize), 0)) {
            case ((t, r), (itemName, letter)) =>
              (
                t.putLine(
                  Point(0, r),
                  s"[$letter] $itemName",
                  RGB.White,
                  RGBA.Black
                ),
                r + 1
              )
          }
          ._1
      else
        TerminalEmulator(innerSize).putLine(
          Point.zero,
          "(Empty)",
          RGB.White,
          RGBA.Black
        )

    window.inset(term, Point(1, 1))

object DropWindow:

  val WindowTitle: String = "┤ Select an item to drop ├"
  val MinWindowSize: Size = Size(WindowTitle.length + 4, 5)

  def apply(size: Size): DropWindow =
    val checkSize = size.max(MinWindowSize)
    DropWindow(checkSize, 0, Window.createWindow(checkSize, WindowTitle))
