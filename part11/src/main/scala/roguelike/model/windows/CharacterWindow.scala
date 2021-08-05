package roguelike.model.windows

import roguelike.terminal.TerminalEmulator
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import roguelike.model.Player

final case class CharacterWindow(size: Size, window: TerminalEmulator) extends Window:

  def toTerminal(player: Player): TerminalEmulator =
    val innerSize = size - 2

    val term =
      TerminalEmulator(innerSize)
        .putLine(Point(0, 1), s"Level: ${player.level}", RGB.White, RGBA.Black)
        .putLine(Point(0, 2), s"XP: ${player.xp}", RGB.White, RGBA.Black)
        .putLine(Point(0, 3), s"XP for next level: ${player.experienceToNextLevel}", RGB.White, RGBA.Black)
        .putLine(Point(0, 4), s"Attack: ${player.fighter.power}", RGB.White, RGBA.Black)
        .putLine(Point(0, 5), s"Defense: ${player.fighter.defense}", RGB.White, RGBA.Black)

    window.inset(term, Point(1, 1))

object CharacterWindow:

  val WindowTitle: String = "┤ Character Information ├"
  val MinWindowSize: Size = Size(WindowTitle.length + 4, 9)

  def create: CharacterWindow =
    val checkSize = MinWindowSize
    CharacterWindow(checkSize, Window.createWindow(checkSize, WindowTitle))
