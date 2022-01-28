package roguelike.model.windows

import io.indigoengine.roguelike.starterkit.*
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import roguelike.model.Fighter

final case class LevelUpWindow(size: Size, window: TerminalEmulator)
    extends Window:

  def toTerminal(playerFigher: Fighter): TerminalEmulator =
    val innerSize = size - 2

    val term =
      TerminalEmulator(innerSize)
        .putLine(
          Point(0, 1),
          "Congratulations! You level up!",
          RGB.White,
          RGBA.Black
        )
        .putLine(
          Point(0, 2),
          "Select an attribute to increase.",
          RGB.White,
          RGBA.Black
        )
        .putLine(
          Point(0, 4),
          s"[1] Constitution (+20 HP, from ${playerFigher.maxHp})",
          RGB.White,
          RGBA.Black
        )
        .putLine(
          Point(0, 5),
          s"[2] Strength (+1 attack, from ${playerFigher.power})",
          RGB.White,
          RGBA.Black
        )
        .putLine(
          Point(0, 6),
          s"[3] Agility (+1 defense, from ${playerFigher.defense})",
          RGB.White,
          RGBA.Black
        )

    window.inset(term, Point(1, 1))

object LevelUpWindow:

  val WindowTitle: String = "┤ Level Up! ├"
  val MinWindowSize: Size = Size(40 + 4, 10)

  def create: LevelUpWindow =
    val checkSize = MinWindowSize
    LevelUpWindow(checkSize, Window.createWindow(checkSize, WindowTitle))
