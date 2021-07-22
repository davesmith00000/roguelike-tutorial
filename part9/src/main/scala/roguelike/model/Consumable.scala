package roguelike.model

import roguelike.terminal.MapTile
import roguelike.DfTiles
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB
import roguelike.ColorScheme
import indigo.shared.Outcome
import roguelike.GameEvent

sealed trait Consumable:
  def name: String
  def tile: MapTile

object Consumable:

  final case class HealthPotion(amount: Int) extends Consumable:
    val name: String  = "Health Potion"
    val tile: MapTile = MapTile(DfTiles.Tile.`!`, RGB(0.5, 0.0, 1.0))

    def action(player: Player): Outcome[ConsumeAttempt] =
      val possibleAmount =
        player.fighter.maxHp - player.fighter.hp
      val amountRecovered =
        Math.min(possibleAmount, amount)

      if amountRecovered <= 0 then
        val msg = Message("Your health is already full.", ColorScheme.impossible)
        Outcome(ConsumeAttempt(player, false)).addGlobalEvents(GameEvent.Log(msg))
      else
        val msg = Message(s"You consume the $name, and recover $amountRecovered", ColorScheme.healthRecovered)
        Outcome(ConsumeAttempt(player.heal(amountRecovered), true))
          .addGlobalEvents(
            GameEvent.Log(msg),
            GameEvent.PlayerTurnEnd
          )

final case class ConsumeAttempt(player: Player, consumed: Boolean)
