package roguelike.model

import roguelike.ColorScheme
import indigo.shared.Outcome
import roguelike.GameEvent
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.Point

final case class Inventory(capacity: Int, items: List[Item]):

  def add(item: Item): Outcome[(Inventory, Boolean)] =
    val newItems = items :+ item

    if newItems.length > capacity then
      Outcome((this, false))
        .addGlobalEvents(GameEvent.Log(Message("Your inventory is full.", ColorScheme.impossible)))
    else
      Outcome(
        (
          this.copy(
            items = newItems
          ),
          true
        )
      ).addGlobalEvents(
        GameEvent.Log(Message(s"You picked up the ${item.name}!", RGB(0.0, 0.85, 0.0))),
        GameEvent.PlayerTurnEnd
      )

  def consume(itemAt: Int, player: Player): Outcome[(Inventory, Player)] =
    items.lift(itemAt) match
      case None =>
        Outcome((this, player))
          .addGlobalEvents(GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid)))

      case Some(Item(_, potion @ Consumable.HealthPotion(_))) =>
        potion.action(player).map { attempt =>
          if attempt.consumed then
            val (start, end) = items.splitAt(itemAt)

            val next = this.copy(
              items = start ++ end.drop(1)
            )
            (next, attempt.player)
          else (this, player)
        }

  def drop(itemAt: Int): Outcome[(Inventory, Option[Item])] =
    items.lift(itemAt) match
      case None =>
        Outcome((this, None))
          .addGlobalEvents(GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid)))

      case item @ Some(_) =>
        val (start, end) = items.splitAt(itemAt)
        val next = this.copy(
          items = start ++ end.drop(1)
        )
        Outcome((next, item))
          .addGlobalEvents(
            GameEvent.Log(Message(s"You dropped the ${item.map(_.name).getOrElse("<name missing!>")}.", RGB.White)),
            GameEvent.PlayerTurnEnd
          )
