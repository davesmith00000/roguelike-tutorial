package roguelike.model

import roguelike.ColorScheme
import indigo.shared.Outcome
import roguelike.GameEvent
import indigo.shared.datatypes.RGB

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
