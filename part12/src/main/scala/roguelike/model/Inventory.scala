package roguelike.model

import roguelike.ColorScheme
import indigo.shared.Outcome
import roguelike.GameEvent
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.Point

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

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

  def consume(itemAt: Int, player: Player, visibleHostiles: List[Hostile]): Outcome[(Inventory, Player)] =
    val remove: (Int, List[Item]) => List[Item] = (at, items) =>
      val (start, end) = items.splitAt(itemAt)
      start ++ end.drop(1)

    items.lift(itemAt) match
      case None =>
        Outcome((this, player))
          .addGlobalEvents(GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid)))

      case Some(Item(_, potion @ Consumable.HealthPotion(_))) =>
        Consumable.useHealthPotion(potion, player).map { attempt =>
          if attempt._2 then (this.copy(items = remove(itemAt, items)), attempt._1)
          else (this, player)
        }

      case Some(Item(_, scroll @ Consumable.LightningScroll(_, _))) =>
        Consumable.useLightningScroll(scroll, player, visibleHostiles).map { consumed =>
          if consumed then (this.copy(items = remove(itemAt, items)), player)
          else (this, player)
        }

      case Some(Item(_, Consumable.ConfusionScroll(_))) =>
        Outcome((this, player))
          .addGlobalEvents(GameEvent.TargetUsingItem(itemAt, 0))

      case Some(Item(_, Consumable.FireBallScroll(_, radius))) =>
        Outcome((this, player))
          .addGlobalEvents(GameEvent.TargetUsingItem(itemAt, radius))

  def consumeTargeted(
      itemAt: Int,
      player: Player,
      target: Hostile,
      hostiles: List[Hostile]
  ): Outcome[(Inventory, Player)] =
    val remove: (Int, List[Item]) => List[Item] = (at, items) =>
      val (start, end) = items.splitAt(itemAt)
      start ++ end.drop(1)

    items.lift(itemAt) match
      case None =>
        Outcome((this, player))
          .addGlobalEvents(GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid)))

      case Some(Item(_, Consumable.HealthPotion(_))) =>
        Outcome((this, player))

      case Some(Item(_, Consumable.LightningScroll(_, _))) =>
        Outcome((this, player))

      case Some(Item(_, scroll @ Consumable.ConfusionScroll(_))) =>
        Consumable.useConfusionScroll(scroll, player, target).map { consumed =>
          if consumed then (this.copy(items = remove(itemAt, items)), player)
          else (this, player)
        }

      case Some(Item(_, scroll @ Consumable.FireBallScroll(_, _))) =>
        Consumable.useFireballScroll(scroll, player, target, hostiles).map { consumed =>
          if consumed then (this.copy(items = remove(itemAt, items)), player)
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

object Inventory:

  given Encoder[Inventory] = new Encoder[Inventory] {
    final def apply(data: Inventory): Json = Json.obj(
      ("capacity", Json.fromInt(data.capacity)),
      ("items", data.items.asJson)
    )
  }

  given Decoder[Inventory] = new Decoder[Inventory] {
    final def apply(c: HCursor): Decoder.Result[Inventory] =
      for {
        capacity <- c.downField("capacity").as[Int]
        items    <- c.downField("items").as[List[Item]]
      } yield Inventory(capacity, items)
  }
