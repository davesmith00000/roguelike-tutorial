package roguelike.model

import roguelike.terminal.MapTile
import roguelike.DfTiles
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB
import roguelike.ColorScheme
import indigo.shared.Outcome
import roguelike.GameEvent

import scala.annotation.tailrec

sealed trait Consumable:
  def name: String
  def tile: MapTile

object Consumable:

  final case class HealthPotion(amount: Int) extends Consumable:
    val name: String  = "Health Potion"
    val tile: MapTile = MapTile(DfTiles.Tile.`!`, RGB(0.5, 0.0, 1.0))

  final case class LightningScroll(damage: Int, maximumRange: Int) extends Consumable:
    val name: String  = "Lightning Scroll"
    val tile: MapTile = MapTile(DfTiles.Tile.`!`, RGB.Cyan)

  final case class FireBallScroll(damage: Int, radius: Int) extends Consumable:
    val name: String  = "Fireball Scroll"
    val tile: MapTile = MapTile(DfTiles.Tile.`~`, RGB.Red)

  final case class ConfusionScroll(numberOfTurns: Int) extends Consumable:
    val name: String  = "Confusion Scroll"
    val tile: MapTile = MapTile(DfTiles.Tile.`~`, RGB.fromColorInts(207, 63, 255))

  def useHealthPotion(healthPotion: HealthPotion, player: Player): Outcome[(Player, Boolean)] =
    val possibleAmount =
      player.fighter.maxHp - player.fighter.hp
    val amountRecovered =
      Math.min(possibleAmount, healthPotion.amount)

    if amountRecovered <= 0 then
      val msg = Message("Your health is already full.", ColorScheme.impossible)
      Outcome((player, false)).addGlobalEvents(GameEvent.Log(msg))
    else
      val msg =
        Message(s"You consume the ${healthPotion.name}, and recover $amountRecovered", ColorScheme.healthRecovered)
      Outcome((player.heal(amountRecovered), true))
        .addGlobalEvents(
          GameEvent.Log(msg),
          GameEvent.PlayerTurnEnd
        )

  def useLightningScroll(scroll: LightningScroll, player: Player, hostiles: List[Hostile]): Outcome[Boolean] =
    @tailrec
    def findClosest(remaining: List[Hostile], closestDistance: Int, acc: Option[Hostile]): Option[Hostile] =
      remaining match
        case Nil =>
          acc

        case x :: xs =>
          val dist = x.position.distanceTo(player.position).toInt
          if dist < closestDistance then findClosest(xs, dist, Option(x))
          else findClosest(xs, closestDistance, acc)

    findClosest(hostiles, scroll.maximumRange + 1, None) match
      case None =>
        Outcome(false)
          .addGlobalEvents(GameEvent.Log(Message("No enemy is close enough to strike.", ColorScheme.impossible)))

      case Some(target) =>
        val msg = s"A lighting bolt strikes the ${target.name} with a loud thunder, for ${scroll.damage} damage!"
        Outcome(true)
          .addGlobalEvents(
            GameEvent.Log(Message(msg, ColorScheme.playerAttack)),
            GameEvent.PlayerAttack(player.name, scroll.damage, target.id)
          )

  def useConfusionScroll(scroll: ConfusionScroll, player: Player, target: Hostile): Outcome[Boolean] =
    val msg = s"The eyes of the ${target.name} look vacant, as it starts to stumble around!"
    Outcome(true)
      .addGlobalEvents(
        GameEvent.Log(Message(msg, ColorScheme.playerAttack)),
        GameEvent.PlayerCastsConfusion(player.name, scroll.numberOfTurns, target.id)
      )

  def useFireballScroll(
      scroll: FireBallScroll,
      player: Player,
      target: Hostile,
      hostiles: List[Hostile]
  ): Outcome[Boolean] =
    val events =
      hostiles.filter(_.position.distanceTo(target.position).toInt <= scroll.radius).flatMap { h =>
        List(
          GameEvent.Log(
            Message(
              s"The ${h.name} is engulfed in a fiery explosion, taking ${scroll.damage} damage!",
              ColorScheme.playerAttack
            )
          ),
          GameEvent.PlayerCastsFireball(player.name, scroll.damage, h.id)
        )
      }

    Outcome(true)
      .addGlobalEvents(events)
