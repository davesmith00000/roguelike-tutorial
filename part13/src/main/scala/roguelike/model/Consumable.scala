package roguelike.model

import io.indigoengine.roguelike.starterkit.*
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB
import roguelike.ColorScheme
import indigo.shared.Outcome
import indigo.shared.datatypes.BindingKey
import roguelike.GameEvent

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

import scala.annotation.tailrec
import roguelike.model.Consumable.ConfusionScroll
import indigo.shared.dice.Dice

sealed trait Consumable:
  def name: String
  def tile: MapTile

sealed trait Weapon extends Consumable:
  def id: BindingKey
  def powerBonus: Int

object Weapon:

  given Encoder[Weapon] = new Encoder[Weapon] {
    final def apply(data: Weapon): Json =
      data match
        case Consumable.Dagger(id, powerBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("powerBonus", Json.fromInt(powerBonus))
          )

        case Consumable.Sword(id, powerBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("powerBonus", Json.fromInt(powerBonus))
          )
  }

  given Decoder[Weapon] = new Decoder[Weapon] {
    final def apply(c: HCursor): Decoder.Result[Weapon] =
      c.downField("name").as[String].flatMap {
        case Consumable.Dagger.name =>
          for {
            id         <- c.downField("id").as[String]
            powerBonus <- c.downField("powerBonus").as[Int]
          } yield Consumable.Dagger(BindingKey(id), powerBonus)

        case Consumable.Sword.name =>
          for {
            id         <- c.downField("id").as[String]
            powerBonus <- c.downField("powerBonus").as[Int]
          } yield Consumable.Sword(BindingKey(id), powerBonus)
      }
  }

sealed trait Armour extends Consumable:
  def id: BindingKey
  def defenseBonus: Int

object Armour:

  given Encoder[Armour] = new Encoder[Armour] {
    final def apply(data: Armour): Json =
      data match
        case Consumable.LeatherArmor(id, defenseBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("defenseBonus", Json.fromInt(defenseBonus))
          )

        case Consumable.ChainMail(id, defenseBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("defenseBonus", Json.fromInt(defenseBonus))
          )
  }

  given Decoder[Armour] = new Decoder[Armour] {
    final def apply(c: HCursor): Decoder.Result[Armour] =
      c.downField("name").as[String].flatMap {
        case Consumable.LeatherArmor.name =>
          for {
            id           <- c.downField("id").as[String]
            defenseBonus <- c.downField("defenseBonus").as[Int]
          } yield Consumable.LeatherArmor(BindingKey(id), defenseBonus)

        case Consumable.ChainMail.name =>
          for {
            id           <- c.downField("id").as[String]
            defenseBonus <- c.downField("defenseBonus").as[Int]
          } yield Consumable.ChainMail(BindingKey(id), defenseBonus)
      }
  }

object Consumable:

  given Encoder[Consumable] = new Encoder[Consumable] {
    final def apply(data: Consumable): Json =
      data match
        case HealthPotion(amount) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("amount", Json.fromInt(amount))
          )

        case LightningScroll(damage, maximumRange) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("damage", Json.fromInt(damage)),
            ("maximumRange", Json.fromInt(maximumRange))
          )

        case FireBallScroll(damage, radius) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("damage", Json.fromInt(damage)),
            ("radius", Json.fromInt(radius))
          )

        case ConfusionScroll(numberOfTurns) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("numberOfTurns", Json.fromInt(numberOfTurns))
          )

        case Dagger(id, powerBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("powerBonus", Json.fromInt(powerBonus))
          )

        case Sword(id, powerBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("powerBonus", Json.fromInt(powerBonus))
          )

        case LeatherArmor(id, defenseBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("defenseBonus", Json.fromInt(defenseBonus))
          )

        case ChainMail(id, defenseBonus) =>
          Json.obj(
            ("name", Json.fromString(data.name)),
            ("id", Json.fromString(id.toString)),
            ("defenseBonus", Json.fromInt(defenseBonus))
          )
  }

  given Decoder[Consumable] = new Decoder[Consumable] {
    final def apply(c: HCursor): Decoder.Result[Consumable] =
      c.downField("name").as[String].flatMap {
        case HealthPotion.name =>
          for {
            amount <- c.downField("amount").as[Int]
          } yield HealthPotion(amount)

        case LightningScroll.name =>
          for {
            damage       <- c.downField("damage").as[Int]
            maximumRange <- c.downField("maximumRange").as[Int]
          } yield LightningScroll(damage, maximumRange)

        case FireBallScroll.name =>
          for {
            damage <- c.downField("damage").as[Int]
            radius <- c.downField("radius").as[Int]
          } yield FireBallScroll(damage, radius)

        case ConfusionScroll.name =>
          for {
            numberOfTurns <- c.downField("numberOfTurns").as[Int]
          } yield ConfusionScroll(numberOfTurns)

        case Dagger.name =>
          for {
            id         <- c.downField("id").as[String]
            powerBonus <- c.downField("powerBonus").as[Int]
          } yield Dagger(BindingKey(id), powerBonus)

        case Sword.name =>
          for {
            id         <- c.downField("id").as[String]
            powerBonus <- c.downField("powerBonus").as[Int]
          } yield Sword(BindingKey(id), powerBonus)

        case LeatherArmor.name =>
          for {
            id           <- c.downField("id").as[String]
            defenseBonus <- c.downField("defenseBonus").as[Int]
          } yield LeatherArmor(BindingKey(id), defenseBonus)

        case ChainMail.name =>
          for {
            id           <- c.downField("id").as[String]
            defenseBonus <- c.downField("defenseBonus").as[Int]
          } yield ChainMail(BindingKey(id), defenseBonus)
      }
  }

  // Potions and spells

  final case class HealthPotion(amount: Int) extends Consumable:
    val name: String  = HealthPotion.name
    val tile: MapTile = MapTile(Tile.`!`, RGB(0.5, 0.0, 1.0))
  object HealthPotion:
    val name: String = "Health Potion"

  final case class LightningScroll(damage: Int, maximumRange: Int) extends Consumable:
    val name: String  = LightningScroll.name
    val tile: MapTile = MapTile(Tile.`!`, RGB.Cyan)
  object LightningScroll:
    val name: String = "Lightning Scroll"

  final case class FireBallScroll(damage: Int, radius: Int) extends Consumable:
    val name: String  = FireBallScroll.name
    val tile: MapTile = MapTile(Tile.`~`, RGB.Red)
  object FireBallScroll:
    val name: String = "Fireball Scroll"

  final case class ConfusionScroll(numberOfTurns: Int) extends Consumable:
    val name: String  = ConfusionScroll.name
    val tile: MapTile = MapTile(Tile.`~`, RGB.fromColorInts(207, 63, 255))
  object ConfusionScroll:
    val name: String = "Confusion Scroll"

  // Equipment

  final case class Dagger(id: BindingKey, powerBonus: Int) extends Weapon:
    val name: String  = Dagger.name
    val tile: MapTile = MapTile(Tile.`/`, RGB.fromColorInts(0, 191, 255))
  object Dagger:
    val name: String               = "Dagger"
    def create(dice: Dice): Dagger = Dagger(BindingKey.fromDice(dice), 2)

  final case class Sword(id: BindingKey, powerBonus: Int) extends Weapon:
    val name: String  = Sword.name
    val tile: MapTile = MapTile(Tile.`/`, RGB.fromColorInts(0, 191, 255))
  object Sword:
    val name: String              = "Sword"
    def create(dice: Dice): Sword = Sword(BindingKey.fromDice(dice), 4)

  final case class LeatherArmor(id: BindingKey, defenseBonus: Int) extends Armour:
    val name: String  = LeatherArmor.name
    val tile: MapTile = MapTile(Tile.`[`, RGB.fromColorInts(139, 69, 19))
  object LeatherArmor:
    val name: String                     = "Leather Armor"
    def create(dice: Dice): LeatherArmor = LeatherArmor(BindingKey.fromDice(dice), 1)

  final case class ChainMail(id: BindingKey, defenseBonus: Int) extends Armour:
    val name: String  = ChainMail.name
    val tile: MapTile = MapTile(Tile.`[`, RGB.fromColorInts(139, 69, 19))
  object ChainMail:
    val name: String                  = "Chain Mail"
    def create(dice: Dice): ChainMail = ChainMail(BindingKey.fromDice(dice), 3)

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
