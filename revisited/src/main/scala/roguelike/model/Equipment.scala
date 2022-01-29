package roguelike.model

import indigo.Dice
import indigo.Outcome
import indigo.RGB
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe._
import io.circe.syntax._
import roguelike.GameEvent

final case class Equipment(weapon: Option[Weapon], armour: Option[Armour]):

  def defenseBonus: Int =
    armour.map(_.defenseBonus).getOrElse(0)

  def powerBonus: Int =
    weapon.map(_.powerBonus).getOrElse(0)

  def itemIsEquipped(equipment: Weapon | Armour): Boolean =
    equipment match
      case w: Weapon =>
        weapon.map(_.id == w.id).getOrElse(false)

      case a: Armour =>
        armour.map(_.id == a.id).getOrElse(false)

  def equip(equipment: Weapon | Armour): Outcome[Equipment] =
    equipment match
      case w: Weapon =>
        Outcome(this.copy(weapon = Option(w)))
          .addGlobalEvents(
            GameEvent.Log(Message(s"You equip the ${w.name}.", RGB.White))
          )

      case a: Armour =>
        Outcome(this.copy(armour = Option(a)))
          .addGlobalEvents(
            GameEvent.Log(Message(s"You equip the ${a.name}.", RGB.White))
          )

  def unequip(equipment: Weapon | Armour): Outcome[Equipment] =
    equipment match
      case w: Weapon if itemIsEquipped(w) =>
        unequipWeapon

      case a: Armour if itemIsEquipped(a) =>
        unequipArmour

      case _ =>
        Outcome(this)

  def unequipArmour: Outcome[Equipment] =
    val msg =
      armour match
        case None => Nil
        case Some(a) =>
          List(GameEvent.Log(Message(s"You remove the ${a.name}.", RGB.White)))

    Outcome(this.copy(armour = None))
      .addGlobalEvents(msg)

  def unequipWeapon: Outcome[Equipment] =
    val msg =
      weapon match
        case None => Nil
        case Some(w) =>
          List(GameEvent.Log(Message(s"You remove the ${w.name}.", RGB.White)))

    Outcome(this.copy(weapon = None))
      .addGlobalEvents(msg)

object Equipment:
  given Encoder[Equipment] = new Encoder[Equipment] {
    final def apply(data: Equipment): Json = Json.obj(
      ("weapon", data.weapon.asJson),
      ("armour", data.armour.asJson)
    )
  }

  given Decoder[Equipment] = new Decoder[Equipment] {
    final def apply(c: HCursor): Decoder.Result[Equipment] =
      for {
        weapon <- c.downField("weapon").as[Option[Weapon]]
        armour <- c.downField("armour").as[Option[Armour]]
      } yield Equipment(weapon, armour)
  }
