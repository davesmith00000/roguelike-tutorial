package roguelike.model

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class Equipment(weapon: Option[Weapon], armour: Option[Armour]):

  def defenseBonus: Int =
    armour.map(_.defenseBonus).getOrElse(0)

  def powerBonus: Int =
    weapon.map(_.powerBonus).getOrElse(0)

object Equipment:
  val initial: Equipment =
    Equipment(Option(Consumable.Dagger.default), Option(Consumable.LeatherArmor.default))

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
