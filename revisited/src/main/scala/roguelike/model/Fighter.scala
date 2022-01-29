package roguelike.model

import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe._
import io.circe.syntax._

/** Fighter class
  * @param hp
  *   hp represents the entity’s hit points
  * @param maxHp
  *   is the maximum hp allowed
  * @param defense
  *   defense is how much taken damage will be reduced
  * @param power
  *   power is the entity’s raw attack power
  */
final case class Fighter(hp: Int, maxHp: Int, defense: Int, power: Int):
  def withHp(value: Int): Fighter =
    this.copy(hp = Math.max(0, Math.min(value, maxHp)))

  def takeDamage(amount: Int): Fighter =
    this.copy(hp = hp - amount)

  def heal(amount: Int): Fighter =
    this.copy(hp = hp + amount)

object Fighter:
  def apply(hp: Int, defense: Int, power: Int): Fighter =
    Fighter(hp, hp, defense, power)

  given Encoder[Fighter] = new Encoder[Fighter] {
    final def apply(data: Fighter): Json = Json.obj(
      ("hp", Json.fromInt(data.hp)),
      ("maxHp", Json.fromInt(data.maxHp)),
      ("defense", Json.fromInt(data.defense)),
      ("power", Json.fromInt(data.power))
    )
  }

  given Decoder[Fighter] = new Decoder[Fighter] {
    final def apply(c: HCursor): Decoder.Result[Fighter] =
      for {
        hp      <- c.downField("hp").as[Int]
        maxHp   <- c.downField("maxHp").as[Int]
        defense <- c.downField("defense").as[Int]
        power   <- c.downField("power").as[Int]
      } yield Fighter(hp, maxHp, defense, power)
  }
