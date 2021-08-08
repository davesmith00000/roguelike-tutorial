package roguelike.model

import indigo._

class DungeonGenTests extends munit.FunSuite {

  test("random choices floor 0") {
    val chances =
      DungeonGen.itemChances

    val count: Int = 50
    val floor: Int = 0

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(123), count, floor, chances)

    assert(actual.length == count)
    assert(actual.forall(_.name == Consumable.HealthPotion.name))
  }

  test("random choices floor 2") {
    val chances =
      DungeonGen.itemChances

    val count: Int = 50
    val floor: Int = 2

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(456), count, floor, chances)

    val possibilities = List(
      Consumable.HealthPotion.name,
      Consumable.ConfusionScroll.name
    )

    assert(actual.length == count)
    assert(actual.forall(p => possibilities.contains(p)))
  }

  test("random choices floor 4") {
    val chances =
      DungeonGen.itemChances

    val count: Int = 50
    val floor: Int = 4

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(789), count, floor, chances)

    val possibilities = List(
      Consumable.HealthPotion.name,
      Consumable.ConfusionScroll.name,
      Consumable.LightningScroll.name,
      Consumable.Sword.name
    )

    assert(actual.length == count)
    assert(actual.forall(p => possibilities.contains(p)))
  }

  test("random choices floor 6") {
    val chances =
      DungeonGen.itemChances

    val count: Int = 50
    val floor: Int = 6

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(6181151), count, floor, chances)

    val possibilities = List(
      Consumable.HealthPotion.name,
      Consumable.ConfusionScroll.name,
      Consumable.LightningScroll.name,
      Consumable.FireBallScroll.name,
      Consumable.Sword.name,
      Consumable.ChainMail.name
    )

    assert(actual.length == count)
    assert(actual.forall(p => possibilities.contains(p)))
  }

}
