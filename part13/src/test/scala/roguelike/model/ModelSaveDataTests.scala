package roguelike.model

import indigo.shared.datatypes.Size
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB
import indigoextras.trees.QuadTree

import io.circe._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.parser.decode

class ModelSaveDataTests extends munit.FunSuite {

  import SharedCodecs.given

  test("json - all data") {
    val actual =
      ModelSaveData.fromJsonString(ModelSaveDataSamples.saveData.toJsonString)

    val expected =
      Some(ModelSaveDataSamples.saveData)

    assertEquals(actual, expected)
  }

  test("json - inventory") {
    val actual =
      decode[Inventory](ModelSaveDataSamples.inventory.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.inventory)

    assertEquals(actual, expected)
  }

  test("json - item") {
    val item = Item(Point(1), Consumable.HealthPotion(1))

    val actual =
      decode[Item](item.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(item)

    assertEquals(actual, expected)
  }

  test("json - consumable") {
    val consumable: Consumable = Consumable.HealthPotion(1)

    val actual =
      decode[Consumable](consumable.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(consumable)

    assertEquals(actual, expected)
  }

  test("json - gameMap") {
    val actual =
      decode[GameMap](ModelSaveDataSamples.gameMap.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.gameMap)

    assertEquals(actual, expected)
  }

  test("json - player") {
    val actual =
      decode[Player](ModelSaveDataSamples.player.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.player)

    assertEquals(actual, expected)
  }

  test("json - messageLog") {
    val actual =
      decode[MessageLog](ModelSaveDataSamples.messageLog.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.messageLog)

    assertEquals(actual, expected)
  }

  test("json - size") {
    val actual =
      decode[Size](ModelSaveDataSamples.size.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.size)

    assertEquals(actual, expected)
  }

}

object ModelSaveDataSamples:

  val inventory: Inventory =
    Inventory(
      3,
      List(
        Item(Point(1), Consumable.HealthPotion(1)),
        Item(Point(2), Consumable.LightningScroll(2, 3)),
        Item(Point(3), Consumable.FireBallScroll(4, 5)),
        Item(Point(4), Consumable.ConfusionScroll(6))
      )
    )

  val gameMap: GameMap =
    GameMap(
      Size(80, 50),
      QuadTree.empty[GameTile](80, 50),
      List(Point(1), Point(2), Point(3)),
      Set(Point(1), Point(2), Point(3), Point(4)),
      List(
        Orc(1, Point(1), true, Fighter(2, 3, 4, 5), List(Point(2)), HostileState.Confused(3)),
        Troll(2, Point(2), false, Fighter(3, 4, 5, 6), List(Point(3)), HostileState.Normal)
      ),
      List(
        Item(Point(5), Consumable.HealthPotion(7)),
        Item(Point(6), Consumable.LightningScroll(8, 9)),
        Item(Point(7), Consumable.FireBallScroll(10, 11)),
        Item(Point(8), Consumable.ConfusionScroll(12))
      )
    ).insert(Point(10), GameTile.Wall)
      .insert(Point(20), GameTile.Ground)

  val size: Size =
    Size(80, 50)

  val player: Player =
    Player(Point(5, 6), true, Fighter(1, 2, 3, 4), inventory, 2, 350)

  val stairsPosition: Point =
    Point(10, 10)

  val messageLog: MessageLog =
    MessageLog(
      List(
        Message("a", RGB.Red),
        Message("b", RGB.Green),
        Message("c", RGB.Blue)
      ),
      None
    )

  val currentLevel: Int =
    1

  val saveData: ModelSaveData =
    ModelSaveData(
      size,
      player,
      stairsPosition,
      gameMap,
      messageLog,
      currentLevel
    )
