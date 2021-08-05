package roguelike.model

import indigo.Point
import indigo.Dice
import indigo.Rectangle
import indigo.Size

class GameMapTests extends munit.FunSuite {

  val walkable: List[Point] =
    List(
      Point(4, 6),
      Point(3, 6),
      Point(4, 5),
      Point(3, 5),
      Point(4, 4),
      Point(3, 4),
      Point(4, 3),
      Point(3, 3),
      Point(3, 2),
      Point(2, 6),
      Point(1, 6),
      Point(0, 6),
      Point(2, 5),
      Point(1, 5),
      Point(0, 5),
      Point(2, 4),
      Point(1, 4),
      Point(2, 3),
      Point(1, 3),
      Point(0, 4),
      Point(0, 3),
      Point(2, 2),
      Point(3, 1),
      Point(3, 0),
      Point(2, 1),
      Point(2, 0)
    ).map(_ + Point(17, 21))

  test("getPathTo") {
    val actual =
      GameMap.getPathTo(Dice.fromSeed(0), Point(20, 23), Point(19, 26), walkable, Rectangle(Point(17, 21), Size(5, 7)))

    val possiblePaths: List[List[Point]] =
      List(
        List(Point(20, 23), Point(19, 23), Point(19, 24), Point(19, 25), Point(19, 26)),
        List(Point(20, 23), Point(20, 24), Point(20, 25), Point(19, 25), Point(19, 26))
      )

    assert(possiblePaths.contains(actual))
  }

}
