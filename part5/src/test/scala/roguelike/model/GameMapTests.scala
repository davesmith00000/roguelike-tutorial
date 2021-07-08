package roguelike.model

import indigo.Point

class GameMapTests extends munit.FunSuite {

  test("bresenhams line - 1x1") {
    val actual =
      GameMap.bresenhamLine(Point(0, 0), Point(1, 1))

    val expected =
      List(Point(1, 1), Point(0, 0))

    assertEquals(actual, expected)
  }

  test("bresenhams line - vertical") {
    val actual =
      GameMap.bresenhamLine(Point(10, 13), Point(10, 21))

    val expected =
      List(
        Point(10, 21),
        Point(10, 20),
        Point(10, 19),
        Point(10, 18),
        Point(10, 17),
        Point(10, 16),
        Point(10, 15),
        Point(10, 14),
        Point(10, 13)
      )

    assertEquals(actual, expected)
  }

  test("bresenhams line - horizontal") {
    val actual =
      GameMap.bresenhamLine(Point(7, 13), Point(12, 13))

    val expected =
      List(
        Point(12, 13),
        Point(11, 13),
        Point(10, 13),
        Point(9, 13),
        Point(8, 13),
        Point(7, 13)
      )

    assertEquals(actual, expected)
  }

  test("bresenhams line - diagonal") {
    val actual =
      GameMap.bresenhamLine(Point(0, 1), Point(6, 4))

    val expected =
      List(
        Point(6, 4),
        Point(5, 4),
        Point(4, 3),
        Point(3, 3),
        Point(2, 2),
        Point(1, 2),
        Point(0, 1)
      )

    assertEquals(actual, expected)
  }

  test("bresenhams line - same") {
    val actual =
      GameMap.bresenhamLine(Point(0, 1), Point(0, 1))

    val expected =
      List(
        Point(0, 1)
      )

    assertEquals(actual, expected)
  }

  test("bresenhams line - tricky - misses target") {
    val actual =
      GameMap.bresenhamLine(Point(8, 3), Point(5, 5))

    val expected =
      List(
        Point(8, 3),
        Point(7, 4),
        Point(6, 5)
      ).reverse

    assertEquals(actual, expected)
  }

}
