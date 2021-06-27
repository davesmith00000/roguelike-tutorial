// package roguelike.model

// import indigo._
// import indigoextras.trees.QuadTree
// import indigoextras.trees.QuadTree.{QuadBranch, QuadEmpty, QuadLeaf}
// import indigoextras.geometry.Vertex

// import roguelike.DfTiles
// import scala.annotation.tailrec

// final case class GameMap(size: Size, map: QuadTree[MapTile]):

//   private val coordsList: List[Point] =
//     (0 until screenSize.width).flatMap { x =>
//       (0 until screenSize.height).map { y =>
//         Point(x, y)
//       }
//     }.toList

//   def put(coords: Point, tile: DfTiles.Tile, fgColor: RGB, bgColor: RGBA): ConsoleEmulator =
//     this.copy(charMap = charMap.insertElement(MapTile(tile, fgColor, bgColor), Vertex.fromPoint(coords)))
//   def put(coords: Point, tile: DfTiles.Tile, fgColor: RGB): ConsoleEmulator =
//     this.copy(charMap = charMap.insertElement(MapTile(tile, fgColor), Vertex.fromPoint(coords)))
//   def put(coords: Point, tile: DfTiles.Tile): ConsoleEmulator =
//     this.copy(charMap = charMap.insertElement(MapTile(tile), Vertex.fromPoint(coords)))

//   def put(tiles: List[(Point, MapTile)]): ConsoleEmulator =
//     this.copy(charMap = charMap.insertElements(tiles.map(p => (p._2, Vertex.fromPoint(p._1)))))
//   def put(tiles: (Point, MapTile)*): ConsoleEmulator =
//     put(tiles.toList)

//   def get(coords: Point): Option[MapTile] =
//     charMap.fetchElementAt(Vertex.fromPoint(coords))

//   def draw(default: MapTile): List[MapTile] =
//     coordsList.map(pt => get(pt).getOrElse(default))

//   def toList: List[MapTile] =
//     @tailrec
//     def rec(open: List[QuadTree[MapTile]], acc: List[MapTile]): List[MapTile] =
//       open match
//         case Nil =>
//           acc

//         case x :: xs =>
//           x match {
//             case _: QuadEmpty[MapTile] =>
//               rec(xs, acc)

//             case l: QuadLeaf[MapTile] =>
//               rec(xs, l.value :: acc)

//             case b: QuadBranch[MapTile] if b.isEmpty =>
//               rec(xs, acc)

//             case QuadBranch(_, a, b, c, d) =>
//               val next =
//                 (if a.isEmpty then Nil else List(a)) ++
//                   (if b.isEmpty then Nil else List(b)) ++
//                   (if c.isEmpty then Nil else List(c)) ++
//                   (if d.isEmpty then Nil else List(d))

//               rec(xs ++ next, acc)
//           }

//     rec(List(charMap), Nil)

//   def toPositionedList: List[(Point, MapTile)] =
//     @tailrec
//     def rec(open: List[QuadTree[MapTile]], acc: List[(Point, MapTile)]): List[(Point, MapTile)] =
//       open match
//         case Nil =>
//           acc

//         case x :: xs =>
//           x match {
//             case _: QuadEmpty[MapTile] =>
//               rec(xs, acc)

//             case l: QuadLeaf[MapTile] =>
//               rec(xs, (l.exactPosition.toPoint, l.value) :: acc)

//             case b: QuadBranch[MapTile] if b.isEmpty =>
//               rec(xs, acc)

//             case QuadBranch(_, a, b, c, d) =>
//               val next =
//                 (if a.isEmpty then Nil else List(a)) ++
//                   (if b.isEmpty then Nil else List(b)) ++
//                   (if c.isEmpty then Nil else List(c)) ++
//                   (if d.isEmpty then Nil else List(d))

//               rec(xs ++ next, acc)
//           }

//     rec(List(charMap), Nil)

//   def combine(otherConsole: ConsoleEmulator): ConsoleEmulator =
//     this.copy(
//       charMap = charMap.insertElements(otherConsole.toPositionedList.map(p => (p._2, Vertex.fromPoint(p._1))))
//     )

// object ConsoleEmulator:
//   def apply(screenSize: Size): ConsoleEmulator =
//     ConsoleEmulator(screenSize, QuadTree.empty[MapTile](screenSize.width.toDouble, screenSize.height.toDouble))
//     ConsoleEmulator(screenSize, QuadTree.empty[MapTile](screenSize.width.toDouble, screenSize.height.toDouble))
