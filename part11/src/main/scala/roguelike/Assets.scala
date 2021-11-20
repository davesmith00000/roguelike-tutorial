package roguelike

import indigo._

object Assets:

  val tileMap: AssetName        = AssetName("Anikki_square_10x10")
  val menuBackground: AssetName = AssetName("menu_background")

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(tileMap, AssetPath("assets/" + tileMap.toString + ".png")),
      AssetType.Image(menuBackground, AssetPath("assets/" + menuBackground.toString + ".png"))
    )

end Assets
