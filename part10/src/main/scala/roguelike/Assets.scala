package roguelike

import indigo._

object Assets:

  val tileMap: AssetName        = AssetName("Anikki_square_10x10")
  val menuBackground: AssetName = AssetName("menu_background")

  val assets: Set[AssetType] =
    Required.assets ++ Set(
      AssetType.Image(tileMap, AssetPath("assets/" + tileMap.toString + ".png")),
      AssetType.Image(menuBackground, AssetPath("assets/" + menuBackground.toString + ".png"))
    )

  object Required:
    val mapFragShader  = AssetName("map frag")
    val textFragShader = AssetName("text frag")

    val assets: Set[AssetType] =
      Set(
        AssetType.Text(textFragShader, AssetPath("assets/shaders/text.frag")),
        AssetType.Text(mapFragShader, AssetPath("assets/shaders/map.frag"))
      )
  end Required

end Assets
