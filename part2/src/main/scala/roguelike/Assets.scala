package roguelike

import indigo._

object Assets:

  val tileMap = AssetName("Anikki_square_10x10")

  val assets: Set[AssetType] =
    Required.assets ++ Set(
      AssetType.Image(tileMap, AssetPath("assets/" + tileMap.toString + ".png"))
    )

  object Required:
    val mapFragShader  = AssetName("map frag")
    val mapVertShader  = AssetName("map vert")
    val textFragShader = AssetName("text frag")

    val assets: Set[AssetType] =
      Set(
        AssetType.Text(textFragShader, AssetPath("assets/shaders/text.frag")),
        AssetType.Text(mapVertShader, AssetPath("assets/shaders/map.vert")),
        AssetType.Text(mapFragShader, AssetPath("assets/shaders/map.frag"))
      )
  end Required

end Assets
