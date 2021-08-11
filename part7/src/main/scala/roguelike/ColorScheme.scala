package roguelike

import indigo.shared.datatypes.RGB

object ColorScheme:

  val welcomeText: RGB = RGB.fromHexString("20A0FF")

  val barText: RGB   = RGB.White
  val barFilled: RGB = RGB.fromHexString("006000")
  val barEmpty: RGB  = RGB.fromHexString("401010")

  val healthRecovered: RGB = RGB.fromColorInts(0x0, 0xFF, 0x0)
