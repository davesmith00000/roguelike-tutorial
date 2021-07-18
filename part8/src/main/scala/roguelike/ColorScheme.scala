package roguelike

import indigo.shared.datatypes.RGB

object ColorScheme:

  val invalid: RGB    = RGB.Yellow
  val impossible: RGB = RGB.fromHexString("808080")
  val error: RGB      = RGB.fromHexString("FF4040")

  val welcomeText: RGB     = RGB.fromHexString("20A0FF")
  val healthRecovered: RGB = RGB.Green

  val barText: RGB   = RGB.White
  val barFilled: RGB = RGB.fromHexString("006000")
  val barEmpty: RGB  = RGB.fromHexString("401010")
