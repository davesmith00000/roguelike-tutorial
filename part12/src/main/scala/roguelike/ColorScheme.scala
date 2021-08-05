package roguelike

import indigo.shared.datatypes.RGB

object ColorScheme:

  val white: RGB = RGB.White
  val black: RGB = RGB.Black
  val red: RGB   = RGB.Red

  val playerAttack: RGB        = RGB.fromColorInts(0xe0, 0xe0, 0xe0)
  val enemyAttack: RGB         = RGB.fromColorInts(0xff, 0xc0, 0xc0)
  val needsTarget: RGB         = RGB.fromColorInts(0x3f, 0xff, 0xff)
  val statusEffectApplied: RGB = RGB.fromColorInts(0x3f, 0xff, 0x3f)
  val descend: RGB             = RGB.fromColorInts(0x9f, 0x3f, 0xff)

  val playerDie: RGB = RGB.fromColorInts(0xff, 0x30, 0x30)
  val enemyDie: RGB  = RGB.fromColorInts(0xff, 0xa0, 0x30)

  val invalid: RGB    = RGB.Yellow
  val impossible: RGB = RGB.fromHexString("808080")
  val error: RGB      = RGB.fromHexString("FF4040")

  val welcomeText: RGB     = RGB.fromHexString("20A0FF")
  val healthRecovered: RGB = RGB.Green

  val barText: RGB   = RGB.White
  val barFilled: RGB = RGB.fromHexString("006000")
  val barEmpty: RGB  = RGB.fromHexString("401010")
