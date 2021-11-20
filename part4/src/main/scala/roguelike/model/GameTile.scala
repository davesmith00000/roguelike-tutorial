package roguelike.model

import indigo._

import io.indigoengine.roguelike.starterkit.*

sealed trait GameTile:
  def lightMapTile: MapTile
  def darkMapTile: MapTile
  def blocked: Boolean
  def blockSight: Boolean
  def isBlocked: Boolean = blocked

object GameTile:
  case object Wall extends GameTile:
    val lightMapTile: MapTile   = MapTile(Tile.DARK_SHADE, RGB(0.9, 0.1, 0.1))
    val darkMapTile: MapTile    = MapTile(Tile.DARK_SHADE, RGB(0.4, 0.1, 0.1))
    val blocked: Boolean        = true
    val blockSight: Boolean     = true

  case object Ground extends GameTile:
    val lightMapTile: MapTile = MapTile(Tile.LIGHT_SHADE, RGB(1.0, 1.0, 0.0), RGBA(0.75, 0.6, 0.3, 1.0))
    val darkMapTile: MapTile  = MapTile(Tile.LIGHT_SHADE, RGB(0.0, 0.4, 1.0), RGBA(0.0, 0.0, 0.5, 1.0))
    val blocked: Boolean      = false
    val blockSight: Boolean   = false
