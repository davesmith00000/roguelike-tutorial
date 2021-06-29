package roguelike.model

import indigo._
import roguelike.terminal.MapTile
import roguelike.DfTiles

sealed trait GameTile:
  def lightMapTile: MapTile
  def darkMapTile: MapTile
  def blocked: Boolean
  def blockSight: Boolean
  def explored: Boolean
  def isBlocked: Boolean = blocked
  def markExplored: GameTile

object GameTile:
  case object Wall extends GameTile:
    val lightMapTile: MapTile   = MapTile(DfTiles.Tile.DARK_SHADE, RGB(0.9, 0.1, 0.1))
    val darkMapTile: MapTile    = MapTile(DfTiles.Tile.DARK_SHADE, RGB(0.4, 0.1, 0.1))
    val blocked: Boolean        = true
    val blockSight: Boolean     = true
    val explored: Boolean       = false
    def markExplored: GameTile = this

  final case class Ground(explored: Boolean) extends GameTile:
    val lightMapTile: MapTile = MapTile(DfTiles.Tile.LIGHT_SHADE, RGB(1.0, 1.0, 0.0), RGBA(0.75, 0.6, 0.3, 1.0))
    val darkMapTile: MapTile  = MapTile(DfTiles.Tile.LIGHT_SHADE, RGB(0.1, 0.1, 0.4))
    val blocked: Boolean      = false
    val blockSight: Boolean   = false
    def markExplored: GameTile = this.copy(explored = true)
