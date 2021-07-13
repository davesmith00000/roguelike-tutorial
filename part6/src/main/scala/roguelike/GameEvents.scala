package roguelike

import indigo.shared.events.GlobalEvent

import indigo.Point

enum GameEvent extends GlobalEvent:
  case MeleeAttack(attackerName: String, power: Int, id: Option[Int]) extends GameEvent
  case MoveEntity(id: Int, to: Point) extends GameEvent
