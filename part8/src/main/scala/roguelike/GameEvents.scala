package roguelike

import indigo.shared.events.GlobalEvent

import indigo.Point
import roguelike.model.Message

enum GameEvent extends GlobalEvent:
  case MeleeAttack(attackerName: String, power: Int, id: Option[Int]) extends GameEvent
  case MoveEntity(id: Int, to: Point) extends GameEvent
  case Log(message: Message) extends GameEvent
  case RegenerateLevel extends GameEvent
  case Redraw extends GameEvent
