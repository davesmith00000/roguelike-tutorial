package roguelike

import indigo.shared.events.GlobalEvent

import indigo.Point
import roguelike.model.Message

enum GameEvent extends GlobalEvent:
  // Player events
  case PlayerMeleeAttack(attackerName: String, power: Int, id: Int) extends GameEvent
  case PlayerTurnEnd extends GameEvent

  // Hostile events
  case HostileMeleeAttack(attackerName: String, power: Int) extends GameEvent

  // System events
  case Log(message: Message) extends GameEvent
  case RegenerateLevel extends GameEvent
  case Redraw extends GameEvent
