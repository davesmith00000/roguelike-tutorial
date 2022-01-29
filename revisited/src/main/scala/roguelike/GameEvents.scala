package roguelike

import indigo.Point
import indigo.shared.events.GlobalEvent
import roguelike.model.Message

enum GameEvent extends GlobalEvent:
  // Player events
  case PlayerAttack(attackerName: String, power: Int, id: Int) extends GameEvent
  case PlayerCastsConfusion(attackerName: String, turns: Int, id: Int)
      extends GameEvent
  case PlayerCastsFireball(attackerName: String, damage: Int, id: Int)
      extends GameEvent
  case PlayerTurnEnd extends GameEvent

  // Hostile events
  case HostileMeleeAttack(attackerName: String, power: Int) extends GameEvent
  case HostileGiveXP(amount: Int)                           extends GameEvent

  // System events
  case Log(message: Message)                                extends GameEvent
  case Redraw                                               extends GameEvent
  case TargetUsingItem(inventoryPosition: Int, radius: Int) extends GameEvent
  case Targeted(position: Point)                            extends GameEvent
