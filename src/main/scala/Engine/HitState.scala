package Engine

sealed trait HitState
case class Hit(point: Point) extends HitState
case object Sunk extends HitState
case object Miss extends HitState
case object Incorrect extends HitState
