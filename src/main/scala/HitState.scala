abstract class HitState
case class Hit(x: Int, y: Int) extends HitState
case object Sunk extends HitState
case object Miss extends HitState
case object Incorrect extends HitState
