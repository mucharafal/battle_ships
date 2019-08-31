package Engine

object FieldState extends Enumeration {
  type FieldState = Value
  val Empty, MissShot, SunkShip, AliveShip = Value
}
