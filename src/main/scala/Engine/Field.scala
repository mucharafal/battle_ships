package Engine

import Engine.FieldState.FieldState
import Engine.FieldState._

case class Field(point: Point, fieldState: FieldState) {

}

object Field{
  def apply(point: Point, wasShotOnField: Boolean, isShipOnField: Boolean): Field = {
    val fieldState =
      if(wasShotOnField) {
        if (isShipOnField) {
          SunkShip
        } else {
          MissShot
        }
      } else {
        if(isShipOnField) {
          AliveShip
        } else {
          Empty
        }
      }
    new Field(point, fieldState)
  }
}
