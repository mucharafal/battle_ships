package Engine

import Engine.FieldState._

case class BoardRepresentation(fields: List[Field]) {
  def getViewForEnemy: BoardRepresentation = BoardRepresentation(fields.filter(x => x.fieldState != AliveShip))

  def getStateOf(point: Point): FieldState = {
    fields.find(x => x.point == point) match {
      case Some(field) => field.fieldState
      case None => Empty
    }
  }
}
