package Engine

import Engine.Direction.{Direction, Horizontal, Vertical}

case class Ship(length: Int,
                position: Point, direction: Direction) {
  def collide(other: Ship): Boolean= {
    direction match {
      case Vertical =>
        (0 until length).exists(x => {other.isIn(position.down(x))})
      case Horizontal =>
        (0 until length).exists(x => {other.isIn(position.right(x))})
    }
  }

  def isIn(point: Point): Boolean = {
    direction match {
      case Vertical =>
        point.isOnVerticalLineBetween(position, position.down(length-1))
      case Horizontal =>
        point.isOnHorizontalLineBetween(position, position.right(length-1))
    }
  }

  def getListOfFieldsCooridinates: List[Point] = {
    (position to this.endPoint).toList
  }

  def endPoint: Point = direction match {
    case Vertical => position.down(length-1)
    case Horizontal => position.right(length-1)
  }

}
