package Engine

import java.util.Random

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
    point.isOnLineBetween(position, this.endPoint)
  }

  def getListOfFieldsCoordinates: List[Point] = {
    (position to this.endPoint).toList
  }

  def endPoint: Point = direction match {
    case Vertical => position.down(length-1)
    case Horizontal => position.right(length-1)
  }

}

object Ship {
  def generate(length: Int): Ship = {
    val random = new Random()
    val x = random.nextInt(Board.getSize)
    val y = random.nextInt(Board.getSize)
    val direction = if (random.nextBoolean()) {
      Horizontal
    } else {
      Vertical
    }
    Ship(length, Point(x, y),direction)
  }
}
