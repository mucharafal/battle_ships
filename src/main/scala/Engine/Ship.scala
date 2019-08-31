package Engine

import Engine.Direction.{Direction, Horizontal, Vertical}

case class Ship(length: Int,
                positionX: Int, positionY: Int, direction: Direction) {
  def collide(other: Ship): Boolean= {
    direction match {
      case Vertical =>
        (0 until length).exists(x => {other.isIn(x + positionX, positionY)})
      case Horizontal =>
        (0 until length).exists(x => {other.isIn(positionX, positionY + x)})
    }
  }

  def isIn(pointX: Int, pointY: Int): Boolean = {
    direction match {
      case Vertical =>
        positionY == pointY && positionX <= pointX && positionX + length > pointX
      case Horizontal =>
        positionX == pointX && positionY <= pointY && positionY + length > pointY
    }
  }

  def getListOfFieldsCooridinates: List[(Int, Int)] = {
    var coordinates: List[(Int, Int)] = List()
    for(i <- 0 until length) {
      direction match {
        case Vertical => coordinates = (positionX + i, positionY) :: coordinates
        case Horizontal => coordinates = (positionX, positionY + i) :: coordinates
      }
    }
    coordinates
  }


}
