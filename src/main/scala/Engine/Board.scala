package Engine

import Engine.Direction._
import Engine.FieldState._

case class Board(ships: List[Ship], enemyHits: EnemyActions) {
  import Board._

  def addShip(ship: Ship): (Board, Boolean) = {
    if(
      canShipWithGivenLengthBeAdded(ship.length) &&
      isInsideBoard(ship) &&
      isNotTooCloseToOthers(ship)
    ) {
      (Board(ship :: ships, enemyHits), true)
    } else {
      (this, false)
    }
  }

  def isNotTooCloseToOthers(ship: Ship): Boolean = {
    val beginPoint = ship.position.left.up
    val endPoint = ship.endPoint.right.down
    !(beginPoint to endPoint).exists(shipIsOnField)
  }

  def isAlive: Boolean = {
    ships.exists(ship => isShipAlive(ship))
  }

  def deleteShip(ship: Ship): Board ={
    Board(ships.filter(_ != ship), enemyHits)
  }

  def collide(ship: Ship): Boolean = {
    ships.exists(_.collide(ship))
  }

  def canShipWithGivenLengthBeAdded(length: Int): Boolean = {
    getMaximumNumberOfShips(length) -  getNumberOfShipsOnBoard(length) > 0
  }

  def getNumberOfShipsOnBoard(length: Int): Int = {
    ships.count(_.length == length)
  }

  def getViewForEnemy: BoardRepresentation = {
    getViewForOwner.getViewForEnemy
  }

  def getViewForOwner: BoardRepresentation = {
    val fields =
      for(point <- Point(0, 0) until Point(boardSize, boardSize)
          if getFieldState(point) != Empty)
        yield {
          Field(point, getFieldState(point))
      }
    BoardRepresentation(fields.toList)
  }

  def getFieldState(point: Point): FieldState = {
    if(shipIsOnField(point)) {
      if(enemyHits.wasShotOn(point)) {
        SunkShip
      } else {
        AliveShip
      }
    } else {
      if(enemyHits.wasShotOn(point)) {
        MissShot
      } else {
        Empty
      }
    }
  }

  def shotOn(point: Point): (Board, HitState) = {
    if(enemyHits.wasShotOn(point) ||
      !isInsideBoard(point)) {
      (this, Incorrect)
    } else {
      val newEnemyHits = enemyHits.addShot(point)
      val newBoard = Board(ships, newEnemyHits)
      val optionShip = ships.find(_.isIn(point))
      optionShip match {
        case None => (newBoard, Miss)
        case Some(ship) =>
          if (newBoard.isShipAlive(ship)) {
            (newBoard, Hit(point))
          } else {
            (aroundSunkShipWithHits(ship), Sunk)
          }
      }
    }
  }

  def aroundSunkShipWithHits(ship: Ship): Board = {
    var newEnemyHits = enemyHits
    val leftUpperCorner = ship.position.left.up
    val rightDownCorner = ship.endPoint.right.down
    (leftUpperCorner to rightDownCorner).foreach(point => if(isInsideBoard(point)) {newEnemyHits = newEnemyHits.addShot(point)})
    Board(ships, newEnemyHits)
  }

  def shipIsOnField(point: Point): Boolean = {
    ships.exists(p => p.isIn(point))
  }

  def isShipAlive(ship: Ship): Boolean = {
    val points = ship.getListOfFieldsCoordinates
    points.exists(point => enemyHits.fieldIsClear(point))
  }

  def isReady: Boolean = {
    !numberOfShipsWithGivenLength.exists(x => getNumberOfShipsOnBoard(x._1) == x._2) &&
    ships.length == 10
  }
}

object Board {
  val boardSize = 10
  val numberOfShipsWithGivenLength = Map(1 -> 4, 2 -> 3, 3 -> 2, 4 -> 1)

  def getMaximumNumberOfShips(length: Int): Int = {
    numberOfShipsWithGivenLength getOrElse (length, 0)
  }

  def apply(): Board = new Board(List[Ship](), EnemyActions())

  def getSize: Int = boardSize

  def isInsideBoard(ship: Ship): Boolean = {
    isInsideBoard(ship.position) && isInsideBoard(ship.endPoint)
  }

  def isInsideBoard(point: Point): Boolean = {
    val insideAxis = (coordinate: Int) => coordinate >= 0 && coordinate < boardSize
    val insideBoard = (p: Point) => insideAxis(p.x) && insideAxis(p.y)
    insideBoard(point)
  }
}


