package Engine

import Engine.Direction._
import Engine.FieldState._

case class Board(ships: List[Ship], enemyHits: EnemyActions) {
  import Board._

  def addShip(ship: Ship): (Board, Boolean) = {
    if(
      canShipWithGivenLengthBeAdded(ship.length) &&
      isInBoard(ship) &&
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
    !(beginPoint to endPoint).exists(shipIsOnField(_))
  }

  def isAlive: Boolean = {
    getViewForOwner.exists(row => row.contains(AliveShip))
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

  def isInBoard(ship: Ship): Boolean = {
    val insideAxis = (coordinate: Int) => coordinate >= 0 && coordinate < boardSize
    val insideBoard = (p: Point) => insideAxis(p.x) && insideAxis(p.y)
    insideBoard(ship.position) && insideBoard(ship.endPoint)
  }

  def getViewForEnemy: Array[Array[FieldState]] = {
    getViewForOwner.map(x => x.map{
      case AliveShip => Empty
      case anyOther => anyOther
    })
  }

  def getViewForOwner: Array[Array[FieldState]] = {
    val ownerView: Array[Array[FieldState]] = Array.ofDim[FieldState](boardSize, boardSize)
    for(i <- 0 until boardSize) {
      for(j <- 0 until boardSize) {
        ownerView(i)(j) = enemyHits.wasShotOn(Point(i, j)) match {
          case true if shipIsOnField(Point(i, j)) => SunkShip
          case true => MissShot
          case false if shipIsOnField(Point(i, j)) => AliveShip
          case false => Empty
        }
      }
    }
    ownerView
  }

  def shotOn(point: Point): (Board, HitState) = {
    if(enemyHits.wasShotOn(point)) {
      (this, Incorrect)
    } else {
      val newEnemyHits = enemyHits.addShot(point)
      val newBoard = Board(ships, newEnemyHits)
      val optionShip = ships.find(_.isIn(point))
      optionShip match {
        case None => (newBoard, Miss)
        case Some(ship) =>
          if (isShipAlive(ship)) {
            (newBoard, Hit(point))
          } else {
            (aroundSunkShipWithHits(ship), Sunk)
          }
      }
    }
  }

  def aroundSunkShipWithHits(ship: Ship): Board = {
    var newEnemyHits = enemyHits
    ship.direction match {
      case Horizontal =>
        for(x <- Math.max(0, ship.position.x - 1) to Math.min(boardSize-1, ship.position.x + 1)) {
          for(y <- Math.max(0, ship.position.y - 1) to Math.min(boardSize-1, ship.position.y + ship.length)) {
            newEnemyHits = newEnemyHits.addShot(Point(x, y))
          }
        }
      case Vertical =>
        for(x <- Math.max(0, ship.position.x - 1) to Math.min(boardSize-1, ship.position.x + ship.length)) {
          for(y <- Math.max(0, ship.position.y - 1) to Math.min(boardSize-1, ship.position.y + 1)) {
            newEnemyHits = newEnemyHits.addShot(Point(x, y))
          }
        }
    }
    Board(ships, newEnemyHits)
  }

  def shipIsOnField(point: Point): Boolean = {
    ships.exists(p => p.isIn(point))
  }

  def isShipAlive(ship: Ship): Boolean = {
    val points = ship.getListOfFieldsCooridinates
    print(points)
    points.exists(point => enemyHits.fieldIsClear(point))
  }

  def isReady: Boolean = {
    getNumberOfShipsOnBoard(4) == 1 &&
    getNumberOfShipsOnBoard(3) == 2 &&
    getNumberOfShipsOnBoard(2) == 3 &&
    getNumberOfShipsOnBoard(1) == 4 &&
    ships.length == 10
  }
}

object Board {
  val boardSize = 10

  def createFieldsForBoard(boardSize: Int): Array[Array[Boolean]] = {
    val board: Array[Array[Boolean]] = Array.ofDim[Boolean](boardSize, boardSize)
    for(i <- 0 until boardSize) {
      for(j <- 0 until boardSize) {
        board(i)(j) = false
      }
    }
    board
  }

  def getMaximumNumberOfShips(length: Int): Int = {
    // todo refactor this, in code should be one place with specification of this
    length match {
      case 1 => 4
      case 2 => 3
      case 3 => 2
      case 4 => 1
      case _ => 0
    }
  }

  def apply(): Board = new Board(List[Ship](), EnemyActions())
}


