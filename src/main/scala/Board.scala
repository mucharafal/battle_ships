import FieldState._
import Direction._
import HitState.HitState

class Board {
  import Board._
  val boardSize = 10
  var ships: List[Ship] = List.empty
  var enemyHits: Array[Array[Boolean]] = createFieldsForBoard(boardSize)
  def addShip(ship: Ship): Boolean = {
    if(
      canShipWithGivenLengthBeAdded(ship.length) &&
      isInBoard(ship) &&
      isNotTooClose(ship)
    ) {
      ships = ship :: ships
      true
    } else {
      false
    }
  }

  def isNotTooClose(ship: Ship): Boolean = {
    val x_begin = ship.positionX - 1
    val y_begin = ship.positionY - 1
    val x_end = ship.direction match {
      case Vertical => ship.positionX + ship.length
      case Horizontal => ship.positionX + 1
    }
    val y_end = ship.direction match {
      case Vertical => ship.positionY + 1
      case Horizontal => ship.positionY + ship.length
    }
    for(x <- x_begin to x_end){
      for(y <- y_begin to y_end){
        if(ships.exists(_.isIn(x, y))) {
          return false
        }
      }
    }
    true
  }

  def isAlive: Boolean = {
    getViewForOwner.exists(row => row.contains(AliveShip))
  }

  def deleteShip(ship: Ship): Unit ={
    ships = ships.filter(_ != ship)
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
    ship.positionX < boardSize && ship.positionY < boardSize &&
      (ship.direction match {
      case Vertical => ship.positionY + ship.length < boardSize
      case Horizontal => ship.positionX + ship.length < boardSize
    })
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
        ownerView(i)(j) = enemyHits(i)(j) match {
          case true if shipIsOnField(i, j) => SunkShip
          case true => MissShot
          case false if shipIsOnField(i, j) => AliveShip
          case false => Empty
        }
      }
    }
    ownerView
  }

  def shotOn(x: Int, y: Int): HitState = {
    if(enemyHits(x)(y)) {
      HitState.Incorrect
    } else {
      enemyHits(x)(y) = true
      val optionShip = ships.find(_.isIn(x, y))
      optionShip match {
        case None => HitState.Miss
        case Some(ship) =>
          if (isShipAlive(ship)) {
            HitState.Hit
          } else {
            HitState.Sunk
          }
      }
    }
  }

  def shipIsOnField(x: Int, y: Int): Boolean = {
    ships.exists(p => p.isIn(x, y))
  }

  def isShipAlive(ship: Ship): Boolean = {
    val coordinates = ship.getListOfFieldsCooridinates
    coordinates.forall(coordinate => enemyHits(coordinate._1)(coordinate._2))
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
    length match {
      case 1 => 4
      case 2 => 3
      case 3 => 2
      case 4 => 1
      case _ => 0
    }
  }
}


