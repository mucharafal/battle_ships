package AI

import CLI.BoardPrinter
import Engine.Direction._
import Engine.FieldState.FieldState
import Engine.{Board, FieldState, Player, Ship}

import scala.util.Random
class PlayerAgent extends Player {
  var state: Shooter = RandomShooter()
  var number: Int = Random.nextInt()
  override def endGame(win: Boolean): Unit = {

  }

  override def enemyShot(ownBoard: Array[Array[FieldState]]): Unit = {
    println(number)
    BoardPrinter.printBoard(ownBoard)
  }

  override def generateNewBoard(): Board = {
    val board = new Board()
    val random = new Random()
    for(length <- 1 to 4) {
      val numberOfUnits = Board.getMaximumNumberOfShips(length)
      for(_ <- 0 until numberOfUnits) {
        var ship: Option[Ship] = None
        do {
          val x = random.nextInt(10)
          val y = random.nextInt(10)
          val direction = if (random.nextBoolean()) {
            Horizontal
          } else {
            Vertical
          }
          ship = Some(Ship(length, x, y, direction))
        }
        while(!board.addShip(ship.get))
      }
    }
    println(number)
    BoardPrinter.printBoard(board.getViewForOwner)
    board
  }

  override def shipHit(positionX: Int, positionY: Int): Unit = {
    println("Hit!- " + number)
    state = state match {
      case RandomShooter() => GetDirectionShooter(positionX, positionY)
      case GetDirectionShooter(past_x, _) if past_x == positionX =>
        FinishInDirectionShooter(positionX, positionY, Horizontal)
      case GetDirectionShooter(_, _) => FinishInDirectionShooter(positionX, positionY, Vertical)
      case x: FinishInDirectionShooter => x
    }
  }

  override def shipIsSunk(): Unit = {
    println("Sunk- " + number)
    state = RandomShooter()
  }

  override def makeShot(enemyBoard: Array[Array[FieldState]]): (Int, Int) = {
    state.makeShot(enemyBoard)
  }
}

trait Shooter {
  def makeShot(enemyBoard: Array[Array[FieldState]]): (Int, Int)
  def canGiveShot(x: Int, y: Int, enemyBoard: Array[Array[FieldState]]): Boolean = {
    x >= 0 && x < enemyBoard.length &&
      y >= 0 && y < enemyBoard.length &&
      enemyBoard(x)(y) == FieldState.Empty
  }
}

case class RandomShooter() extends Shooter {
  override def makeShot(enemyBoard: Array[Array[FieldState]]): (Int, Int) = {
    val x = Random.nextInt(enemyBoard.length)
    val y = Random.nextInt(enemyBoard.length)
    if(canGiveShot(x, y, enemyBoard)) {
      (x, y)
    } else {
      makeShot(enemyBoard)
    }
  }
}

case class GetDirectionShooter(x: Int, y: Int) extends Shooter {
  override def makeShot(enemyBoard: Array[Array[FieldState]]): (Int, Int) = {
    val up = (x, y+1)
    val down = (x, y-1)
    val left = (x-1, y)
    val right = (x+1, y)

    val positions = Random.shuffle(List(up, down, left, right))

    lazy val choosePosition: List[(Int, Int)] => (Int, Int) = {
      case head :: _ if canGiveShot(head._1, head._2, enemyBoard) => head
      case _ :: tail => choosePosition(tail)
    }

    choosePosition(positions)
  }


}

case class FinishInDirectionShooter(x: Int, y: Int, direction: Direction) extends Shooter {
  override def makeShot(enemyBoard: Array[Array[FieldState]]): (Int, Int) = {
    direction match {
      case Horizontal =>
        maxPositionOnLeft(enemyBoard, x, y) match {
          case None => maxPositionOnRight(enemyBoard, x, y).get
          case Some(position) => position
        }
      case Vertical =>
        maxPositionUp(enemyBoard, x, y) match {
          case None => maxPositionDown(enemyBoard, x, y).get
          case Some(position) => position
        }
    }
  }

  def maxPositionDown(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): Option[(Int, Int)] = {
    val stepDown = (position: (Int, Int)) => (position._1 + 1, position._2)
    maxPositionInDirection(enemyBoard, begin_x, begin_y, stepDown)
  }

  def maxPositionUp(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): Option[(Int, Int)] = {
    val stepUp = (position: (Int, Int)) => (position._1 - 1, position._2)
    maxPositionInDirection(enemyBoard, begin_x, begin_y, stepUp)
  }

  def maxPositionOnLeft(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): Option[(Int, Int)] = {
    val stepLeft = (position: (Int, Int)) => (position._1, position._2 - 1)
    maxPositionInDirection(enemyBoard, begin_x, begin_y, stepLeft)
  }

  def maxPositionOnRight(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): Option[(Int, Int)] = {
    val stepRight = (position: (Int, Int)) => (position._1, position._2 + 1)
    maxPositionInDirection(enemyBoard, begin_x, begin_y, stepRight)
  }

  def maxPositionInDirection(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int,
                             nextStep: ((Int, Int)) => (Int, Int)): Option[(Int, Int)] = {
    var nextPosition = nextStep((begin_x, begin_y))
    while(insideBoard(nextPosition, enemyBoard.length) && enemyBoard(nextPosition._1)(nextPosition._2) == FieldState.SunkShip) {
      nextPosition = nextStep(nextPosition)
    }
    if(canGiveShot(nextPosition._1, nextPosition._2, enemyBoard)) {
      Some(nextPosition)
    } else {
      None
    }
  }

  def insideBoard(position: (Int, Int), boardSize: Int): Boolean = {
    val x = position._1
    val y = position._2
    val inside = (p: Int) => p >= 0 && p < boardSize
    inside(x) && inside(y)
  }
}
