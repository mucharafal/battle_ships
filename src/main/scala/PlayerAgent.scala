import FieldState.FieldState
import Direction._
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
        var ship: Ship = null
        do {
          val x = random.nextInt(10)
          val y = random.nextInt(10)
          val direction = if (random.nextBoolean()) {
            Horizontal
          } else {
            Vertical
          }
          ship = Ship(length, x, y, direction)
        }
        while(!board.addShip(ship))
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
      case x => x
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
          case null => maxPositionOnRight(enemyBoard, x, y)
          case position => position
        }
      case Vertical =>
        maxPositionUp(enemyBoard, x, y) match {
          case null => maxPositionDown(enemyBoard, x, y)
          case position => position
        }
    }
  }

  def maxPositionDown(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): (Int, Int) = {
    val y = begin_y
    var x = begin_x + 1
    while(x - 1 < enemyBoard.length && enemyBoard(x)(y) == FieldState.SunkShip) {
      x += 1
    }
    if(canGiveShot(x, y, enemyBoard)) {
      (x, y)
    } else {
      null
    }
  }

  def maxPositionUp(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): (Int, Int) = {
    val y = begin_y
    var x = begin_x - 1
    while(x > 0 && enemyBoard(x)(y) == FieldState.SunkShip) {
      x -= 1
    }
    if(canGiveShot(x, y, enemyBoard)) {
      (x, y)
    } else {
      null
    }
  }

  def maxPositionOnLeft(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): (Int, Int) = {
    var y = begin_y - 1
    val x = begin_x
    while(y > 0 && enemyBoard(x)(y) == FieldState.SunkShip) {
      y -= 1
    }
    if(canGiveShot(x, y, enemyBoard)) {
      (x, y)
    } else {
      null
    }
  }

  def maxPositionOnRight(enemyBoard: Array[Array[FieldState]], begin_x: Int, begin_y: Int): (Int, Int) = {
    var y = begin_y + 1
    val x = begin_x
    while(y - 1 < enemyBoard.length && enemyBoard(x)(y) == FieldState.SunkShip) {
      y += 1
    }
    if(canGiveShot(x, y, enemyBoard)) {
      (x, y)
    } else {
      null
    }
  }
}
