package AI

import Engine.Direction._
import Engine.{Direction => _, _}

import scala.annotation.tailrec
import scala.util.Random
class PlayerAgent extends Player {
  var state: Shooter = RandomShooter()
  var number: Int = Random.nextInt()

  override def win(): Unit = {
  }

  override def lost(): Unit = {
  }

  override def enemyShot(ownBoard: BoardRepresentation): Unit = {
  }

  override def generateNewBoard(): Board = {
    var board = Board()
    var wasAdded = false
    val random = new Random()
    for(length <- 1 to 4) {
      val numberOfUnits = Board.getMaximumNumberOfShips(length)
      for(_ <- 0 until numberOfUnits) {
        var ship: Option[Ship] = None
        do {
          val x = random.nextInt(Board.getSize)
          val y = random.nextInt(Board.getSize)
          val direction = if (random.nextBoolean()) {
            Horizontal
          } else {
            Vertical
          }
          // todo refactor generating points
          ship = Some(Ship(length, Point(x, y),direction))
          val newBoardAndResult = board.addShip(ship.get)
          board = newBoardAndResult._1
          wasAdded = newBoardAndResult._2
        }
        while(!wasAdded)
      }
    }
    board
  }

  override def shipHit(position: Point): Unit = {
    state = state match {
      case RandomShooter() => FindDirectionShooter(position)
      case FindDirectionShooter(lastAccurateShot) if lastAccurateShot.x == position.x =>
        FinishInDirectionShooter(position, Horizontal)
      case FindDirectionShooter(_) => FinishInDirectionShooter(position, Vertical)
      case x: FinishInDirectionShooter => x
    }
  }

  override def shipIsSunk(): Unit = {
    state = RandomShooter()
  }

  override def makeShot(enemyBoard: BoardRepresentation): Point = {
    state.makeShot(enemyBoard)
  }
}

trait Shooter {
  def makeShot(enemyBoard: BoardRepresentation): Point
  def canGiveShot(position: Point, enemyBoard: BoardRepresentation): Boolean = {
      insideBoard(position) &&
      enemyBoard.getStateOf(position) == FieldState.Empty
  }
  def insideBoard(position: Point): Boolean = {
    val inside = (p: Int) => p >= 0 && p < Board.getSize
    inside(position.x) && inside(position.y)
  }
}

case class RandomShooter() extends Shooter {
  @tailrec override final def makeShot(enemyBoard: BoardRepresentation): Point = {
    val position = Point(Random.nextInt(Board.getSize), Random.nextInt(Board.getSize))
    if(canGiveShot(position, enemyBoard)) {
      position
    } else {
      makeShot(enemyBoard)
    }
  }
}

case class FindDirectionShooter(lastAccurateShot: Point) extends Shooter {
  override def makeShot(enemyBoard: BoardRepresentation): Point = {
    val positions = Random.shuffle(
      List(lastAccurateShot.up,
      lastAccurateShot.down,
      lastAccurateShot.left,
      lastAccurateShot.right))

    lazy val choosePosition: List[Point] => Point = {
      case head :: _ if canGiveShot(head, enemyBoard) => head
      case _ :: tail => choosePosition(tail)
    }

    choosePosition(positions)
  }
}

case class FinishInDirectionShooter(lastAccurateShot: Point, direction: Direction) extends Shooter {
  override def makeShot(enemyBoard: BoardRepresentation): Point = {
    direction match {
      case Horizontal =>
        maxPositionOnLeft(enemyBoard, lastAccurateShot) match {
          case None => maxPositionOnRight(enemyBoard, lastAccurateShot).get
          case Some(position) => position
        }
      case Vertical =>
        maxPositionUp(enemyBoard, lastAccurateShot) match {
          case None => maxPositionDown(enemyBoard, lastAccurateShot).get
          case Some(position) => position
        }
    }
  }

  def maxPositionDown(enemyBoard: BoardRepresentation, beginPoint: Point): Option[Point] = {
    val stepDown = (x: Point) => x.down
    maxPositionInDirection(enemyBoard, beginPoint, stepDown)
  }

  def maxPositionUp(enemyBoard: BoardRepresentation, beginPoint: Point): Option[Point] = {
    val stepUp = (x: Point) => x.up
    maxPositionInDirection(enemyBoard, beginPoint, stepUp)
  }

  def maxPositionOnLeft(enemyBoard: BoardRepresentation, beginPoint: Point): Option[Point] = {
    val stepLeft = (x: Point) => x.left
    maxPositionInDirection(enemyBoard, beginPoint, stepLeft)
  }

  def maxPositionOnRight(enemyBoard: BoardRepresentation, beginPoint: Point): Option[Point] = {
    val stepRight = (x: Point) => x.right
    maxPositionInDirection(enemyBoard, beginPoint, stepRight)
  }

  def maxPositionInDirection(enemyBoard: BoardRepresentation, beginPosition: Point,
                             nextStep: Point => Point): Option[Point] = {
    var nextPosition = nextStep(beginPosition)
    while(insideBoard(nextPosition) && enemyBoard.getStateOf(nextPosition) == FieldState.SunkShip) {
      nextPosition = nextStep(nextPosition)
    }
    if(canGiveShot(nextPosition, enemyBoard)) {
      Some(nextPosition)
    } else {
      None
    }
  }
}
