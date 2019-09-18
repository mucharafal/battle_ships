package AI

import Engine.Direction._
import Engine.{Direction => _, _}

import scala.annotation.tailrec
import scala.util.Random
case class PlayerAgent(state: Shooter, number: Int) extends Player {

  override def win() {
  }

  override def lost() {
  }

  override def incorrectMove() {
  }

  override def enemyShot(ownBoard: BoardRepresentation) {
  }

  override def generateNewBoard(): Board = {
    lazy val addShipWithLength: (Int, Board) => Board = (length: Int, board: Board) => {
      val ship = Ship.generate(length)
      val (newBoard, ifAdded) = board.addShip(ship)
      if(ifAdded) {
        newBoard
      } else {
        addShipWithLength(length, board)
      }
    }
    val addShipsWithLength = (length: Int, board: Board) => {
      val numberOfShips = Board.getMaximumNumberOfShips(length)
      (1 to numberOfShips).foldLeft(board)((board, _) => addShipWithLength(length, board))
    }
    Board.getLengthsOfShips.foldLeft(Board())((board: Board, length: Int) => addShipsWithLength(length, board))
  }

  override def shipHit(position: Point): PlayerAgent = {
    val newState = state match {
      case RandomShooter() => FindDirectionShooter(position)
      case FindDirectionShooter(lastAccurateShot) if lastAccurateShot.x == position.x =>
        FinishInDirectionShooter(position, Horizontal)
      case FindDirectionShooter(_) => FinishInDirectionShooter(position, Vertical)
      case x: FinishInDirectionShooter => x
    }
    PlayerAgent(newState, number)
  }

  override def shipIsSunk(): PlayerAgent = {
    PlayerAgent(RandomShooter(), number)
  }

  override def makeShot(enemyBoard: BoardRepresentation): Point = {
    state.makeShot(enemyBoard)
  }
}

object PlayerAgent {
  def apply(state: Shooter): PlayerAgent = new PlayerAgent(state, new Random().nextInt())
  def apply(): PlayerAgent = new PlayerAgent(RandomShooter(), new Random().nextInt())
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
    lazy val maximumPositionInDirection: Point => Point = (position: Point) => {
      val nextPosition = nextStep(position)
      if(insideBoard(nextPosition) && enemyBoard.getStateOf(position) == FieldState.SunkShip) {
        maximumPositionInDirection(nextPosition)
      } else {
        position
      }
    }
    val nextPosition = maximumPositionInDirection(beginPosition)
    if(canGiveShot(nextPosition, enemyBoard)) {
      Some(nextPosition)
    } else {
      None
    }
  }
}
