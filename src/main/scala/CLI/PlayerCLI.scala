package CLI

import CLI.Action.Action
import Engine.Direction.Direction
import Engine._

import scala.io.StdIn
class PlayerCLI extends Player {
  override def lost(): Unit = {
    println("You have lost...")
  }

  override def win(): Unit = {
    println("You have won!")
  }

  override def makeShot(enemyBoard: BoardRepresentation): Point = {
    BoardPrinter.printBoard(enemyBoard)
    println("Type coordinates of your shot: ")
    val x = StdIn.readInt()
    val y = StdIn.readInt()
    Point(x, y)
  }

  override def enemyShot(ownBoard: BoardRepresentation): Unit = {
    println("Enemy shot...")
    BoardPrinter.printBoard(ownBoard)
  }

  override def shipIsSunk(): Unit = {
    println("You sunk enemy ship!")
  }

  override def shipHit(point: Point): Unit = {
    println("You have shot in enemy ship!")
  }

  override def generateNewBoard(): Board = {
    var board: Board = Board()
    while(true) {
      if(board.isReady) {
        if(askFor("Board is ready. Would like to start game?")) {
          return board
        }
      }
      BoardPrinter.printBoard(board.getViewForOwner)
      board = askForAction() match {
        case Action.AddShip => addShip(board)
        case Action.RemoveShip => removeShip(board)
        case _ =>
          println("Unknown action")
          board
      }
    }
    board
  }
  def askFor(question: String): Boolean = {
    println(question + " Answer: y/n")
    val answer = StdIn.readChar()
    answer == 'y'
  }

  def askForAction(): Action = {
    val message = "Actions:\n" +
      "1- add ship\n" +
      "2- remove ship"
    println(message)
    StdIn.readInt() match {
      case 1 => Action.AddShip
      case 2 => Action.RemoveShip
      case _ => Action.Unknown
    }
  }

  def askForCoordinates: Point = {
    println("Type coordinates: ")
    Point(StdIn.readInt(), StdIn.readInt())
  }

  def askForDirection: Direction = {
    println("Direction- horizontal(h) or vertical(v)?")
    StdIn.readChar() match {
      case 'h' => Direction.Horizontal
      case 'v' => Direction.Vertical
      case _ => askForDirection
    }
  }

  def askForLength: Int = {
    println("Type length: ")
    StdIn.readInt()
  }

  def addShip(board: Board): Board = {
    val coordinates = askForCoordinates
    val direction = askForDirection
    val length = askForLength

    val ship = Ship(length, coordinates, direction)
    val newBoardAndResult = board.addShip(ship)
    if(newBoardAndResult._2) {
      println("Ship added successfully")
    } else {
      println("Cannot add ship")
    }
    newBoardAndResult._1
  }

  def removeShip(board: Board): Board = {
    val coordinates = askForCoordinates

    val optionShip = board.ships.find(_.isIn(coordinates))
    optionShip match {
      case Some(ship) => board.deleteShip(ship)
      case None => println("Coordinates are incorrect")
        board
    }
  }
}

object Action extends Enumeration {
  type Action = Value
  val AddShip, RemoveShip, Unknown = Value
}
