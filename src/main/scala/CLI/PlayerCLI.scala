package CLI

import CLI.Action.Action
import Engine.Direction.Direction
import Engine.FieldState.FieldState
import Engine.{Board, Direction, Player, Ship}

import scala.io.StdIn
class PlayerCLI extends Player {
  override def endGame(win: Boolean): Unit = {
    if(win) {
      println("You have won!")
    } else {
      println("You have lost...")
    }
  }

  override def makeShot(enemyBoard: Array[Array[FieldState]]): (Int, Int) = {
    BoardPrinter.printBoard(enemyBoard)
    println("Type coordinates of your shot: ")
    val x = StdIn.readInt()
    val y = StdIn.readInt()
    (x, y)
  }

  override def enemyShot(ownBoard: Array[Array[FieldState]]): Unit = {
    println("Enemy shot...")
    BoardPrinter.printBoard(ownBoard)
  }

  override def shipIsSunk(): Unit = {
    println("You sunk enemy ship!")
  }

  override def shipHit(positionX: Int, positionY: Int): Unit = {
    println("You have shot in enemy ship!")
  }

  override def generateNewBoard(): Board = {
    val board: Board = new Board()
    while(true) {
      if(board.isReady) {
        if(askFor("Board is ready. Would like to start game?")) {
          return board
        }
      }
      BoardPrinter.printBoard(board.getViewForOwner)
      askForAction() match {
        case Action.AddShip => addShip(board)
        case Action.RemoveShip => removeShip(board)
        case _ => println("Unknown action")
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

  def askForCoordinates: (Int, Int) = {
    println("Type coordinates: ")
    (StdIn.readInt(), StdIn.readInt())
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

  def addShip(board: Board) {
    val coordinates = askForCoordinates
    val direction = askForDirection
    val length = askForLength

    val ship = Ship(length, coordinates._1, coordinates._2, direction)
    if(board.addShip(ship)) {
      println("Ship added successfully")
    } else {
      println("Cannot add ship")
    }
  }

  def removeShip(board: Board) {
    val coordinates = askForCoordinates

    val optionShip = board.ships.find(_.isIn(coordinates._1, coordinates._2))
    optionShip match {
      case Some(ship) => board.deleteShip(ship)
      case None => println("Coordinates are incorrect")
    }
  }
}

object Action extends Enumeration {
  type Action = Value
  val AddShip, RemoveShip, Unknown = Value
}
