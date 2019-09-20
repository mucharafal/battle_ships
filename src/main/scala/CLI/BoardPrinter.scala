package CLI

import Engine.{Board, BoardRepresentation, Field, Point}
import Engine.FieldState.{AliveShip, Empty, MissShot, SunkShip}

object BoardPrinter {
  def printBoard(board: BoardRepresentation) {
    val boardSize = Board.getSize
    print(" Y")
    (0 until boardSize).foreach(print(_))
    println()
    print("Xr")
    (0 until boardSize).foreach(_ => print("-"))

    println()
    for(x <- 0 until boardSize) {
      print(x + "|")
      for(y <- 0 until boardSize) {
        val sign = board.getStateOf(Point(x, y)) match {
          case MissShot => "M"
          case SunkShip => "X"
          case AliveShip => "S"
          case Empty => " "
        }
        print(sign)
      }
      println()
    }
  }
}
