package CLI

import Engine.FieldState.{AliveShip, Empty, FieldState, MissShot, SunkShip}

object BoardPrinter {
  def printBoard(board: Array[Array[FieldState]]) {
    val boardSize = board.length
    print(" Y")
    (0 until boardSize).foreach(print(_))
    println()
    print("Xr")
    (0 until boardSize).foreach(_ => print("-"))

    println()
    for(x <- 0 until boardSize) {
      print(x + "|")
      for(y <- 0 until boardSize) {
        val sign = board(x)(y) match {
          case Empty => " "
          case MissShot => "M"
          case SunkShip => "X"
          case AliveShip => "S"
        }
        print(sign)
      }
      println()
    }
  }
}
