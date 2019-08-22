import FieldState._

object BoardPrinter {
  def printBoard(board: Array[Array[FieldState]]) {
    val boardSize = board.length
    for(x <- 0 until boardSize) {
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
