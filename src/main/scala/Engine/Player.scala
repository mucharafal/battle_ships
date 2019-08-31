package Engine

import Engine.FieldState.FieldState

trait Player {
  def generateNewBoard(): Board
  def makeShot(enemyBoard: Array[Array[FieldState]]): (Int, Int)
  def enemyShot(ownBoard: Array[Array[FieldState]]): Unit
  def shipIsSunk(): Unit
  def shipHit(positionX: Int, positionY: Int): Unit
  def endGame(win: Boolean): Unit
}
