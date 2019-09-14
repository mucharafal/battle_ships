package Engine

import Engine.FieldState.FieldState

trait Player {
  def generateNewBoard(): Board
  def makeShot(enemyBoard: Array[Array[FieldState]]): Point
  def enemyShot(ownBoard: Array[Array[FieldState]]): Unit
  def shipIsSunk(): Unit
  def shipHit(point: Point): Unit
  def win()
  def lost()
}
