package Engine

trait Player {
  def generateNewBoard(): Board
  def makeShot(enemyBoard: BoardRepresentation): Point
  def enemyShot(ownBoard: BoardRepresentation): Unit
  def shipIsSunk(): Unit
  def shipHit(point: Point): Unit
  def win()
  def lost()
  def incorrectMove()
}
