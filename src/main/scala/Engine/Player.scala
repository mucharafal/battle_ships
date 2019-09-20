package Engine

trait Player {
  def generateNewBoard(): Board
  def makeShot(enemyBoard: BoardRepresentation): Point
  def enemyShot(ownBoard: BoardRepresentation)
  def shipIsSunk(): Player
  def shipHit(point: Point): Player
  def win()
  def lost()
  def incorrectMove()
}
