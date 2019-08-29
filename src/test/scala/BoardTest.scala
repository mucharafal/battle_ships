import org.scalatest.FlatSpec
import Direction._

class BoardTest extends FlatSpec{
  "Board " should "add ship on" in {
    val board: Board = new Board()
    val ship: Ship = Ship(4, 0, 0, Horizontal)
    assert(board.addShip(ship))
    assert(board.ships.contains(ship))
  }
  it should "remove ship" in {
    val board: Board = new Board()
    val ship: Ship = Ship(4, 0, 0, Horizontal)
    board.addShip(ship)
    board.addShip(Ship(2, 2, 2, Horizontal))
    board.deleteShip(ship)
    assert(!board.ships.contains(ship) && board.ships.contains(Ship(2, 2, 2, Horizontal)))
  }

  "isNotTooClose" should "give correct results" in {
    val board = new Board()
    val ship = Ship(4, 0, 0, Horizontal)
    board.addShip(ship)
    assert(!board.isNotTooClose(Ship(1, 1, 1, Vertical)))
    assert(!board.isNotTooClose(Ship(3, 1, 2, Horizontal)))
    assert(board.isNotTooClose(Ship(1, 1, 5, Horizontal)))
  }

  "shipIsInField" should "give correct result" in {
    val board = new Board()
    val ship = Ship(2, 0, 0, Horizontal)
    board.addShip(ship)
    assert(board.shipIsOnField(0, 0))
    assert(board.shipIsOnField(0, 1))
  }

  "isShipAlive" should "give proper result" in {
    val board = new Board()
    val ship = Ship(2, 0, 0, Horizontal)
    board.addShip(ship)
    board.enemyHits(0)(0) = true
    board.enemyHits(0)(1) = true
    assert(!board.isShipAlive(ship))
  }

  "collide" should "work" in {
    val board = new Board()
    val ship = Ship(2, 1, 0, Horizontal)
    board.addShip(ship)
    assert(board.collide(Ship(2, 0, 1, Vertical)))
  }

  "isInBoard" should "work" in {
    val board = new Board()
    val ship = Ship(4, 9, 0, Horizontal)
    assert(board.isInBoard(ship))
    assert(board.isInBoard(Ship(3, 7, 7, Vertical)))
    assert(!board.isInBoard(Ship(2, 9, 9, Vertical)))
  }
}
