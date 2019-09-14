import org.scalatest.FlatSpec
import Engine.Direction._
import Engine.{Board, EnemyActions, Point, Ship}

class BoardTest extends FlatSpec{
  "Engine.Board " should "add ship on" in {
    val board: Board = Board()
    val ship: Ship = Ship(4, Point(0, 0), Horizontal)
    val (boardWithAddedShip, ifAdded) = board.addShip(ship)
    println(boardWithAddedShip)
    assert(ifAdded)
    assert(boardWithAddedShip.ships.contains(ship))
  }
  it should "remove ship" in {
    val ship1: Ship = Ship(4, Point(0, 0), Horizontal)
    val ship2 = Ship(2, Point(2, 2), Horizontal)
    val board = Board(List(ship1, ship2), EnemyActions()).deleteShip(ship1)
    assert(!board.ships.contains(ship1) && board.ships.contains(ship2))
  }

  "isNotTooClose" should "give correct results" in {
    val ship = Ship(4, Point(0, 0), Horizontal)
    val board = Board(List(ship), EnemyActions())
    assert(!board.isNotTooCloseToOthers(Ship(1, Point(1, 1), Vertical)))
    assert(!board.isNotTooCloseToOthers(Ship(3, Point(1, 2), Horizontal)))
    assert(board.isNotTooCloseToOthers(Ship(1, Point(1, 5), Horizontal)))
  }

  "shipIsInField" should "give correct result" in {
    val board = Board()
    val ship = Ship(2, Point(0, 0), Horizontal)
    val (boardWithAddedShip, ifAdded) = board.addShip(ship)
    assert(boardWithAddedShip.shipIsOnField(Point(0, 0)))
    assert(boardWithAddedShip.shipIsOnField(Point(0, 1)))
    assert(!boardWithAddedShip.shipIsOnField(Point(1, 1)))
  }

  "isShipAlive" should "give proper result" in {
    val beginPoint = Point(0, 0)
    val ship = Ship(2, beginPoint, Horizontal)
    var enemyHits = EnemyActions().addShot(beginPoint)
    enemyHits = enemyHits.addShot(beginPoint.right)
    val board = Board(List(ship), enemyHits)
    assert(!board.isShipAlive(ship))
  }

  "collide" should "work" in {
    val board = Board()
    val ship = Ship(2, Point(1, 0), Horizontal)
    val (boardWithAddedShip, ifAdded) = board.addShip(ship)
    assert(boardWithAddedShip.collide(Ship(2, Point(0, 1), Vertical)))
  }

  "isInBoard" should "work" in {
    val board = Board()
    val ship = Ship(4, Point(9, 0), Horizontal)
    assert(board.isInBoard(ship))
    assert(board.isInBoard(Ship(3, Point(7, 7), Vertical)))
    assert(!board.isInBoard(Ship(2, Point(9, 9), Vertical)))
  }
}
