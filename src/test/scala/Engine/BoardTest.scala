package Engine

import org.scalatest.FunSuite
import Engine.Direction._
import Engine.FieldState._

class BoardTest extends FunSuite {

  test("testShipIsOnField") {
    val ship = Ship(4, Point(2, 3), Horizontal)
    val board = Board(List(ship), EnemyActions())
    assert(board.shipIsOnField(Point(2, 3)))
    assert(board.shipIsOnField(Point(2, 3).right))
    assert(!board.shipIsOnField(Point(2, 3).left))
    assert(!board.shipIsOnField(Point(2, 3).up))
    assert(!board.shipIsOnField(Point(2, 3).down))

    val anotherShip = Ship(1, Point(0, 0), Horizontal)
    val newBoard = Board(List(ship, anotherShip), EnemyActions())
    assert(newBoard.shipIsOnField(Point(2, 3)))
    assert(newBoard.shipIsOnField(Point(2, 3).right))
    assert(!newBoard.shipIsOnField(Point(2, 3).left))
    assert(!newBoard.shipIsOnField(Point(2, 3).up))
    assert(!newBoard.shipIsOnField(Point(2, 3).down))

    assert(newBoard.shipIsOnField(Point(0, 0)))
    assert(!newBoard.shipIsOnField(Point(0, 0).right))
    assert(!newBoard.shipIsOnField(Point(0, 0).left))
    assert(!newBoard.shipIsOnField(Point(0, 0).up))
    assert(!newBoard.shipIsOnField(Point(0, 0).down))
  }

  test("testGetViewForEnemy") {
    val ship = Ship(2, Point(2, 3), Horizontal)
    val board = Board(List(ship), EnemyActions())
    assert(board.getViewForEnemy.fields == List())

    val enemyShots = EnemyActions().addShot(Point(2, 4))
    val anotherBoard = Board(List(ship), enemyShots)
    assert(anotherBoard.getViewForEnemy.getStateOf(Point(2, 4)) == SunkShip)
  }

  test("testCollide") {
    val ship = Ship(2, Point(2, 3), Horizontal)
    val board = Board(List(ship), EnemyActions())

    val collidingShip = Ship(3, Point(1, 4), Vertical)
    assert(board.collide(collidingShip))

    val nonCollidingShip = Ship(2, Point(0, 4), Vertical)
    assert(!board.collide(nonCollidingShip))
  }

  test("testDeleteShip") {
    val ship = Ship(2, Point(2, 3), Horizontal)
    val ship2 = Ship(1, Point(4, 9), Vertical)

    val board = Board(List(ship, ship2), EnemyActions())
    val newBoard = board.deleteShip(ship)
    assert(newBoard.ships == List(ship2))

    val anotherBoard = board.deleteShip(ship2)
    assert(anotherBoard.ships == List(ship))
  }

  test("testIsShipAlive") {
    val ship = Ship(2, Point(2, 3), Horizontal)
    val enemyShots = EnemyActions().addShot(Point(2, 4))
    val boardWithAliveShip = Board(List(ship), enemyShots)
    assert(boardWithAliveShip.isShipAlive(ship))

    val (boardWithDeadShip, _) = boardWithAliveShip.shotOn(Point(2, 3))
    assert(!boardWithDeadShip.isShipAlive(ship))
  }

  test("testIsAlive") {
    val ship = Ship(2, Point(2, 3), Horizontal)
    val enemyShots = EnemyActions().addShot(Point(2, 4))
    val boardWithAliveShip = Board(List(ship), enemyShots)
    assert(boardWithAliveShip.isAlive)

    val (boardWithDeadShip, _) = boardWithAliveShip.shotOn(Point(2, 3))
    assert(!boardWithDeadShip.isAlive)
  }

  test("testGetNumberOfShipsOnBoard") {
    val ship = Ship(2, Point(2, 3), Horizontal)

    val boardWithOneShip = Board(List(ship), EnemyActions())
    assert(boardWithOneShip.getNumberOfShipsOnBoard(2) == 1)

    val boardWithTwoShip = Board(List(ship, ship), EnemyActions())
    assert(boardWithTwoShip.getNumberOfShipsOnBoard(2) == 2)

    assert(boardWithOneShip.getNumberOfShipsOnBoard(3) == 0)
  }

  test("testShotOn") {
    val board = Board(List(), EnemyActions())
    val (newBoard, hitState) = board.shotOn(Point(2, 3))
    assert(newBoard.enemyHits.wasShotOn(Point(2, 3)))
    assert(hitState == Miss)

    val (anotherBoard, newHitState) = newBoard.shotOn(Point(2, 3))
    assert(newHitState == Incorrect)
    assert(anotherBoard.enemyHits.enemyActions.length == 1)

    val ship = Ship(2, Point(2, 3), Horizontal)
    val boardWithOneShip = Board(List(ship), EnemyActions())
    val (boardSuccessShot, successState) = boardWithOneShip.shotOn(Point(2, 3))
    assert(successState == Hit(Point(2, 3)))
    assert(boardSuccessShot.enemyHits.wasShotOn(Point(2, 3)))

    val (boardWithDeadShip, deadState) = boardSuccessShot.shotOn(Point(2, 4))
    assert(deadState == Sunk)
    assert(boardWithDeadShip.enemyHits.wasShotOn(Point(2, 3)))
    assert(boardWithDeadShip.enemyHits.wasShotOn(Point(2, 4)))

    val (anotherBoard1, newHitState1) = newBoard.shotOn(Point(100, 3))
    assert(newHitState1 == Incorrect)
    assert(anotherBoard1.enemyHits.enemyActions.length == 1)
  }

  test("testGetFieldState") {
    val board = Board(List(), EnemyActions())
    assert(board.getFieldState(Point(9, 9)) == Empty)

    val ship = Ship(2, Point(2, 3), Horizontal)
    val enemyShots = EnemyActions().addShot(Point(2, 4))
    val boardWithAliveShip = Board(List(ship), enemyShots)
    assert(boardWithAliveShip.getFieldState(Point(2, 3)) == AliveShip)
    assert(boardWithAliveShip.getFieldState(Point(2, 4)) == SunkShip)

    val boardWithMissShot = Board(List(), enemyShots)
    assert(boardWithMissShot.getFieldState(Point(2, 4)) == MissShot)
  }

  test("testIsInBoard") {
    val ship = Ship(2, Point(9, 9), Horizontal)
    assert(!Board.isInsideBoard(ship))

    val ship1 = Ship(2, Point(9, 9), Vertical)
    assert(!Board.isInsideBoard(ship1))

    val ship2 = Ship(1, Point(9, 9), Vertical)
    assert(Board.isInsideBoard(ship2))
  }

  test("testIsNotTooCloseToOthers") {
    val ship = Ship(1, Point(1, 1), Horizontal)
    val board = Board(List(ship), EnemyActions())

    (Point(0, 0) to Point(2, 2)).foreach(x => assert(!board.isNotTooCloseToOthers(Ship(1, x, Horizontal))))

    assert(board.isNotTooCloseToOthers(Ship(1, Point(4, 0), Horizontal)))
  }

  test("testAddShip") {
    val ship = Ship(1, Point(1, 1), Horizontal)
    val board = Board(List(ship), EnemyActions())

    // Collide
    (Point(0, 0) to Point(2, 2)).foreach(point => {
      val (_, ifAdded) = board.addShip(Ship(1, point, Horizontal))
      assert(!ifAdded)
    })

    // Outside
    (Point(9, 0) to Point(9, 2)).foreach(point => {
      val (_, ifAdded) = board.addShip(Ship(2, point, Vertical))
      assert(!ifAdded)
    })

    //Too much such ships
    val fullBoard = Board(List(Ship(4, Point(0, 0), Horizontal)), EnemyActions())
    val (_, ifAdded) = fullBoard.addShip(Ship(4, Point(9, 0), Horizontal))
    assert(!ifAdded)

    // Just add
    val (boardWithAdded, ifAdded1) = fullBoard.addShip(Ship(3, Point(9, 0), Horizontal))
    assert(ifAdded1)
    assert(boardWithAdded.ships.contains(Ship(3, Point(9, 0), Horizontal)))
  }

  test("testNumberOfShipsWithGivenLength") {
    val fullBoard = Board(List(Ship(4, Point(0, 0), Horizontal)), EnemyActions())
    assert(fullBoard.getNumberOfShipsOnBoard(4) == 1)
    assert(fullBoard.getNumberOfShipsOnBoard(3) == 0)
  }

  test("testAroundSunkShipWithHits") {
    val board = Board(List(), EnemyActions())
    val newBoard = board.aroundSunkShipWithHits(Ship(1, Point(0, 0), Horizontal))
    assert(!newBoard.enemyHits.wasShotOn(Point(-1, -1)))
    assert(!newBoard.enemyHits.wasShotOn(Point(-1, 0)))
    assert(!newBoard.enemyHits.wasShotOn(Point(0, -1)))
    assert(!newBoard.enemyHits.wasShotOn(Point(1, -1)))
    assert(!newBoard.enemyHits.wasShotOn(Point(-1, 1)))
    assert(newBoard.enemyHits.wasShotOn(Point(1, 1)))
    assert(newBoard.enemyHits.wasShotOn(Point(0, 1)))
    assert(newBoard.enemyHits.wasShotOn(Point(0, 0)))
    assert(newBoard.enemyHits.wasShotOn(Point(1, 0)))
  }
}
