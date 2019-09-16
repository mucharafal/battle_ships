package Engine

import org.scalatest.FunSuite
import Engine.Direction._


class ShipTest extends FunSuite {

  test("testIsIn") {
    val ship = Ship(3, Point(0, 0), Horizontal)
    val pointInside = Point(0, 0)
    assert(ship.isIn(pointInside))
    val pointOutside = Point(1, 2)
    assert(!ship.isIn(pointOutside))
    assert(ship.isIn(pointInside.right))
    assert(ship.isIn(Point(0, 2)))
  }

  test("testCollide") {
    val testPermutationCollision = (ship1: Ship, ship2: Ship) => {
      assert(ship1.collide(ship2))
      assert(ship2.collide(ship1))
    }

    val testPermutationNoCollision = (ship1: Ship, ship2: Ship) => {
      assert(!ship1.collide(ship2))
      assert(!ship2.collide(ship1))
    }
    val ship = Ship(2, Point(0, 0), Horizontal)
    testPermutationCollision(ship, ship)
    val otherShip = Ship(1, ship.position.right, Horizontal)
    testPermutationCollision(ship, otherShip)
    val nonColideShip = Ship(4, ship.position.down, Vertical)
    testPermutationNoCollision(ship, nonColideShip)
  }

  test("testGetListOfFieldsCooridinates") {

  }

  test("testEndPoint") {

  }

}
