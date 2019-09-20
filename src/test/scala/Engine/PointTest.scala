package Engine

import org.scalatest.FunSuite

class PointTest extends FunSuite {

  test("testDown") {
    val point = Point(0, 0)
    assert(point.down == Point(1, 0))
  }

  test("testDownFewStep") {
    val point = Point(0, 0)
    assert(point.down(2) == Point(2, 0))
  }

  test("testUp") {
    val point = Point(1, 0)
    assert(point.up == Point(0, 0))
  }

  test("testUpFewSteps") {
    val point = Point(2, 0)
    assert(point.up(2) == Point(0, 0))
  }

  test("testRight") {
    val point = Point(0, 0)
    assert(point.right == Point(0, 1))
  }

  test("testRightFewSteps") {
    val point = Point(0, 0)
    assert(point.right(2) == Point(0, 2))
  }

  test("testLeft") {
    val point = Point(0, 2)
    assert(point.left == Point(0, 1))
  }

  test("testLeftFewSteps") {
    val point = Point(0, 2)
    assert(point.left(2) == Point(0, 0))
  }

  test("testUntil") {
    val points = List(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1))
    val generatedPoints = Point(0, 0) until Point(2, 2)
    assert(generatedPoints.toList == points)
  }

  test("testTo") {
    val points = List(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1))
    val generatedPoints = Point(0, 0) to Point(1, 1)
    assert(generatedPoints.toList == points)
  }

  test("testIsOnLineBetween") {
    val point1 = Point(0, 0)
    val point2 = Point(0, 3)
    val point3 = Point(0, 1)
    assert(point3.isOnLineBetween(point1, point2))
    assert(!point1.isOnLineBetween(point2, point3))
    val point4 = Point(1, 0)
    assert(!point4.isOnLineBetween(point1, point2))
  }

}
