package Engine

import org.scalatest.FunSuite

class EnemyActionsTest extends FunSuite {

  test("testFieldIsClear") {
    val enemyActions = EnemyActions(List(Point(2, 2), Point(2, 4)))
    assert(!enemyActions.fieldIsClear(Point(2, 4)))
    assert(!enemyActions.fieldIsClear(Point(2, 2)))
    assert(enemyActions.fieldIsClear(Point(2, 3)))
  }

  test("testAddShot") {
    val enemyActions = EnemyActions()
    val addedAction = enemyActions.addShot(Point(2, 3))
    assert(addedAction.enemyActions == List(Point(2, 3)))
  }

  test("testWasShotOn") {
    val enemyActions = EnemyActions(List(Point(2, 2), Point(2, 4)))
    assert(enemyActions.wasShotOn(Point(2, 4)))
    assert(enemyActions.wasShotOn(Point(2, 2)))
    assert(!enemyActions.wasShotOn(Point(2, 3)))
  }

}
