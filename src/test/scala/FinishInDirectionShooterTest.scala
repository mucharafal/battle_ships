import AI.FinishInDirectionShooter
import Engine.{Direction, FieldState, Point}
import org.scalatest.{FlatSpec, FunSuite}

class FinishInDirectionShooterTest extends FlatSpec {

  "make shot" should "work in vertical" in {
    val enemyBoardView = Array(Array(FieldState.SunkShip, FieldState.MissShot),
      Array(FieldState.SunkShip, FieldState.Empty))
    val coordinates = FinishInDirectionShooter(Point(0, 1), Direction.Vertical).makeShot(enemyBoardView)
    assert(coordinates == Point(1, 1))
  }

  "make shot" should "work in horizontal" in {
    val enemyBoardView = Array(Array(FieldState.SunkShip, FieldState.SunkShip, FieldState.Empty),
      Array(FieldState.SunkShip, FieldState.Empty, FieldState.Empty),
      Array(FieldState.Empty, FieldState.Empty, FieldState.Empty))
    val coordinates = FinishInDirectionShooter(Point(0, 1), Direction.Horizontal).makeShot(enemyBoardView)
    assert(coordinates == Point(0, 2))
    val coordinates1 = FinishInDirectionShooter(Point(0, 0), Direction.Horizontal).makeShot(enemyBoardView)
    assert(coordinates1 == Point(0, 2))
  }
}
