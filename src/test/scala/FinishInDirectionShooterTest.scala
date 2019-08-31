import AI.FinishInDirectionShooter
import Engine.{Direction, FieldState}
import org.scalatest.{FlatSpec, FunSuite}

class FinishInDirectionShooterTest extends FlatSpec {

  "make shot" should "work in vertical" in {
    val enemyBoardView = Array(Array(FieldState.SunkShip, FieldState.MissShot),
      Array(FieldState.SunkShip, FieldState.Empty))
    val coordinates = FinishInDirectionShooter(0, 1, Direction.Vertical).makeShot(enemyBoardView)
    assert(coordinates == (1, 1))
  }

  "make shot" should "work in horizontal" in {
    val enemyBoardView = Array(Array(FieldState.SunkShip, FieldState.SunkShip, FieldState.Empty),
      Array(FieldState.SunkShip, FieldState.Empty, FieldState.Empty),
      Array(FieldState.Empty, FieldState.Empty, FieldState.Empty))
    val coordinates = FinishInDirectionShooter(0, 1, Direction.Horizontal).makeShot(enemyBoardView)
    assert(coordinates == (0, 2))
    val coordinates1 = FinishInDirectionShooter(0, 0, Direction.Horizontal).makeShot(enemyBoardView)
    assert(coordinates1 == (0, 2))
  }
}
