import AI.{FindDirectionShooter, FinishInDirectionShooter}
import Engine._
import org.scalatest.{FlatSpec, FunSuite}

class FinishInDirectionShooterTest extends FlatSpec {

  "make shot" should "work in vertical" in {
    val field00 = Field(Point(0, 0), FieldState.MissShot)
    val field01 = Field(Point(0, 1), FieldState.MissShot)
    val field10 = Field(Point(1, 0), FieldState.SunkShip)
    val field11 = Field(Point(0, 0), FieldState.Empty)

    val enemyBoardView = BoardRepresentation(List(field00, field01, field10, field11))

    val coordinates = FinishInDirectionShooter(Point(1, 0), Direction.Horizontal).makeShot(enemyBoardView)
    assert(coordinates == Point(1, 1))
  }

  "make shot" should "work in horizontal" in {
    val field00 = Field(Point(0, 0), FieldState.SunkShip)
    val field01 = Field(Point(0, 1), FieldState.SunkShip)

    val enemyBoardView = BoardRepresentation(List(field00, field01))

    val coordinates = FinishInDirectionShooter(Point(0, 1), Direction.Horizontal).makeShot(enemyBoardView)
    assert(coordinates == Point(0, 2))
    val coordinates1 = FinishInDirectionShooter(Point(0, 0), Direction.Horizontal).makeShot(enemyBoardView)
    assert(coordinates1 == Point(0, 2))
  }
}
