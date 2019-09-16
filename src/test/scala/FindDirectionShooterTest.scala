import AI.FindDirectionShooter
import Engine.{BoardRepresentation, Field, FieldState, Point}
import org.scalatest.FlatSpec

class FindDirectionShooterTest extends FlatSpec {
  "make shot" should "work" in {
    val field00 = Field(Point(0, 0), FieldState.MissShot)
    val field01 = Field(Point(0, 1), FieldState.MissShot)
    val field10 = Field(Point(1, 0), FieldState.SunkShip)
    val field11 = Field(Point(0, 0), FieldState.Empty)

    val enemyBoardView = BoardRepresentation(List(field00, field01, field10, field11))
    val coordinates = FindDirectionShooter(Point(1, 0)).makeShot(enemyBoardView)
    assert(coordinates == Point(1, 1) || coordinates == Point(2, 0))
  }
}
