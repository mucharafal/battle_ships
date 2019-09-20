package Engine

case class EnemyActions(enemyActions: List[Point]) {
  def fieldIsClear(point: Point): Boolean = {
    !wasShotOn(point)
  }

  def wasShotOn(point: Point): Boolean = {
    enemyActions.contains(point)
  }

  def addShot(point: Point): EnemyActions = {
    EnemyActions(point::enemyActions)
  }
}

object EnemyActions {
  def apply(): EnemyActions = new EnemyActions(List.empty[Point])
}
