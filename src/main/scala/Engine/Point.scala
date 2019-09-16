package Engine

case class Point(x: Int, y: Int) {
  def right(shift: Int): Point = {
    Point(x, y + shift)
  }

  def right: Point = Point(x, y + 1)

  def left(shift: Int): Point = {
    Point(x, y - shift)
  }

  def left: Point = Point(x, y - 1)

  def down(shift: Int): Point = {
    Point(x + shift, y)
  }

  def down: Point = Point(x + 1, y)

  def up(shift: Int): Point = {
    Point(x - shift, y)
  }

  def up: Point = Point(x - 1, y)

  def isOnLineBetween(begin: Point, end: Point): Boolean = {
    (begin to end).contains(this)
  }

  def to(otherPoint: Point): IndexedSeq[Point] = {
    for (i <- x to otherPoint.x;
         j <- y to otherPoint.y)
      yield Point(i, j)
  }

  def until(otherPoint: Point): IndexedSeq[Point] = {
    for(i <- x until otherPoint.x;
        j <- y until otherPoint.y)
      yield Point(i, j)
  }
}
