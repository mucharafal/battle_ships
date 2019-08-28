object Main {
  def main(args: Array[String]): Unit = {
    val player1 = new PlayerAgent()
    val player2 = new PlayerAgent

    val judge = new Judge(player1, player2)
    judge.processGame()
  }
}
