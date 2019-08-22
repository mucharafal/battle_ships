object Main {
  def main(args: Array[String]): Unit = {
    val player1 = new PlayerCLI()
    val player2 = new PlayerCLI()

    val judge = new Judge(player1, player2)
    judge.processGame()
  }
}
