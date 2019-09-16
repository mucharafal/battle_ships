import AI.PlayerAgent
import CLI.PlayerCLI
import Engine.Judge

object Main {
  def main(args: Array[String]): Unit = {
    val player1 = new PlayerAgent
    val player2 = new PlayerAgent

    Judge.startGame(player1, player2)
    print("End")
  }
}
