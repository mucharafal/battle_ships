import scala.annotation.tailrec

class Judge(player1: Player, player2: Player) {
  def processGame(): Unit = {
    val boardPlayer1 = player1.generateNewBoard()
    val boardPlayer2 = player2.generateNewBoard()

    proceedGame(player1, player2, boardPlayer1, boardPlayer2)
  }

  @tailrec final def proceedGame(playerWithMove: Player, waitingPlayer: Player, boardPlayerWithMove: Board,
                  boardWaitingPlayer: Board) {
    boardPlayerWithMove.isAlive match {
      case true if boardWaitingPlayer.isAlive =>
        makeMove(playerWithMove, boardWaitingPlayer) match {
          case Hit(x, y) =>
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForOwner)
            playerWithMove.shipHit(x, y)
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, boardWaitingPlayer)
          case Sunk =>
            playerWithMove.shipIsSunk()
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForOwner)
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, boardWaitingPlayer)
          case Miss =>
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForOwner)
            proceedGame(waitingPlayer, playerWithMove, boardWaitingPlayer, boardPlayerWithMove)
          case Incorrect =>
            print("Incorrect")
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, boardWaitingPlayer)
        }
      case true =>
        playerWithMove.endGame(true)
        waitingPlayer.endGame(false)
      case false =>
        playerWithMove.endGame(false)
        waitingPlayer.endGame(true)
    }
  }

  def makeMove(player: Player, enemyBoard: Board): HitState = {
    val (shotX, shotY) = player.makeShot(enemyBoard.getViewForEnemy)
    enemyBoard.shotOn(shotX, shotY)
  }
}
