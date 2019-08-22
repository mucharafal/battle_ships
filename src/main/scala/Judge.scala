import HitState._

class Judge(player1: Player, player2: Player) {
  def processGame(): Unit = {
    val boardPlayer1 = player1.generateNewBoard()
    val boardPlayer2 = player2.generateNewBoard()

    proceedGame(player1, player2, boardPlayer1, boardPlayer2)
  }

  def proceedGame(playerWithMove: Player, waitingPlayer: Player, boardPlayerWithMove: Board,
                  boardWaitingPlayer: Board) {
    boardPlayerWithMove.isAlive match {
      case true if boardWaitingPlayer.isAlive =>
        makeMove(playerWithMove, boardWaitingPlayer) match {
          case Hit =>
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, boardWaitingPlayer)
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForOwner)
          case Sunk =>
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, boardWaitingPlayer)
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForOwner)
            playerWithMove.shipIsSunk()
          case Miss =>
            proceedGame(waitingPlayer, playerWithMove, boardWaitingPlayer, boardPlayerWithMove)
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForOwner)
          case Incorrect =>
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
