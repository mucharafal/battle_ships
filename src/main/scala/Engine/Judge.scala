package Engine

import scala.annotation.tailrec

object Judge {
  def startGame(player1: Player, player2: Player): Unit = {
    val boardPlayer1 = player1.generateNewBoard()
    val boardPlayer2 = player2.generateNewBoard()

    proceedGame(player1, player2, boardPlayer1, boardPlayer2)
  }

  @tailrec final def proceedGame(playerWithMove: Player, waitingPlayer: Player, boardPlayerWithMove: Board,
                  boardWaitingPlayer: Board) {
    boardPlayerWithMove.isAlive match {
      case true if boardWaitingPlayer.isAlive =>
        makeMove(playerWithMove, boardWaitingPlayer) match {
          case (newBoard, Hit(point)) =>
            playerWithMove.shipHit(point)
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForEnemy)
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, newBoard)
          case (newBoard, Sunk)=>
            playerWithMove.shipIsSunk()
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForEnemy)
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, newBoard)
          case (newBoard, Miss) =>
            waitingPlayer.enemyShot(boardWaitingPlayer.getViewForEnemy)
            proceedGame(waitingPlayer, playerWithMove, newBoard, boardPlayerWithMove)
          case (newBoard, Incorrect) =>
            print("Incorrect")
            proceedGame(playerWithMove, waitingPlayer, boardPlayerWithMove, newBoard)
        }
      case true =>
        playerWithMove.win()
        waitingPlayer.lost()
      case false =>
        playerWithMove.lost()
        waitingPlayer.win()
    }
  }

  def makeMove(player: Player, enemyBoard: Board): (Board, HitState) = {
    val position = player.makeShot(enemyBoard.getViewForEnemy)
    enemyBoard.shotOn(position)
  }
}
