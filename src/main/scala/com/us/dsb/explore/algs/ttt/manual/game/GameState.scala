package com.us.dsb.explore.algs.ttt.manual.game

import cats.syntax.option._
import cats.syntax.either._


object GameState {

  /**
   * Result of completed game.
   */
  sealed trait GameResult
  object GameResult {
    case object Draw extends GameResult
    case class  Win(player: Player) extends GameResult
  }

  def initial(startingPlayer: Player): GameState =
    GameState(Board.initial, None, startingPlayer)
  def initial: GameState = initial(Player.X)
}

/**
 * TTT game state _and_ controller--should functions be separated or together?
 *
 * @param gameResult  `None` means no win or draw yet
 */
case class GameState(board: Board,
                     gameResult: Option[GameState.GameResult],
                     currentPlayer: Player
                    ) {

  // ?? later refine from Either[String, ...] to "fancier" error type
  // ?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult;

  def tryMoveAt(row: RowIndex,
                column: ColumnIndex
               ): Either[String, GameState] = {
    board.getMarkAt(row, column) match {
      case None =>
        val markedBoard = board.withCellMarkedForPlayer(row, column, currentPlayer)
        val newGameResult =
          if (markedBoard.hasThreeInARow(currentPlayer)) {
            GameState.GameResult.Win(currentPlayer).some
          }
          else if (markedBoard.hasNoMovesLeft) {
            GameState.GameResult.Draw.some
          }
          else {
            gameResult
          }
        import Player._
        val nextPlayer = currentPlayer match {
          case X => O
          case O => X
        }
        GameState(markedBoard, newGameResult, nextPlayer).asRight
      case Some(nameThis) =>
        (s"Can't place mark at row $row, column $column;" +
            s" is already marked (${nameThis})").asLeft
    }
  }

}
