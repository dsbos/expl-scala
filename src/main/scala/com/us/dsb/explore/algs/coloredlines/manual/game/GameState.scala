package com.us.dsb.explore.algs.coloredlines.manual.game

import cats.syntax.option._
import cats.syntax.either._
import com.us.dsb.explore.algs.coloredlines.manual.game.BallKind
import com.us.dsb.explore.algs.coloredlines.manual.game.GameState.GameResult.PlaceholderDone
import com.us.dsb.explore.algs.coloredlines.manual.game.{ColumnIndex, RowIndex}

import scala.util.Random


private[manual] object GameState {

  /**
   * Result of completed game.
   */
  private[manual] sealed trait GameResult //???? change to final score (and maybe stats?)
  private[manual] object GameResult {
    private[manual] case class PlaceholderDone(stats: Float) extends GameResult
  }

  private[this] def xxselectRandomEmptyCell(rng: Random, board: Board): (RowIndex, ColumnIndex) = {
    val row = rowIndices(rng.nextInt(BoardOrder))
    val col = columnIndices(rng.nextInt(BoardOrder))
    if (board.getBallStateAt(row, col).isEmpty)
      (row, col)
    else
      xxselectRandomEmptyCell(rng, board) // loop: try again
  }

  private def getRandomBallKind(rng: Random): BallKind = BallKind.values(rng.nextInt(BallKind.values.size))

  private[this] def xxmakeInitialState(rng: Random): GameState = {

    val board1 = Board.empty

    val board3 =
      (1 to 5).foldLeft(board1) {
        case (board2, _) =>
          val (row, column) = xxselectRandomEmptyCell(rng, board2)
          board2.withCellHavingBall(row, column, getRandomBallKind(rng))
      }

    //?????? add 3 on-back balls


    GameState(rng, board3, None)
  }

  private[game] def initial(seed: Long): GameState = xxmakeInitialState(new Random(seed))
  private[manual] def initial: GameState = xxmakeInitialState(new Random())
}
import GameState._

/**
 * TTT game state _and_ controller--should functions be separated or together?
 *
 * @param gameResult  `None` means no win or draw yet
 */
 //???? add random-data state


/** Game state AND currently controller.
 */
private[manual] case class GameState(rng: Random,
                                     board: Board,
                                     gameResult: Option[GameResult]
                                    ) {

  private[this] def xxpickRandomEmptyCell(): Option[(RowIndex, ColumnIndex)] = {
    if (board.isFull)
      None
    else {
      val row = rowIndices(rng.nextInt(BoardOrder))
      val col = columnIndices(rng.nextInt(BoardOrder))
      if (board.getBallStateAt(row, col).isEmpty)
        Some((row, col))
      else
        xxpickRandomEmptyCell() // loop: try again
    }
  }

  private[this] def doPass(): Board = {
    //?????? scatter from real on-deck list, and replenish it too
    val onDeckListPlaceholder = List.fill(3)(getRandomBallKind(rng))
    val newBoard =
      onDeckListPlaceholder
        .foldLeft(board) {
          case (board2, _) =>
            xxpickRandomEmptyCell() match {
              case None => board2
              case Some((row, column)) =>
                board2.withCellHavingBall(row, column, getRandomBallKind(rng))
            }
        }
    newBoard
  }

  // ?? later refine from Either[String, ...] to "fancier" error type
  // Xx?? maybe add result of move (win/draw/other) with new state (so caller
  //  doesn't have to check state's gameResult; also, think about where I'd add
  //  game history
  private[manual] def tryMoveAt(row: RowIndex,
                                column: ColumnIndex
                               ): Either[String, GameState] = {
    sealed trait Action
    object Action {
      private[GameState] case object SelectBall  extends Action
      private[GameState] case object SelectEmpty extends Action
      private[GameState] case object Deselect    extends Action
      private[GameState] case object TryMoveBall extends Action
      private[GameState] case object Pass        extends Action
    }
    import Action._

    sealed trait OnBallOrEmpty
    case object OnBall  extends OnBallOrEmpty
    case object OnEmpty extends OnBallOrEmpty

    sealed trait OnSelOrUnsel
    case object OnSel   extends OnSelOrUnsel
    case object OnUnsel extends OnSelOrUnsel

    sealed trait HadBallOrNot
    case object HadBall extends HadBallOrNot
    case object NoBall  extends HadBallOrNot

    sealed trait HadSelOrNot
    case object HadSel extends HadSelOrNot
    case object NoSel   extends HadSelOrNot

    val onBallOrEmpty = if (board.hasABallAt(row, column))   OnBall  else OnEmpty
    val onSelOrUnsel  = if (board.isSelectedAt(row, column)) OnSel   else OnUnsel
    val hadBallOrNot  = if (board.hasABallSelected)          HadBall else NoBall
    val hadSelOrNot   = if (board.hasAnyCellSelected)        HadSel  else NoSel
    val action: Action = {
        val conditions = (onBallOrEmpty, hadBallOrNot, onSelOrUnsel, hadSelOrNot)
        conditions match {
          case (OnBall,  _,       OnUnsel, _     ) => SelectBall   // - tap on ball  when unselected
          case (OnEmpty, HadBall, _,       _     ) => TryMoveBall  // - tap on empty when ball selected

          case (OnEmpty, NoBall,  OnUnsel, NoSel ) => SelectEmpty  // - tap on empty when no selection
          case (OnEmpty, NoBall,  OnUnsel, HadSel) => Pass         // - tap on empty when selected

          case (_,       _,       OnSel,   _     ) => Deselect     // - tap on either when selected
        }
    }
    val nextBoard =
      action match {
        case SelectBall |
             SelectEmpty => board.withCellSelected(row, column)
        case Deselect    => board.withNoSelection
        case TryMoveBall =>
          //?????? do ... path check, ball update, on deck, etc. around here
          println("NIY: " + action); board.withNoSelection
        case Pass        => doPass().withNoSelection  //??? clean board ref

      }

    val nextState =
      if (! nextBoard.isFull) {
        GameState(rng, nextBoard, gameResult).asRight
      }
      else {
        GameState(rng, nextBoard, Some(PlaceholderDone(1.23f))).asRight
      }
    nextState
  }

}
