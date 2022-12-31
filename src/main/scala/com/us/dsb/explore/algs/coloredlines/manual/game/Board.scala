package com.us.dsb.explore.algs.coloredlines.manual.game

private[manual] object Board {

  /** Empty or ball of some color. */
  private[game] case class CellState(ballState: Option[BallKind])
  private[game] object CellState {
    private[game] val empty: CellState = CellState(None)
  }

  private[game] def empty: Board =
    new Board(Vector.fill[CellState](BoardOrder * BoardOrder)(CellState.empty), Nil, None)
}

import Board._

/**
 * State of board (just cells; not other game state (e.g., score).)
 */
private[manual] class Board(private[this] val cellStates: Vector[CellState],
                            private[this] val onDeck: Iterable[BallKind],
                           //???? move to game (low-level UI) state:
                           //?????? Tuple2 -> case class
                            private[this] val selectionCoordinates: Option[(RowIndex, ColumnIndex)]
                           ) {

  /** Computes row-major cell-array index from row and column numbers. */
  private[this] def vectorIndex(row: RowIndex, column: ColumnIndex): Int =
    (row.value.value - 1) * BoardOrder + (column.value.value - 1)


  private[manual] def getCellStateAt(row: RowIndex, column: ColumnIndex): CellState = {
    cellStates(vectorIndex(row, column))
  }

  private[game] def getOnDeckBalls: Iterable[BallKind] = onDeck

  private[game] def withOnDeckBalls(newBalls: Iterable[BallKind]): Board =
    new Board(cellStates, newBalls, selectionCoordinates)

  private[game] def isFull: Boolean = ! cellStates.exists(_.ballState.isEmpty)

  private[game] def getBallStateAt(row: RowIndex, column: ColumnIndex): Option[BallKind] = {
    cellStates(vectorIndex(row, column)).ballState
  }
  private[game] def hasABallAt(row: RowIndex, column: ColumnIndex): Boolean = {
    cellStates(vectorIndex(row, column)).ballState.isDefined
  }
  private[manual] def isSelectedAt(row: RowIndex, column: ColumnIndex): Boolean = {
    selectionCoordinates.fold(false)(coords => coords._1 == row && coords._2 == column)
  }
  private[game] def hasABallSelected: Boolean =
    selectionCoordinates.fold(false)(coords => hasABallAt(coords._1, coords._2))

  private[game] def hasAnyCellSelected: Boolean = selectionCoordinates.isDefined

  private[game] def getSelectionCoordinates: Option[(RowIndex, ColumnIndex)] =
    selectionCoordinates

  private def withCellState(row: RowIndex,
                            column: ColumnIndex,
                            newState: CellState): Board =
    new Board(cellStates.updated(vectorIndex(row, column), newState), onDeck, selectionCoordinates)

  private[game] def withCellHavingBall(row: RowIndex,
                                       column: ColumnIndex,
                                       ball: BallKind): Board =
    withCellState(row, column, getCellStateAt(row, column).copy(ballState = Some(ball)))

  private[game] def withCellSelected(row: RowIndex,
                                     column: ColumnIndex): Board =
    new Board(cellStates, onDeck, Some((row, column)))

  private[game] def withNoSelection: Board =
    new Board(cellStates, onDeck, None)

  /*
    getting (multiple) lines of 5 given a cell (with a ball)
    - consider 4 axes (N, NE, E, SE)
    - consider 2 directions per axois
    - for each axis, get number of same-color balls in a row:
      - for each direction
        - keep moving out while next position exists and have ball of same color (and
        - ~combine direction results to get length of line in axes (1 to 9 balls)
    - if no axis row has 5 or more:  place 3 on-deck balls and select next 3
    - if any has 5 or more:
      - for each with five or more:
        - assimilate length into move score (total 5 -> 10 pt, +1 -> +4 pt;  (4 * N - 10)
        - remove balls from cells (watch overlap)
   */

  private[manual] def getStateChar(state: CellState, isSelected: Boolean): String = {  //???? move out
    state.ballState match {
      case Some(ball) => ball.getColoredCharSeq(isSelected)
      case None => if (! isSelected) "-" else "@"
    }
  }

  /** Makes compact single-line string like Xx"<X-O/-X-/O-X>". */
  override def toString: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        getStateChar(getCellStateAt(row, column), isSelectedAt(row, column))
      }.mkString("")
    }.mkString("<", "/", ">")
  }

  private[this] def renderMultiline: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
          "" + getStateChar(getCellStateAt(row, column), isSelectedAt(row, column)) + " "
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

  private[this] def renderCompactMultiline: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        getStateChar(getCellStateAt(row, column), isSelectedAt(row, column))
      }.mkString("|")  // make each row line
    }.mkString("\n")
  }

}

