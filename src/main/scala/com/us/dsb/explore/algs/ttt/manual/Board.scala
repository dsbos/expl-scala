package com.us.dsb.explore.algs.ttt.manual

import cats.syntax.option._
import cats.syntax.either._


object Board {
  /** Empty, X, or O */
  /*@newtype deferred*/
  private case class Cell(state: Option[Player])  // if newType, has to be in object

  private object Cell {
    val empty: Cell = Cell(None)
  }

  def initial: Board = new Board(Vector.fill[Cell](Order * Order)(Cell.empty))
}

import Board._

/**
 * State of TTT board (just cells; not whose turn it is/etc.)
 */
class Board(private val cellStates: Vector[Cell]) {

  /** Computes row-major cell-array index from row and column numbers. */
  private def vectorIndex(row: RowIndex, column: ColumnIndex): Int =
    (row.value.value - 1) * Order + (column.value.value - 1)

  def getMarkAt(row: RowIndex, column: ColumnIndex): Option[Player] = {
    cellStates(vectorIndex(row, column)).state
  }

  def hasNoMovesLeft: Boolean = {
    ! cellStates.exists(_.state.isEmpty)
  }

  // (note almost completely unoptimized; do we care?)
  // ?? revisit passing player (which made check simpler)
  def hasThreeInARow(player: Player): Boolean = {
    // ?? Q: How to use Tuple3 in data and then convert to or use as 3-element
    //   List and call .forall?
    // ?? any way auto-generate this simply?  or factor out repeated cell pairs?
    type CellRawIndices = Tuple2[Int, Int]
    val linesData: List[Tuple3[CellRawIndices, CellRawIndices, CellRawIndices]] =
      List(
        ((1, 1), (1, 2), (1, 3)),
        ((2, 1), (2, 2), (2, 3)),
        ((3, 1), (3, 2), (3, 3)),
        ((1, 1), (2, 1), (3, 1)),
        ((1, 2), (2, 2), (3, 2)),
        ((1, 3), (2, 3), (3, 3)),
        ((1, 1), (2, 2), (3, 3)),
        ((1, 3), (2, 2), (3, 1))
    )
    // See if there _exists_ line where _all_ cells are marked by user:
    linesData.exists { case (cell1, cell2, cell3) =>
      val lineList = List(cell1, cell2, cell3)
      lineList.forall { case (row, column) =>
        player.some ==
            getMarkAt(RowIndex(Index.unsafeFrom(row)),
                      ColumnIndex(Index.unsafeFrom(column)))
        }
    }
  }

  // ?? "withCellMarkedBy" ("with" re immutable state
  /** Marks specified cell, if not already marked. */
  def markCell(player: Player,
               row: RowIndex,
               column: ColumnIndex): Either[String, Board] = {
    getMarkAt(row, column) match {
      case None =>
        val newCellArray =
          cellStates.updated(vectorIndex(row, column), Cell(player.some))
        new Board(newCellArray).asRight
      case Some(nameThis) =>
        (s"Can't place mark at row $row, column $column;" +
            s" is already marked (${nameThis})").asLeft    }
  }

  /** Makes compact single-line string like "<X-O/-X-/O-X>". */
  override def toString: String = {
    rowIndices.map { row =>
      columnIndices.map { column =>
        getMarkAt(row, column) match {
          case None         => "-"
          case Some(player) => player.toString
        }
      }.mkString("")
    }.mkString("<", "/", ">")
  }

  def renderMultiline: String = {
    val cellWidth = " X ".length
    val cellSeparator = "|"
    // ?? use new Order or leave using indices declarations?
    val wholeWidth =
      columnIndices.length * cellWidth +
          (columnIndices.length - 1) * cellSeparator.length
    val rowSeparator = "\n" + ("-" * wholeWidth) + "\n"

    rowIndices.map { row =>
      columnIndices.map { column =>
        getMarkAt(row, column) match {
          case None         => " - "
          case Some(player) => " " + player.toString + " "
        }
      }.mkString(cellSeparator)  // make each row line
    }.mkString(rowSeparator)     // make whole-board multi-line string
  }

}

