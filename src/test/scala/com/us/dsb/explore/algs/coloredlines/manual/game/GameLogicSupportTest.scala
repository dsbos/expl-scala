package com.us.dsb.explore.algs.coloredlines.manual.game

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport.MoveResult
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BoardPlus, ColumnIndex, RowIndex, columnIndices, rowIndices}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.Random

class GameLogicSupportTest extends AnyFunSpec {

  describe("pickRandomBallKind") {pending}

  describe("pickRandomEmptyCell") {
    it("???NIY") {
      pending
    }
  }
  describe("placeInitialBalls") {
    it("???NIY") {
      pending
    }
  }
  ignore("interpretTapLocationToTapAction") {
    it("???NIY") {
      pending
    }
  }
  ignore("tapAndStateToTapAction") {
    it("???NIY") {
      pending
    }
  }
  describe("placeNextBalls") {
    it("???NIY") {
      pending
    }
  }

  describe("pathExists:") {
    implicit val rng: Random = new Random()
    lazy val boardPlus0 = BoardPlus.empty

    it("ball on one-ball board can move anywhere") {
      //?? factor out this frequent iteration pattern (set of cells, iterate, passing CellAddress
      rowIndices.foreach { ballRow =>
        columnIndices.foreach { ballColumn =>
          val fromBallAddress = CellAddress(ballRow, ballColumn)
          val boardPlus = boardPlus0.withBallAt(fromBallAddress,
                                        GameLogicSupport.pickRandomBallKind())
          rowIndices.foreach { row =>
            columnIndices.foreach { column =>
              val toVacancyAddress = CellAddress(row, column)
              val pathExists = GameLogicSupport.pathExists(boardPlus, fromBallAddress, toVacancyAddress)
              withClue( s"from $fromBallAddress to $toVacancyAddress") {
                pathExists shouldBe true
              }
            }
          }
        }
      }
    }

    // top left (1, 1) to bottom right (N, N)
    def makeDiagonallyDividedBoard: BoardPlus = {
      val diagonalAddresses =
        rowIndices.zip(columnIndices).map { case (row, column) => CellAddress(row, column) }
      diagonalAddresses.foldLeft(boardPlus0) { case (board, address) =>
        board.withBallAt(address, GameLogicSupport.pickRandomBallKind())
      }
    }

    it("ball can't move across block (complete diagonal; random probe ball location)") {
      val probeBall = GameLogicSupport.pickRandomBallKind()
      val diagonalBoardPlus = makeDiagonallyDividedBoard
      val fromBallAddress = GameLogicSupport.pickRandomEmptyCell(diagonalBoardPlus).get
      val boardWithProbe = diagonalBoardPlus.withBallAt(fromBallAddress, probeBall)

      // transpose ball coordinates to get cell across boundary
      val toVacancyAddress =
        CellAddress(row = RowIndex(fromBallAddress.column.value),
                    column = ColumnIndex(fromBallAddress.row.value))

      val pathExists = GameLogicSupport.pathExists(boardWithProbe, fromBallAddress, toVacancyAddress)
      pathExists shouldBe false
    }

    it("???NIY") {
      pending
    }
  }

  ignore("doTryMoveBall") {
    it("???NIY") {
      pending
    }
  }
  describe("doPass") {
    it("???NIY") {
      pending
    }
  }




}