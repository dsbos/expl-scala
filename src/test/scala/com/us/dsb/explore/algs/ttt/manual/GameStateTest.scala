package com.us.dsb.explore.algs.ttt.manual

import org.scalatest.funspec.AnyFunSpec
import cats.syntax.option._

import scala.util.chaining.scalaUtilChainingOps

class GameStateTest extends AnyFunSpec {


  describe("GameState$?. tryMoveAt") {
    import Player._
    import scala.language.implicitConversions
    implicit def intToRow(int: Int) = RowIndex(Index.unsafeFrom(int))
    implicit def intToCol(int: Int) = ColumnIndex(Index.unsafeFrom(int))


    ignore("marks cells") {

    }
    ignore("accepts marking an unmarked cell") {

    }
    ignore("rejects marking an already marked cell") {

    }
    describe("detects wins:") {
      it("one case") {

        val winState =
          GameState.initial(Player.X)
              .tryMoveAt(1, 1).toOption.get
              .tryMoveAt(3, 1).toOption.get
              .tryMoveAt(1, 2).toOption.get
              .tryMoveAt(3, 2).toOption.get
              .tryMoveAt(1, 3).toOption.get
        assertResult(GameState.GameResult.Win(Player.X).some, s" (winState = $winState)") {
          winState.gameResult
        }
      }
      ignore("do more cases") {
      }
    }
    describe("detects draws") {
      it("one case") {

        val drawState =
          GameState.initial(Player.X)
              // .tap(s => println(s.board.renderCompactMultiline + "\n"))
              .tryMoveAt(1, 1).toOption.get
              .tryMoveAt(2, 1).toOption.get
              .tryMoveAt(1, 2).toOption.get
              .tryMoveAt(2, 2).toOption.get
              .tryMoveAt(3, 1).toOption.get
              .tryMoveAt(1, 3).toOption.get
              .tryMoveAt(2, 3).toOption.get
              .tryMoveAt(3, 3).toOption.get
              .tryMoveAt(3, 2).toOption.get
        assertResult(GameState.GameResult.Draw.some, s" (drawState = $drawState)") {
          drawState.gameResult
        }
      }
      ignore("do more cases") {
      }
    }

    ignore("??? basic") {
      val state0 = GameState.initial
      val result1 = state0.tryMoveAt(rowIndices.head, columnIndices.head)
      val state1 = result1.toOption.get

    }

  }


}
