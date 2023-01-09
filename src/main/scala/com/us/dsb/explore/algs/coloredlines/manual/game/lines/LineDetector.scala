package com.us.dsb.explore.algs.coloredlines.manual.game.lines

import com.us.dsb.explore.algs.coloredlines.manual.game.board.CellAddress
import com.us.dsb.explore.algs.coloredlines.manual.game.GameLogicSupport.MoveResult
import com.us.dsb.explore.algs.coloredlines.manual.game._
import com.us.dsb.explore.algs.coloredlines.manual.game.board.{BallKind, BoardPlus, BoardOrder, LineOrder}

//???? TODO:  reduce repeated passing of board, etc.; maybe make LineDetector a
// class, to be instantiated for each move; or make local class for passing (but
// leave external-client interface same
object LineDetector {  //????? adjust most from using BoardPlus to using just BoardState

  private[lines] case class LineAxis(labelArray: String,
                                     rowDelta: Int, // -1 / 0 / 1 (Make refined type?)
                                     colDelta: Int)

  private[this] val lineAxes =
    List(
      LineAxis("→",  0, +1), // →  W -->  E
      LineAxis("↘", +1, +1), // ↘ NW --> SE
      LineAxis("↓", +1,  0), // ↓ N  --> S
      LineAxis("↙", +1, -1)) // ↙ NW --> SW

  private[this] val relativeDirectionFactors = List(1, -1) // use type of length 2 (refined List?, Tuple2?, some array?)

  private[lines] def haveMatchingBallAt(moveBallColor: BallKind,
                                        boardPlus: BoardPlus,
                                        rawRowIndex: Int,
                                        rawColIndex: Int): Boolean = {
    val inRange =
      1 <= rawRowIndex && rawRowIndex <= BoardOrder &&
          1 <= rawColIndex && rawColIndex <= BoardOrder
    val haveMatch =
      inRange && {
        val candidateAddress = CellAddress.fromRaw(rawRowIndex, rawColIndex)
        boardPlus.getBallStateAt(candidateAddress).fold(false)(ball => ball == moveBallColor)
      }
    haveMatch
  }

  private[lines] case class RelativeDirectionResult(excursionLength: Int)

  private[lines] def computeDirectionResult(moveBallColor: BallKind,
                                            boardPlus: BoardPlus,
                                            ballTo: CellAddress,
                                            lineDirectionAxis: LineAxis,
                                            directionFactor: Int): RelativeDirectionResult = {
    val newBallRowIndex = ballTo.row.value.value
    val newBallColIndex = ballTo.column.value.value
    import lineDirectionAxis.{colDelta, rowDelta}
    var excursionLength = 0
    while ( {
      val candidateExcursionLength = excursionLength + 1
      val candidateRowIndex = newBallRowIndex + rowDelta * directionFactor * candidateExcursionLength
      val candidateColIndex = newBallColIndex + colDelta * directionFactor * candidateExcursionLength
      //??? println(s"    ??.n.0: candidate address: ($candidateRowIndex / $candidateColIndex)")

      val haveMatchingBall = haveMatchingBallAt(moveBallColor, boardPlus, candidateRowIndex, candidateColIndex)
      if (haveMatchingBall) {
        excursionLength = candidateExcursionLength
      }
      haveMatchingBall
    }) {}
    RelativeDirectionResult(excursionLength)
  }

  private[lines] case class AxisResult(axis: LineAxis,
                                       axisLineAddedLength: Int, // length WITHOUT moved ball
                                       directionDetails: List[RelativeDirectionResult])

  private[lines] def computeLineAxisResult(moveBallColor: BallKind,
                                           boardPlus: BoardPlus,
                                           ballTo: CellAddress,
                                           lineDirectionAxis: LineAxis): AxisResult = {
    //??? println(s"+  computeLineAxisResult( axis = $lineDirectionAxis ).1")
    val directionsResults: List[RelativeDirectionResult] =
      relativeDirectionFactors.map { directionFactor =>
        computeDirectionResult(moveBallColor,
                               boardPlus,
                               ballTo,
                               lineDirectionAxis,
                               directionFactor)
      }
    val axisLineAddedLength = directionsResults.map(_.excursionLength).sum

    val result = AxisResult(lineDirectionAxis, axisLineAddedLength, directionsResults) //????
    //??? println(s"-  computeLineAxisResult( axis = $lineDirectionAxis ).9 result = $result")
    result
  }

  private[lines] def removeCompletedLineBalls(ballTo: CellAddress,
                                              preremovalBoardPlus: BoardPlus,
                                              completedLineAxesResults: List[AxisResult]): BoardPlus = {
    val newBallRemovedBoard = preremovalBoardPlus.withNoBallAt(ballTo)
    val linesRemovedBoard =
      completedLineAxesResults.foldLeft(newBallRemovedBoard) { case (axisBoard, axisResult) =>
        val fromOffset = -axisResult.directionDetails(1).excursionLength
        val toOffset = axisResult.directionDetails(0).excursionLength
        val lineRemovedBoard =
          (fromOffset to toOffset).foldLeft(axisBoard) { case (directionBoard, offset) =>
            import axisResult.axis.{colDelta, rowDelta}
            val rawRowIndex = ballTo.row.value.value    + rowDelta * offset
            val rawColIndex = ballTo.column.value.value + colDelta * offset
            val cellAddress = CellAddress.fromRaw(rawRowIndex, rawColIndex)
            directionBoard.withNoBallAt(cellAddress)
          }
        lineRemovedBoard
      }
    linesRemovedBoard
  }

  /**
   * @return
   * None if no line(s) completed; score increment otherwise
   */
  //???? rename?:  harvestAnyLines?
  private[game] def handleBallArrival(boardPlus: BoardPlus,
                                      ballTo: CellAddress
                                     ): MoveResult = {
    //println(s"+handleBallArrival(... ballTo = $ballTo...).1")
    val moveBallColor = boardPlus.getBallStateAt(ballTo).get //????

    val allAxesResults: List[AxisResult] =
      lineAxes.map { lineAxis =>
        computeLineAxisResult(moveBallColor, boardPlus, ballTo, lineAxis)
      }
    //??? println("??? allAxesResults:" + allAxesResults.mkString("\n- ", "\n- ", ""))
    val completedLineAxesResults = allAxesResults.filter(_.axisLineAddedLength + 1 >= LineOrder)
    //println("??? completedLineAxesResults:" + completedLineAxesResults.map("- " + _.toString).mkString("\n", "\n", "\n:end"))
    val (boardResult, scoreResult) =
      completedLineAxesResults match {
        case Nil =>
          (boardPlus, None) // return None for score (signal to place 3 more IF ball moved by user)
        case linesAxes =>
          val totalBallsBeingRemoved = 1 + linesAxes.map(_.axisLineAddedLength).sum
          println(s" scoreMove(... ballTo = $ballTo...).x totalBallsBeingRemoved = $totalBallsBeingRemoved")
          //???? move?
          // note original game scoring: score = totalBallsBeingRemoved * 4 - 10,
          //  which seems to be from 2 pts per ball in 5-ball line, but 4 for any extra balls in line
          val postLinesRemovalBoard = removeCompletedLineBalls(ballTo,
                                                               boardPlus,
                                                               completedLineAxesResults)
          val ballPlacementScore = 2 * LineOrder + 4 * (totalBallsBeingRemoved - LineOrder)
          (postLinesRemovalBoard.withAddedScore(ballPlacementScore), Some(ballPlacementScore))
      }
    //println(s"-handleBallArrival(... ballTo = $ballTo...).9 = score result = $scoreResult")
    MoveResult(boardResult, scoreResult.isDefined, false)
  }

}