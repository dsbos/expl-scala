package com.us.dsb.explore.algs.ttt.manual.ui

import com.us.dsb.explore.algs.ttt.manual.game.Player
import com.us.dsb.explore.algs.ttt.manual.ui.GameUI.UICommand
import org.scalatest.AppendedClues._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._


class GameUITest extends AnyFunSpec {


  // Crude, manual stub and spy SegregatedTextIO.
  class SegregatedTextIODouble(inputLines: String*) extends SegregatedTextIO {
    private var remainingInputs = inputLines
    private var printedStrings: List[String] = Nil
    // (no tracking of via which method)
    def getPrintedStrings: List[String] = printedStrings

    override def printStateText(lineOrLines: String): Unit = {
      printedStrings ::= lineOrLines
    }

    override def readPromptedLine(prompt: String): String = {
      printedStrings ::= prompt
      remainingInputs match {
        case head +: tail =>
          remainingInputs = tail
          head
        case Nil =>
          ???
      }
    }

    override def printError(fullLine: String): Unit = {
      printedStrings ::= fullLine
    }

    override def printResult(lineOrLines: String): Unit = {
      printedStrings ::= lineOrLines
    }

  }


  describe("getCommand:") {
    import org.scalatest.PrivateMethodTester._
    val getCommand = PrivateMethod[UICommand](Symbol("getCommand"))

    describe("for some valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble("u")
        val callResult = GameUI invokePrivate getCommand(ioDouble, Player.X)
      }
      import LazyShared._

      it("should return decoded command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
      it("should emit only prompt line (once)") {
        ioDouble.getPrintedStrings should have length 1
      }
    }

    describe("for invalid command(s) and then valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble("?", "u")
        val callResult = GameUI invokePrivate getCommand(ioDouble, Player.X)
      }
      import LazyShared._

      it("should emit error line and extra prompt line") {
        ioDouble.getPrintedStrings should have length 3
        // Theoretically, check specifics.
      }
      it("should return decoded eventual valid command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
    }

  }

  describe("runGame, just end to end (commands to game result):") {
    def runViaStrings(inputs: String*): String =
      GameUI.runGame(new SegregatedTextIODouble(inputs: _*)).text
    def runViaChars(inputChars: String): String =
      runViaStrings(inputChars.map("" + _): _*)


    describe("'q' should quit (result in message mentioning \"quit\"):") {
      it("'q' as first command") {
        runViaStrings("q") should include regex ("(?i)quit")
      }
    }
    describe("""X win should report X won; text should:""") {
      lazy val actual = runViaChars("mdmrmdmrm")  // X--/OX-/-OX
      it("""mention "win""") {
        actual should include regex "(?i)W[o]n"
      }
      it("""mention X""") {
        actual should include ("X")
      }
      it("""not mention O (probably)""" ) { // not if "X beat O"
        actual should not include ("O")
      }
      it("""be "Player X won" (currently) """) {
        actual should be ("Player X won")
      }
    }
    describe("""O win should report O won; text should:""") {
      lazy val actual = runViaChars("m rm dlm rm rm dlm")  // XO-/XOX/--O
      it("""mention win, O, and not X (probably)""") {
        actual should (
            include regex ("(?i)W[o]n")
            and include ("O")
            and not include ("X")
            )
      }
      it("""be "Player O won" (currently) """) {
        actual should be ("Player O won")
      }
    }
    describe("draw should report draw; text should:") {
      /*
         XXO/OOX/XXO:
        (1, 1) - m
        (2, 1) - dm
        (1, 2) - urm
        (2, 2) - dm
        (3, 1) - dlm
        (1, 3) - uurrm
        (2, 3) - dm
        (3, 3) - dm
        (3, 2) - lm
       */
      val cmds = "m" + "dm" + "urm" + "dm" + "dlm" + "uurrm" + "dm" + "dm" + "lm"
      lazy val actual = runViaChars(cmds)

      it("""mention "draw""") {
        actual should include regex "(?i)Draw"
      }
      it("""be "Game ended in draw" (currently)""") {
        actual should be ("Game ended in draw")
      }
    }
  }
  ignore("try runGame, layer test with ~mocked GameState?") {
  }
  ignore("no coverage: markAtSelection's error case") {
  }
}
