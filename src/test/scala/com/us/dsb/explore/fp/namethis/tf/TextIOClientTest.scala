//??? BEING REWORKED tagless-final(?) form:
package com.us.dsb.explore.fp.namethis.tf

import cats.Applicative
import com.us.dsb.explore.fp.namethis.io.TextIOClient.UICommand
import cats.effect.IO
import cats.syntax.applicative._
import org.scalatest.AppendedClues._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class TextIOClientTest extends AnyFunSpec {

  // Crude, manual stub and spy SegregatedTextIO.
  class SegregatedTextIODouble[F[_]: Applicative](inputLines: String*) extends SegregatedTextIO[F] {
    private var remainingInputs = inputLines
    private var printedStrings: List[String] = Nil
    // (no tracking of via which method wrote string)
    def getPrintedStrings: List[String] = printedStrings

    override def printStateText(lineOrLines: String): F[Unit] = {
      printedStrings ::= lineOrLines
      ().pure[F]
    }

    override def readPromptedLine(prompt: String): F[String] = {
      printedStrings ::= prompt
      remainingInputs match {
        case head +: tail =>
          remainingInputs = tail
          head.pure[F]
        case Nil =>
          ???
      }
    }

    override def printError(fullLine: String): F[Unit] = {
      printedStrings ::= fullLine
      ().pure[F]
    }

    override def printResult(lineOrLines: String): F[Unit] = {
      printedStrings ::= lineOrLines
      ().pure[F]
    }

  }


  describe("NT.getCommand:") {
    import org.scalatest.PrivateMethodTester._
    val getCommand = PrivateMethod[UICommand](Symbol("getCommand"))

    describe("NT.for some valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble[IO]("u")
        val call = TextIOClient.getCommand(ioDouble, "<dummy X>")
        val callResult = call.unsafeRunSync()
      }
      import LazyShared._

      it("NT.should return decoded command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
      it("NT.should emit only prompt line (once)") {
        ioDouble.getPrintedStrings should have length 1
      }
    }

    describe("NT.for invalid command(s) and then valid command:") {
      object LazyShared {
        val ioDouble = new SegregatedTextIODouble[IO]("?", "u")
        val call = TextIOClient.getCommand(ioDouble, "<dummy X>")
        val callResult = call.unsafeRunSync()
      }
      import LazyShared._

      it("NT.should emit prompt, error, and second prompt line") {
        ioDouble.getPrintedStrings shouldBe
            List("Player <dummy X> command?: ",
                 "Invalid input \"?\"; try u(p), d(own), l(eft), r(right), m(ark), or q(uit)",
                 "Player <dummy X> command?: ")
        // Theoretically, check specifics.
      }
      it("NT.should return decoded eventual valid command") {
        // Note: Can't break line line before withClue; "... . withClue(...)" and
        //   '... \n .withClue(...)" don't attach clue; other options unclear.
        //
        callResult shouldBe UICommand.Up withClue
            s"(printed strings: ${ioDouble.getPrintedStrings}"
      }
    }

  }

}
