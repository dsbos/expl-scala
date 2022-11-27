//??? BEING REWORKED tagless-final(?) form:
package com.us.dsb.explore.fp.tttio.tf

import cats.effect.IO/**/

// Doing ConsoleIO as separate layer to have more simple layers for exploring
// testing, ZIO, etc.
private[fp] trait ConsoleIO[F[_]] {
  def println(lineOrLines: String): F[Unit]
  def readLine(prompt: String): F[String]
}
private[fp] object LiveConsoleIO extends ConsoleIO[IO] {

  override def println(lineOrLines: String): IO/**/[Unit] =
    IO/**/(Predef.println(lineOrLines))

  override def readLine(prompt: String): IO/**/[String] =
    IO/**/(scala.io.StdIn.readLine(prompt))
}
