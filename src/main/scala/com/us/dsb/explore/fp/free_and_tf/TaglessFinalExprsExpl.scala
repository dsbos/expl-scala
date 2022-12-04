package com.us.dsb.explore.fp.free_and_tf

//import cats.{Applicative, Apply, FlatMap, Functor, Id, Monad}
import cats.{Applicative, FlatMap, Functor, Id, Monad}
import cats.effect.IO

// (https://speakerdeck.com/markus1189/functional-web-services-with-final-encoding)
object TaglessFinalExprsExpl extends App {

  // tagless-final algebra:
  sealed trait Expressions[F[_], A] {
    def literal(v: Int): F[A]
    def add(v1: F[A], v2: F[A]): F[A]
    def neg(v: F[A]): F[A]
  }

  // algebra client computation:
  def exprCalculation[F[_], A](implicit e: Expressions[F, A]): F[A] = {
    import e._
    add(literal(50),
        neg(add(literal(6),
                literal(2))))
  }

  // interpreter with Int data and Id effect, without Int instead of Id[Int]:
  val interpreterIntId1: Expressions[Id, Int] = {
    new Expressions[Id, Int] {
      override def literal(v: Int): Int = v
      override def add(v1: Int, v2: Int): Int = v1 + v2
      override def neg(v: Int): Int = -v
    }
  }
  // interpreter with Int data and Id effect, with Id[Int] instead of Int, direct:
  val interpreterIntId2: Expressions[Id, Int] = {
    new Expressions[Id, Int] {
      override def literal(v: Int): Id[Int] = v
      override def add(v1: Id[Int], v2: Id[Int]): Id[Int] = v1 + v2
      override def neg(v: Id[Int]): Id[Int] = -v
    }
  }
  // interpreter with Int data and IO effect, direct:
  val interpreterIntIo: Expressions[IO, Int] = {
    new Expressions[IO, Int] {
      def literal(v: Int): IO[Int] = IO.pure(v)
      def add(v1: IO[Int], v2: IO[Int]): IO[Int] =
        v1.flatMap(v1v => (v2.map(v2v => v1v + v2v)))
      def neg(v: IO[Int]): IO[Int] = v.map(-_)
    }
  }
  // interpreter with Int data and still-abstract effect type:
  def interpreterIntF[F[_]: Monad]: Expressions[F, Int] = {
    //import cats.implicits._
    new Expressions[F, Int] {
      override def literal(v: Int): F[Int] = {
        Applicative[F].pure(v)
        import cats.syntax.applicative._
        //???? "ambiguous implicit...": v.pure
        v.pure
        /* 'v.pure' yields:
        ambiguous implicit values:
          both value evidence$1 of type cats.Monad[F]
          and method catsStdInstancesForArraySeq in trait ArraySeqInstances of
            type cats.Traverse[scala.collection.immutable.ArraySeq]
            with cats.Monad[scala.collection.immutable.ArraySeq]
            with cats.Alternative[scala.collection.immutable.ArraySeq]
            with cats.CoflatMap[scala.collection.immutable.ArraySeq]
            with cats.Align[scala.collection.immutable.ArraySeq]
          match expected type cats.Applicative[F]:
         */
      }
      override def add(v1: F[Int], v2: F[Int]): F[Int] = {
        FlatMap[F].flatMap(v1)(v1v => Functor[F].map(v2)(v2v => v1v + v2v))
        import cats.syntax.flatMap._
        import cats.syntax.functor._
        v1.flatMap(v1v => (v2.map(v2v => v1v + v2v)))
      }
      override def neg(v: F[Int]): F[Int] = {
        Functor[F].map(v)(-_)
        Monad[F].map(v)(-_)
        import cats.syntax.functor._
        v.map(-_)
      }
    }
  }
  // interpreter with Int data and IO effect, via interpreterIntF:
  val interpreterIntIo2: Expressions[IO, Int] = interpreterIntF
  // interpreter with Int data and Id effect, via interpreterIntF:
  val interpreterIntId3: Expressions[Id, Int] = interpreterIntF

  val x1a1 = exprCalculation(interpreterIntId1)
  println("interpreterIntId: x1a1 = " + x1a1)
  val x1a2 = exprCalculation(interpreterIntId1)
  println("interpreterIntId: x1a2 = " + x1a2)
  val x1a3 = exprCalculation(interpreterIntId1)
  println("interpreterIntId: x1a3 = " + x1a3)
  val x1b = exprCalculation(interpreterIntIo)
  println("interpreterIntIo: x1b = " + x1b)
  println("interpreterIntIo: x1b.unsafeRunSync() = " + x1b.unsafeRunSync())
  val x1b2 = exprCalculation(interpreterIntIo2)
  println("interpreterIntIo2: x1b2 = " + x1b2)
  println("interpreterIntIo2: x1b2.unsafeRunSync() = " + x1b2.unsafeRunSync())

  {
    //val x3 = exprCalculation[Int]
    implicit val nameThis = interpreterIntId1
    val x4 = exprCalculation[Id, Int]
    println("x4 = " + x4)
  }

}
