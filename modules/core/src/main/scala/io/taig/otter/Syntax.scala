// package io.taig.otter

// import cats.syntax.all.*
// import cats.Invariant
// import scala.compiletime.*
// import scala.annotation.targetName

// trait Syntax extends Comparison.Syntax:
//   extension [F[_], A](self: F[A])(using Invariant[F])
//     // Breaks type inference (https://github.com/typelevel/twiddles/issues/19)
//     // final def to[B](using convert: Convert[A, B]): Self[B] = imap(convert.to)(convert.from)
//     final inline def to[B]: F[B] =
//       val convert = summonInline[Convert[A, B]]
//       self.imap(convert.to)(convert.from)

//   extension [F[_]](self: F[Unit])(using Invariant[F])
//     final def const[A](a: A): F[A] = self.imap(_ => a)(_ => ())

//     @targetName("constSingleton")
//     final def const[A <: Singleton](a: A): F[A] = self.imap(_ => a)(_ => ())

//   extension [F[_], A, B](self: F[(A, B)])(using Invariant[F])
//     final def merge(using merge: Merge[A, B]): F[merge.Out] =
//       self.imap(merge.apply)(merge.unapply)

// object Syntax extends Syntax
