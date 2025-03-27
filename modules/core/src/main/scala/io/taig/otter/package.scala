// package io.taig.otter

// import cats.Eq
// import cats.syntax.all.*

// import scala.collection.immutable.Iterable

// extension [A: Eq, B](self: Vector[(A, B)])
//   def filterKeys(keys: Iterable[A]): (Vector[(A, B)], Vector[(A, B)]) =
//     val remainingKeys = keys.toBuffer
//     val result = Vector.newBuilder[(A, B)]
//     val remainders = Vector.newBuilder[(A, B)]

//     self.foreach { case value @ (key, _) =>
//       if remainingKeys.exists(_ === key)
//       then
//         remainingKeys -= key
//         result += value
//       else remainders += value
//     }

//     (result.result(), remainders.result())

// extension [A](self: Vector[A])
//   def collectFirstWithRemainders[B](pf: PartialFunction[A, B]): (Vector[A], Option[B]) =
//     @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
//     var result: Option[B] = none
//     val remainders = Vector.newBuilder[A]

//     self.foreach: a =>
//       if result.isEmpty && pf.isDefinedAt(a)
//       then result = pf.apply(a).some
//       else remainders += a

//     if result.isEmpty
//     then (self, none)
//     else (remainders.result(), result)

// @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
// private[otter] def dataAsValue[A <: Data.Value](data: Data.Nullable[A]): Option[A] = data match
//   case Data.Null => None
//   case data      => Some(data.asInstanceOf[A])
