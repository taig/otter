// package io.taig.otter

// import cats.Functor
// import cats.syntax.all.*

// sealed trait Branch[+F[+_], +A, B] extends Branch.Reader[F, A, B], Branch.Writer[F, A, B]:
//   override def schema: F[Schema[F, ?, ?]]
//   override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch[G, ?, B]

// object Branch:
//   sealed trait Reader[+F[+_], +A, +B]:
//     def name: String
//     def schema: F[Schema.Reader[F, ?, ?]]
//     def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Reader[G, ?, B]

//   object Reader:
//     final case class Root[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](name: String, schema: A)
//         extends Branch.Reader[F, A, B]:
//       override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Reader[G, ?, B] =
//         copy(schema = fK(schema).map(_.translate(fK)))

//   sealed trait Writer[+F[+_], +A, -B]:
//     def name: String
//     def schema: F[Schema.Writer[F, ?, ?]]
//     def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Writer[G, ?, B]

//   object Writer:
//     final case class Root[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](name: String, schema: A)
//         extends Branch.Writer[F, A, B]:
//       override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch.Writer[G, ?, B] =
//         copy(schema = fK(schema).map(_.translate(fK)))

//   final case class Root[F[+_], +A <: F[Schema[F, ?, B]], B](name: String, schema: A) extends Branch[F, A, B]:
//     override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Branch[G, ?, B] =
//       copy(schema = fK(schema).map(_.translate(fK)))
