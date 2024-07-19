// package io.taig.otter.http

// import cats.data.Chain

// sealed trait Queries[A]:
//   final def imap[B](f: A => B)(g: B => A): Queries[B] = Queries.Transform(this, f, g)
//   def parameters: Chain[Parameter[?]]
//   final def zip[B](queries: Queries[B]): Queries[(A, B)] = Queries.Combine(this, queries)

// object Queries:
//   final case class Combine[A, B](left: Queries[A], right: Queries[B]) extends Queries[(A, B)]:
//     override def parameters: Chain[Parameter[?]] = left.parameters ++ right.parameters

//   case object Empty extends Queries[Unit]:
//     override def parameters: Chain[Nothing] = Chain.empty

//   final case class One[A](parameter: Parameter[A]) extends Queries[A]:
//     override def parameters: Chain[Parameter[A]] = Chain.one(parameter)

//   final case class Transform[A, B](self: Queries[A], f: A => B, g: B => A) extends Queries[B]:
//     export self.parameters
