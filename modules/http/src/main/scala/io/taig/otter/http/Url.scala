// package io.taig.otter.http

// sealed trait Url[A]:
//   final def imap[B](f: A => B)(g: B => A): Url[B] = Url.Transform(this, f, g)
//   def path: Path[?]
//   def queries: Queries[?]

// object Url:
//   final case class Combine[A, B](left: Url[A], right: Url[B]) extends Url[(A, B)]:
//     override def path: Path[?] = left.path.zip(right.path)
//     override def queries: Queries[?] = left.queries.zip(right.queries)

//   final case class Root[A, B](path: Path[A], queries: Queries[B]) extends Url[(A, B)]

//   final case class Transform[A, B](self: Url[A], f: A => B, g: B => A) extends Url[B]:
//     export self.{path, queries}
