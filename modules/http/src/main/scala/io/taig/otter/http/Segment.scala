// package io.taig.otter.http

// import io.taig.otter.Metadata
// import io.taig.otter.Value
// import cats.Invariant

// sealed trait Segment[A] extends Product, Serializable:
//   def name: String

// object Segment:
//   sealed trait Parameter[A] extends Segment[A]:
//     final def imap[B](f: A => B)(g: B => A): Segment.Parameter[B] = Parameter.Transform(this, f, g)
//     def schema: Value.Required[?, ?]

//   object Parameter:
//     final case class Root[A](metadata: Metadata, name: String, schema: Value.Required[?, A])
//         extends Segment.Parameter[A]

//     final case class Transform[A, B](self: Segment.Parameter[A], f: A => B, g: B => A) extends Segment.Parameter[B]:
//       export self.{name, schema}

//     given Invariant[Segment.Parameter] with
//       override def imap[A, B](fa: Segment.Parameter[A])(f: A => B)(g: B => A): Segment.Parameter[B] = fa.imap(f)(g)

//   final case class Static(name: String) extends Segment[Unit]
