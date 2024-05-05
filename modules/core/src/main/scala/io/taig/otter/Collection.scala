// package io.taig.otter

// import cats.data.Chain
// import io.taig.otter.Collection.Write

// sealed trait Collection[+Of, A]
//     extends Collection.Read[Of, A]
//     with Collection.Write[Of, A]
//     with Collection.Operation[Collection, Collection.Read, Collection.Write, Of, A]:
//   override def imap[B](f: A => B)(g: B => A): Collection[Of, B] = ???
//   override def optional: Collection[Of, Option[A]] = ???

// object Collection:
//   trait Operation[+S[+_, _], +RS[+of, a] >: S[of, a], +WS[+of, a] >: S[of, a], +Of, A]
//       extends Schema.Operation[S[Of, *], S[Of, *], RS[Of, *], RS[Of, *], WS[Of, *], WS[Of, *], A]
//       with Collection.Common[Of]

//   trait Common[+Of]:
//     def schema: Of

//   sealed trait Read[+Of, A] extends Collection.Read.Operation[Collection.Read, Of, A]:
//     override def map[B](f: A => B): Collection.Read[Of, B] = ???
//     override def optional: Collection.Read[Of, Option[A]] = ???

//   object Read:
//     trait Operation[+S[+_, _], +Of, A] extends Schema.Operation.Read[S[Of, *], S[Of, *], A] with Collection.Common[Of]

//     final case class Root[S[_], A](schema: S[A]) extends Collection.Read[S[A], Chain[A]]

//     def apply[S[_], A](schema: S[A]): Collection.Read[S[A], Chain[A]] = Root(schema)

//   sealed trait Write[+Of, A] extends Collection.Write.Operation[Collection.Write, Of, A]:
//     override def contramap[B](f: B => A): Collection.Write[Of, B] = ???
//     override def optional: Collection.Write[Of, Option[A]] = ???

//   object Write:
//     trait Operation[+S[+_, _], +Of, A] extends Schema.Operation.Write[S[Of, *], S[Of, *], A] with Collection.Common[Of]

//     final case class Root[S[_], A](schema: S[A]) extends Collection.Write[S[A], Chain[A]]

//     def apply[S[_], A](schema: S[A]): Collection.Write[S[A], Chain[A]] = Root(schema)

//   def apply[S[_], A](schema: S[A]): Collection[S[A], Chain[A]] =
//     val read = Read(schema)
//     val write = Write(schema)

//     new Collection[S[A], Chain[A]] {
//       override def asRead = read
//       override def asWrite = write
//       override def schema: S[A] = read.schema
//     }
