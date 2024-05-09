// package io.taig.otter

// import cats.data.Chain
// import io.taig.otter.validation.Validation

// final case class Collection[+Of, A] private (reader: Collection.Reader[Of, A], writer: Collection.Writer[Of, A])
//     extends Schema[Of, A],
//       Collection.Reader[Of, A],
//       Collection.Writer[Of, A]:
//   export reader.schema
//   override def ivalidate[B](validation: Validation[A, B])(f: B => A): Collection[Of, B] =
//     Collection(reader.validate(validation), writer.contramap(f))
//   override def optional: Collection[Of, Option[A]] = Collection(reader.optional, writer.optional)

// object Collection:
//   trait Operation[+Of]:
//     def schema: Of

//   sealed trait Reader[+Of, +A] extends Schema.Reader[Of, A], Collection.Operation[Of]:
//     override def validate[B](validation: Validation[A, B]): Collection.Reader[Of, B] = Reader.Validate(this, validation)
//     override def optional: Collection.Reader[Of, Option[A]] = Reader.Optional(this)

//   object Reader:
//     final case class Optional[Of, A](self: Collection.Reader[Of, A]) extends Collection.Reader[Of, Option[A]]:
//       export self.schema
//     final case class Root[S[_], A](schema: S[A]) extends Collection.Reader[S[A], Chain[A]]
//     final case class Validate[Of, A, B](self: Collection.Reader[Of, A], validation: Validation[A, B])
//         extends Collection.Reader[Of, B]:
//       export self.schema

//     def apply[S[_], A](schema: S[A]): Collection.Reader[S[A], Chain[A]] = Root(schema)

//   sealed trait Writer[+Of, -A] extends Schema.Writer[Of, A], Collection.Operation[Of]:
//     final override def contramap[B](f: B => A): Collection.Writer[Of, B] = Writer.Modify(this, f)
//     override def optional: Collection.Writer[Of, Option[A]] = Writer.Optional(this)

//   object Writer:
//     final case class Modify[Of, A, B](self: Collection.Writer[Of, A], f: B => A) extends Collection.Writer[Of, B]:
//       export self.schema
//     final case class Optional[Of, A](self: Collection.Writer[Of, A]) extends Collection.Writer[Of, Option[A]]:
//       export self.schema
//     final case class Root[S[_], A](schema: S[A]) extends Collection.Writer[S[A], Chain[A]]

//     def apply[S[_], A](schema: S[A]): Collection.Writer[S[A], Chain[A]] = Root(schema)

//   def apply[S[_], A](schema: S[A]): Collection[S[A], Chain[A]] = Collection(Reader(schema), Writer(schema))
