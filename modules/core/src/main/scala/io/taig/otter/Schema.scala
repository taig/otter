package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.Schema.Reader

sealed trait Schema[M, +N <: M, +A <: Schema[M, ?, ?, ?], B]
    extends Schema.Reader[M, N, A, B],
      Schema.Writer[M, N, A, B]:
  // def ivalidate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C])(f: C => B): Schema[M, A, C]
  def update[O <: M](f: N => O): Schema[M, O, A, B]
  def translate[O](f: M => O): Schema[O, O, ?, B]
  override def optional: Schema[M, N, A, Option[B]]

object Schema:
  sealed trait Reader[M, +N <: M, +A, +B] extends Product, Serializable:
    def constraints: Chain[Constraint[?]]
    def optional: Schema.Reader[M, N, A, Option[B]]
    // def validate[N >: M, V1, V2, C](validation: SchemaValidation[N, B, V1, V2, C]): Schema.Reader[N, A, C]

  sealed trait Writer[M, +N <: M, +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[M, N, A, C]
    def optional: Schema.Writer[M, N, A, Option[B]]

sealed trait Collection[M, +N <: M, +A <: Schema[M, ?, ?, ?], B]
    extends Schema[M, N, A, B],
      Collection.Reader[M, N, A, B],
      Collection.Writer[M, N, A, B]:
  override def update[O <: M](f: N => O): Collection[M, O, A, B]
//   // override def ivalidate[N >: M, V1, V2, C](validation: SchemaValidation[N, B, V1, V2, C])(f: C => B): Collection[N, A, C] =
//   //   Collection.Modify(this, validation, f)
  final override def optional: Collection[M, N, A, Option[B]] = ??? // Collection.Optional(this)
//   // override def update[N](f: M => N): Collection[N, A, B]
  override def schema: Schema[M, ?, ?, ?]

object Collection:
  sealed trait Reader[M, +N <: M, +A, +B] extends Schema.Reader[M, N, A, B]:
    def constraints: Chain[Constraint[?]]
//     def optional: Collection.Reader[M, A, Option[B]] = Reader.Optional(this)
//     def schema: Schema.Reader[M, ?, ?]
//     final def validate[N >: M, V1, V2, C](validation: SchemaValidation[N, B, V1, V2, C]): Collection.Reader[N, A, C] =
//       Reader.Modify(this, validation)

//   object Reader:
//     final case class Modify[M, A, B, V1, V2, C](
//         self: Collection.Reader[M, A, B],
//         validation: SchemaValidation[M, B, V1, V2, C]
//     ) extends Collection.Reader[M, A, C]:
//       export self.schema
//       override def constraints: Chain[Constraint[?]] = validation.constraints

//     final case class Optional[M, A, B](self: Collection.Reader[M, A, B]) extends Collection.Reader[M, A, Option[B]]:
//       export self.{constraints, schema}

//     final case class Root[M, +A <: Schema.Reader[M, ?, B], B](schema: A) extends Collection.Reader[M, A, Vector[B]]:
//       override def constraints: Chain[Constraint[?]] = Chain.empty

  sealed trait Writer[M, +N <: M, +A, -B] extends Schema.Writer[M, N, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[M, N, A, C] = ??? // Writer.Modify(this, f)
//     def optional: Collection.Writer[M, A, Option[B]] = Writer.Optional(this)
    def schema: Schema.Writer[M, ?, ?, ?]

//   object Writer:
//     final case class Modify[M, A, B, C](self: Collection.Writer[M, A, B], f: C => B) extends Collection.Writer[M, A, C]:
//       export self.schema

//     final case class Optional[M, A, B](self: Collection.Writer[M, A, B]) extends Collection.Writer[M, A, Option[B]]:
//       export self.schema

//     final case class Root[M, +A <: Schema.Writer[M, ?, B], B](schema: A) extends Collection.Writer[M, A, Vector[B]]

//   final case class Modify[M, A, B, V1, V2, C](
//       self: Collection[M, A, B],
//       validation: SchemaValidation[M, B, V1, V2, C],
//       f: C => B
//   ) extends Collection[M, A, C]:
//     export self.schema
//     override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints
//     // override def update[N](f: M => N): Collection[N, A, C] = ???

//   final case class Optional[M, A, B](self: Collection[M, A, B]) extends Collection[M, A, Option[B]]:
//     export self.{constraints, schema}
//     // override def update[N](f: M => N): Collection[N, A, Option[B]] = ???

  final case class Root[M, N <: M, +A <: Schema[M, ?, ?, B], B](metadata: N, schema: A)
      extends Collection[M, N, A, Vector[B]]:
    override def update[O <: M](f: N => O): Collection[M, O, A, Vector[B]] =
      copy(metadata = f(metadata), schema)
    override def translate[O](f: M => O): Collection[O, O, ?, Vector[B]] =
      copy(metadata = f(metadata), schema = schema.translate(f))
    override def constraints: Chain[Constraint[?]] = Chain.empty
    // override def update[N](f: M => N): Collection[N, A, Vector[B]] = ???

// sealed trait Enumeration[+M, +A <: Schema[?, ?, ?], B] extends Schema[M, A, B]

// sealed trait Primitive[+M, A] extends Schema[M, Nothing, A], Primitive.Reader[M, A], Primitive.Writer[M, A]:
//   final override def optional: Primitive[M, Option[A]] = Primitive.Optional(this)
//   // def ivalidate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): Primitive[M, B] =
//   //   Primitive.Modify(this, validation, f)

// object Primitive:
//   sealed trait Required[+M, A] extends Primitive[M, A], Primitive.Required.Reader[M, A], Primitive.Required.Writer[M, A]
//   // override def ivalidate[N >: M, V1, V2, C](validation: SchemaValidation[N, A, V1, V2, C])(f: C => A): Primitive.Required[N, C] = ???

//   object Required:
//     sealed trait Reader[+M, +A] extends Primitive.Reader[M, A]
//     // override def validate[N >: M, V1, V2, C](validation: SchemaValidation[N, A, V1, V2, C]): Primitive.Required.Reader[N, C] = ???

//     object Reader:
//       final case class Modify[M, A, V1, V2, B](
//           self: Primitive.Required.Reader[M, A],
//           validation: SchemaValidation[M, A, V1, V2, B]
//       ) extends Primitive.Required.Reader[M, B]:
//         export self.tpe
//         override def constraints: Chain[Constraint[?]] = validation.constraints

//     sealed trait Writer[+M, -A] extends Primitive.Writer[M, A]:
//       final override def contramap[C](f: C => A): Primitive.Required.Writer[M, C] = Writer.Modify(this, f)

//     object Writer:
//       final case class Modify[M, A, B](self: Primitive.Required.Writer[M, A], f: B => A)
//           extends Primitive.Required.Writer[M, B]:
//         export self.tpe

//     final case class Modify[M, A, V1, V2, B](
//         self: Primitive.Required[M, A],
//         validation: SchemaValidation[M, A, V1, V2, B],
//         f: B => A
//     ) extends Primitive.Required[M, B]:
//       export self.tpe
//       override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

//     final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]:
//       override def constraints: Chain[Constraint[?]] = Chain.empty

//   sealed trait Reader[+M, +A] extends Schema.Reader[M, Nothing, A]:
//     def constraints: Chain[Constraint[?]]
//     def optional: Primitive.Reader[M, Option[A]] = Reader.Optional(this)
//     def tpe: Type[?]
//     // def validate[V1, V2, C](validation: SchemaValidation[M, A, V1, V2, C]): Primitive.Reader[M, C] =
//     //   Reader.Modify(this, validation)

//   object Reader:
//     final case class Modify[M, A, V1, V2, B](
//         self: Primitive.Reader[M, A],
//         validation: SchemaValidation[M, A, V1, V2, B]
//     ) extends Primitive.Reader[M, B]:
//       export self.tpe
//       override def constraints: Chain[Constraint[?]] = validation.constraints

//     final case class Optional[M, A](self: Primitive.Reader[M, A]) extends Primitive.Reader[M, Option[A]]:
//       export self.{constraints, tpe}

//   sealed trait Writer[+M, -A] extends Schema.Writer[M, Nothing, A]:
//     def contramap[C](f: C => A): Primitive.Writer[M, C] = Writer.Modify(this, f)
//     def optional: Primitive.Writer[M, Option[A]] = Writer.Optional(this)
//     def tpe: Type[?]

//   object Writer:
//     final case class Modify[M, A, B](self: Primitive.Writer[M, A], f: B => A) extends Primitive.Writer[M, B]:
//       export self.tpe

//     final case class Optional[M, A](self: Primitive.Writer[M, A]) extends Primitive.Writer[M, Option[A]]:
//       export self.tpe

//   final case class Modify[M, A, V1, V2, B](
//       self: Primitive[M, A],
//       validation: SchemaValidation[M, A, V1, V2, B],
//       f: B => A
//   ) extends Primitive[M, B]:
//     export self.tpe
//     override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

//   final case class Optional[M, A](self: Primitive[M, A]) extends Primitive[M, Option[A]]:
//     export self.{constraints, tpe}

// sealed trait Tuple[+M, +A, B] extends Schema[M, A, B], Tuple.Reader[M, A, B], Tuple.Writer[M, A, B]:
//   // final def ivalidate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C])(f: C => B): Tuple[M, A, C] =
//   //   Tuple.Modify(this, validation, f)
//   final override def optional: Tuple[M, A, Option[B]] = Tuple.Optional(this)
//   override def schemas: Chain[Schema[M, ?, ?]]

// object Tuple:
//   sealed trait Reader[+M, +A, +B] extends Schema.Reader[M, A, B]:
//     def schemas: Chain[Schema.Reader[M, ?, ?]]
//     def constraints: Chain[Constraint[?]]
//     def optional: Tuple.Reader[M, A, Option[B]] = Reader.Optional(this)
//     // final def validate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C]): Tuple.Reader[M, A, C] =
//     //   Reader.Modify(this, validation)

//   object Reader:
//     final case class Empty[M]() extends Tuple.Reader[M, Nothing, Unit]:
//       override def constraints: Chain[Constraint[?]] = Chain.empty
//       override def schemas: Chain[Schema[M, ?, ?]] = Chain.empty

//     final case class Modify[M, A, B, C, V1, V2](
//         self: Tuple.Reader[M, A, B],
//         validation: SchemaValidation[M, B, V1, V2, C]
//     ) extends Tuple.Reader[M, A, C]:
//       export self.schemas
//       override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

//     final case class One[M, +A <: Schema.Reader[M, ?, B], B](schema: A) extends Tuple.Reader[M, A, B]:
//       override def constraints: Chain[Constraint[?]] = Chain.empty
//       override def schemas: Chain[A] = Chain.one(schema)

//     final case class Optional[M, A, B](self: Tuple.Reader[M, A, B]) extends Tuple.Reader[M, A, Option[B]]:
//       export self.{constraints, schemas}

//     final case class Zip[M, A, B, +C <: Schema.Reader[M, ?, D], D](
//         left: Tuple.Reader[M, A, B],
//         right: C
//     ) extends Tuple.Reader[M, A | C, (B, D)]:
//       override def constraints: Chain[Constraint[?]] = left.constraints
//       override def schemas: Chain[Schema.Reader[M, ?, ?]] = left.schemas :+ right

//   sealed trait Writer[+M, +A, -B] extends Schema.Writer[M, A, B]:
//     final def contramap[C](f: C => B): Tuple.Writer[M, A, C] = Writer.Modify(this, f)
//     def optional: Tuple.Writer[M, A, Option[B]] = Writer.Optional(this)
//     def schemas: Chain[Schema.Writer[M, ?, ?]]

//   object Writer:
//     final case class Empty[M](metadata: M) extends Tuple.Writer[M, Nothing, Unit]:
//       override def schemas: Chain[Schema[M, ?, ?]] = Chain.empty

//     final case class Modify[M, A, B, C](self: Tuple.Writer[M, A, B], f: C => B) extends Tuple.Writer[M, A, C]:
//       export self.schemas

//     final case class One[M, +A <: Schema.Writer[M, ?, B], B](metadata: M, schema: A) extends Tuple.Writer[M, A, B]:
//       override def schemas: Chain[A] = Chain.one(schema)

//     final case class Optional[M, A, B](self: Tuple.Writer[M, A, B]) extends Tuple.Writer[M, A, Option[B]]:
//       export self.schemas

//     final case class Zip[M, A, B, +C <: Schema.Writer[M, ?, D], D](
//         left: Tuple.Writer[M, A, B],
//         right: C
//     ) extends Tuple.Writer[M, A | C, (B, D)]:
//       override def schemas: Chain[Schema.Writer[M, ?, ?]] = left.schemas :+ right

//   final case class Empty[M](metadata: M) extends Tuple[M, Nothing, Unit]:
//     override def constraints: Chain[Constraint[?]] = Chain.empty
//     override def schemas: Chain[Schema[M, ?, ?]] = Chain.empty

//   final case class Modify[M, A, B, V1, V2, C](
//       self: Tuple[M, A, B],
//       validation: SchemaValidation[M, B, V1, V2, C],
//       f: C => B
//   ) extends Tuple[M, A, C]:
//     export self.schemas
//     override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

//   final case class One[M, +A <: Schema[M, ?, B], B](metadata: M, schema: A) extends Tuple[M, A, B]:
//     override def constraints: Chain[Constraint[?]] = Chain.empty
//     override def schemas: Chain[A] = Chain.one(schema)

//   final case class Optional[M, A, B](self: Tuple[M, A, B]) extends Tuple[M, A, Option[B]]:
//     export self.{constraints, schemas}

//   final case class Zip[M, A, B, +C <: Schema[M, ?, D], D](left: Tuple[M, A, B], right: C)
//       extends Tuple[M, A | C, (B, D)]:
//     override def constraints: Chain[Constraint[?]] = left.constraints
//     override def schemas: Chain[Schema[M, ?, ?]] = left.schemas :+ right
