package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.Schema.Reader

sealed trait Schema[M, +N <: M, +A <: Schema[M, ?, ?, ?], B]
    extends Schema.Reader[M, N, A, B],
      Schema.Writer[M, N, A, B]:
  def ivalidate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C])(f: C => B): Schema[M, N, A, C]
  override def modify[O <: M](f: N => O): Schema[M, O, A, B]
  override def optional: Schema[M, N, A, Option[B]]
  override def translate[O](f: M => O): Schema[O, O, ?, B]

object Schema:
  sealed trait Reader[M, +N <: M, +A <: Schema.Reader[M, ?, ?, ?], +B] extends Product, Serializable:
    def constraints: Chain[Constraint[?]]
    def modify[O <: M](f: N => O): Schema.Reader[M, O, A, B]
    def optional: Schema.Reader[M, N, A, Option[B]]
    def translate[O](f: M => O): Schema.Reader[O, O, ?, B]
    def validate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C]): Schema.Reader[M, N, A, C]

  sealed trait Writer[M, +N <: M, +A <: Schema.Writer[M, ?, ?, ?], -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[M, N, A, C]
    def modify[O <: M](f: N => O): Schema.Writer[M, O, A, B]
    def optional: Schema.Writer[M, N, A, Option[B]]
    def translate[O](f: M => O): Schema.Writer[O, O, ?, B]

sealed trait Collection[M, +N <: M, +A <: Schema[M, ?, ?, ?], B]
    extends Schema[M, N, A, B],
      Collection.Reader[M, N, A, B],
      Collection.Writer[M, N, A, B]:
  override def ivalidate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C])(f: C => B): Collection[M, N, A, C] =
    Collection.Invariant(this, validation, f)
  override def modify[O <: M](f: N => O): Collection[M, O, A, B]
  final override def optional: Collection[M, N, A, Option[B]] = Collection.Optional(this)
  override def schema: Schema[M, ?, ?, ?]
  override def translate[O](f: M => O): Collection[O, O, ?, B]

object Collection:
  sealed trait Reader[M, +N <: M, +A <: Schema.Reader[M, ?, ?, ?], +B] extends Schema.Reader[M, N, A, B]:
    def constraints: Chain[Constraint[?]]
    override def modify[O <: M](f: N => O): Collection.Reader[M, O, A, B]
    override def optional: Collection.Reader[M, N, A, Option[B]] = Collection.Reader.Optional(this)
    def schema: Schema.Reader[M, ?, ?, ?]
    override def translate[O](f: M => O): Collection.Reader[O, O, ?, B]
    final override def validate[V1, V2, C](
        validation: SchemaValidation[M, B, V1, V2, C]
    ): Collection.Reader[M, N, A, C] = Reader.Functor(this, validation)

  object Reader:
    final case class Functor[M, N <: M, A <: Schema.Reader[M, ?, ?, ?], B, V1, V2, C](
        self: Collection.Reader[M, N, A, B],
        validation: SchemaValidation[M, B, V1, V2, C]
    ) extends Collection.Reader[M, N, A, C]:
      export self.schema
      override def constraints: Chain[Constraint[?]] = validation.constraints
      override def modify[O <: M](f: N => O): Collection.Reader[M, O, A, C] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Collection.Reader[O, O, ?, C] = copy(
        self = self.translate(f),
        validation = validation.mapConstraint(_.leftMap(_.translate(f))).mapActual(_.leftMap(_.translate(f)))
      )

    final case class Optional[M, N <: M, A <: Schema.Reader[M, ?, ?, ?], B](self: Collection.Reader[M, N, A, B])
        extends Collection.Reader[M, N, A, Option[B]]:
      export self.{constraints, schema}
      override def modify[O <: M](f: N => O): Collection.Reader[M, O, A, Option[B]] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Collection.Reader[O, O, ?, Option[B]] = copy(self = self.translate(f))

    final case class Root[M, N <: M, A <: Schema.Reader[M, ?, ?, B], B](metadata: N, schema: A)
        extends Collection.Reader[M, N, A, Vector[B]]:
      override def constraints: Chain[Constraint[?]] = Chain.empty
      override def modify[O <: M](f: N => O): Collection.Reader[M, O, A, Vector[B]] = copy(metadata = f(metadata))
      override def translate[O](f: M => O): Collection.Reader[O, O, ?, Vector[B]] =
        copy(metadata = f(metadata), schema = schema.translate(f))

  sealed trait Writer[M, +N <: M, +A <: Schema.Writer[M, ?, ?, ?], -B] extends Schema.Writer[M, N, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[M, N, A, C] = Writer.Contravariant(this, f)
    override def modify[O <: M](f: N => O): Collection.Writer[M, O, A, B]
    def optional: Collection.Writer[M, N, A, Option[B]] = Writer.Optional(this)
    def schema: Schema.Writer[M, ?, ?, ?]
    override def translate[O](f: M => O): Collection.Writer[O, O, ?, B]

  object Writer:
    final case class Contravariant[M, N <: M, A <: Schema.Writer[M, ?, ?, ?], B, C](
        self: Collection.Writer[M, N, A, B],
        f: C => B
    ) extends Collection.Writer[M, N, A, C]:
      export self.schema
      override def modify[O <: M](f: N => O): Collection.Writer[M, O, A, C] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Collection.Writer[O, O, ?, C] = copy(self = self.translate(f))

    final case class Optional[M, N <: M, A <: Schema.Writer[M, ?, ?, ?], B](self: Collection.Writer[M, N, A, B])
        extends Collection.Writer[M, N, A, Option[B]]:
      export self.schema
      override def modify[O <: M](f: N => O): Collection.Writer[M, O, A, Option[B]] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Collection.Writer[O, O, ?, Option[B]] = copy(self = self.translate(f))

    final case class Root[M, N <: M, A <: Schema.Writer[M, ?, ?, B], B](metadata: N, schema: A)
        extends Collection.Writer[M, N, A, Vector[B]]:
      override def modify[O <: M](f: N => O): Collection.Writer[M, O, A, Vector[B]] =
        copy(metadata = f(metadata))
      override def translate[O](f: M => O): Collection.Writer[O, O, ?, Vector[B]] =
        copy(metadata = f(metadata), schema = schema.translate(f))

  final case class Invariant[M, N <: M, A <: Schema[M, ?, ?, ?], B, V1, V2, C](
      self: Collection[M, N, A, B],
      validation: SchemaValidation[M, B, V1, V2, C],
      f: C => B
  ) extends Collection[M, N, A, C]:
    export self.schema
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints
    override def modify[O <: M](f: N => O): Collection[M, O, A, C] = copy(self = self.modify(f))
    override def translate[O](f: M => O): Collection[O, O, ?, C] = copy(
      self = self.translate(f),
      validation = validation.mapConstraint(_.leftMap(_.translate(f))).mapActual(_.leftMap(_.translate(f)))
    )

  final case class Optional[M, N <: M, A <: Schema[M, ?, ?, ?], B](self: Collection[M, N, A, B])
      extends Collection[M, N, A, Option[B]]:
    export self.{constraints, schema}
    override def modify[O <: M](f: N => O): Collection[M, O, A, Option[B]] = copy(self = self.modify(f))
    override def translate[O](f: M => O): Collection[O, O, ?, Option[B]] = copy(self = self.translate(f))

  final case class Root[M, N <: M, A <: Schema[M, ?, ?, B], B](metadata: N, schema: A)
      extends Collection[M, N, A, Vector[B]]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
    override def modify[O <: M](f: N => O): Collection[M, O, A, Vector[B]] = copy(metadata = f(metadata))
    override def translate[O](f: M => O): Collection[O, O, ?, Vector[B]] =
      copy(metadata = f(metadata), schema = schema.translate(f))

// sealed trait Enumeration[+M, +A <: Schema[?, ?, ?], B] extends Schema[M, A, B]

sealed trait Primitive[M, +N <: M, A]
    extends Schema[M, N, Nothing, A],
      Primitive.Reader[M, N, A],
      Primitive.Writer[M, N, A]:
  override def ivalidate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): Primitive[M, N, B] =
    Primitive.Invariant(this, validation, f)
  override def modify[O <: M](f: N => O): Primitive[M, O, A]
  final override def optional: Primitive[M, N, Option[A]] = Primitive.Optional(this)
  override def translate[O](f: M => O): Primitive[O, O, A]

object Primitive:
  sealed trait Required[M, +N <: M, A]
      extends Primitive[M, N, A],
        Primitive.Required.Reader[M, N, A],
        Primitive.Required.Writer[M, N, A]:
    final override def ivalidate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B])(
        f: B => A
    ): Primitive.Required[M, N, B] = Required.Invariant(this, validation, f)
    override def modify[O <: M](f: N => O): Primitive.Required[M, O, A]
    override def translate[O](f: M => O): Primitive.Required[O, O, A]

  object Required:
    sealed trait Reader[M, +N <: M, +A] extends Primitive.Reader[M, N, A]:
      override def modify[O <: M](f: N => O): Primitive.Required.Reader[M, O, A]
      override def translate[O](f: M => O): Primitive.Required.Reader[O, O, A]
      final override def validate[V1, V2, B](
          validation: SchemaValidation[M, A, V1, V2, B]
      ): Primitive.Required.Reader[M, N, B] = Reader.Functor(this, validation)

    object Reader:
      final case class Functor[M, N <: M, A, V1, V2, B](
          self: Primitive.Required.Reader[M, N, A],
          validation: SchemaValidation[M, A, V1, V2, B]
      ) extends Primitive.Required.Reader[M, N, B]:
        export self.tpe
        override def constraints: Chain[Constraint[?]] = validation.constraints
        override def modify[O <: M](f: N => O): Primitive.Required.Reader[M, O, B] = copy(self = self.modify(f))
        override def translate[O](f: M => O): Primitive.Required.Reader[O, O, B] = copy(
          self = self.translate(f),
          validation = validation.mapConstraint(_.leftMap(_.translate(f))).mapActual(_.leftMap(_.translate(f)))
        )

    sealed trait Writer[M, +N <: M, -A] extends Primitive.Writer[M, N, A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[M, N, B] = Writer.Contravariant(this, f)
      override def modify[O <: M](f: N => O): Primitive.Required.Writer[M, O, A]
      override def translate[O](f: M => O): Primitive.Required.Writer[O, O, A]

    object Writer:
      final case class Contravariant[M, N <: M, A, B](self: Primitive.Required.Writer[M, N, A], f: B => A)
          extends Primitive.Required.Writer[M, N, B]:
        export self.tpe
        override def modify[O <: M](f: N => O): Primitive.Required.Writer[M, O, B] = copy(self = self.modify(f))
        override def translate[O](f: M => O): Primitive.Required.Writer[O, O, B] = copy(self = self.translate(f))

    final case class Invariant[M, N <: M, A, V1, V2, B](
        self: Primitive.Required[M, N, A],
        validation: SchemaValidation[M, A, V1, V2, B],
        f: B => A
    ) extends Primitive.Required[M, N, B]:
      export self.tpe
      override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints
      override def modify[O <: M](f: N => O): Primitive.Required[M, O, B] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Primitive.Required[O, O, B] = copy(
        self = self.translate(f),
        validation = validation.mapConstraint(_.leftMap(_.translate(f))).mapActual(_.leftMap(_.translate(f)))
      )

    final case class Root[M, N <: M, A](metadata: N, tpe: Type[A]) extends Primitive.Required[M, N, A]:
      override def constraints: Chain[Constraint[?]] = Chain.empty
      override def modify[O <: M](f: N => O): Primitive.Required[M, O, A] = copy(metadata = f(metadata))
      override def translate[O](f: M => O): Primitive.Required[O, O, A] = copy(metadata = f(metadata))

  sealed trait Reader[M, +N <: M, +A] extends Schema.Reader[M, N, Nothing, A]:
    override def modify[O <: M](f: N => O): Primitive.Reader[M, O, A]
    override def optional: Primitive.Reader[M, N, Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    override def translate[O](f: M => O): Primitive.Reader[O, O, A]
    override def validate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B]): Primitive.Reader[M, N, B] =
      Reader.Functor(this, validation)

  object Reader:
    final case class Functor[M, N <: M, A, V1, V2, B](
        self: Primitive.Reader[M, N, A],
        validation: SchemaValidation[M, A, V1, V2, B]
    ) extends Primitive.Reader[M, N, B]:
      export self.tpe
      override def constraints: Chain[Constraint[?]] = validation.constraints
      override def modify[O <: M](f: N => O): Primitive.Reader[M, O, B] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Primitive.Reader[O, O, B] = ???

    final case class Optional[M, N <: M, A](self: Primitive.Reader[M, N, A]) extends Primitive.Reader[M, N, Option[A]]:
      export self.{constraints, tpe}
      override def modify[O <: M](f: N => O): Primitive.Reader[M, O, Option[A]] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Primitive.Reader[O, O, Option[A]] = copy(self = self.translate(f))

  sealed trait Writer[M, +N <: M, -A] extends Schema.Writer[M, N, Nothing, A]:
    def contramap[C](f: C => A): Primitive.Writer[M, N, C] = Writer.Contravariant(this, f)
    override def modify[O <: M](f: N => O): Primitive.Writer[M, O, A]
    def optional: Primitive.Writer[M, N, Option[A]] = Writer.Optional(this)
    def tpe: Type[?]
    override def translate[O](f: M => O): Primitive.Writer[O, O, A]

  object Writer:
    final case class Contravariant[M, N <: M, A, B](self: Primitive.Writer[M, N, A], f: B => A)
        extends Primitive.Writer[M, N, B]:
      export self.tpe
      override def modify[O <: M](f: N => O): Primitive.Writer[M, O, B] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Primitive.Writer[O, O, B] = copy(self = self.translate(f))

    final case class Optional[M, N <: M, A](self: Primitive.Writer[M, N, A]) extends Primitive.Writer[M, N, Option[A]]:
      export self.tpe
      override def modify[O <: M](f: N => O): Primitive.Writer[M, O, Option[A]] = copy(self = self.modify(f))
      override def translate[O](f: M => O): Primitive.Writer[O, O, Option[A]] = copy(self = self.translate(f))

  final case class Invariant[M, N <: M, A, V1, V2, B](
      self: Primitive[M, N, A],
      validation: SchemaValidation[M, A, V1, V2, B],
      f: B => A
  ) extends Primitive[M, N, B]:
    export self.tpe
    override def modify[O <: M](f: N => O): Primitive[M, O, B] = copy(self = self.modify(f))
    override def translate[O](f: M => O): Primitive[O, O, B] = copy(self = self.translate(f), validation = ???)
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[M, N <: M, A](self: Primitive[M, N, A]) extends Primitive[M, N, Option[A]]:
    export self.{constraints, tpe}
    override def modify[O <: M](f: N => O): Primitive[M, O, Option[A]] = copy(self = self.modify(f))
    override def translate[O](f: M => O): Primitive[O, O, Option[A]] = copy(self = self.translate(f))

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
