package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.validation.Constraint

sealed trait Schema[+M, +A, B] extends Schema.Reader[M, A, B], Schema.Writer[M, A, B]:
  def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Schema[M, A, C]
  override def optional: Schema[M, A, Option[B]]
  def modify[N](f: M => N): Schema[N, A, B]

object Schema:
  sealed trait Reader[+M, +A, +B] extends Product, Serializable:
    def constraints: Chain[Constraint[?]]
    def modify[N](f: M => N): Schema.Reader[N, A, B]
    def optional: Schema.Reader[M, A, Option[B]]
    def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Schema.Reader[M, A, C]

  object Reader:
    given SchemaOps[Schema.Reader, Schema.Reader, Collection.Reader, Tuple.Reader] with
      extension [M, A, B](self: Schema.Reader[M, A, B])
        override def collection: Collection.Reader[M, self.type, Vector[B]] = Collection.Reader.Root(self)
        override def modify[N](f: M => N): Schema.Reader[N, A, B] = self.modify(f)
        override def optional: Schema.Reader[M, A, Option[B]] = self.optional
        override def toTuple: Tuple.Reader[M, self.type, B] = Tuple.Reader.One(self)

    given [M, A]: SchemaFunctor[Schema.Reader[M, A, *]] with
      extension [B](self: Schema.Reader[M, A, B])
        override def constraints: Chain[Constraint[?]] = self.constraints
        override def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Schema.Reader[M, A, C] =
          self.validate(validation)

  sealed trait Writer[+M, +A, -B] extends Product, Serializable:
    def contramap[C](f: C => B): Schema.Writer[M, A, C]
    def modify[N](f: M => N): Schema.Writer[N, A, B]
    def optional: Schema.Writer[M, A, Option[B]]

  object Writer:
    given SchemaOps[Schema.Writer, Schema.Writer, Collection.Writer, Tuple.Writer] with
      extension [M, A, B](self: Schema.Writer[M, A, B])
        override def collection: Collection.Writer[M, self.type, Vector[B]] = Collection.Writer.Root(self)
        override def modify[N](f: M => N): Schema.Writer[N, A, B] = self.modify(f)
        override def optional: Schema.Writer[M, A, Option[B]] = self.optional
        override def toTuple: Tuple.Writer[M, self.type, B] = Tuple.Writer.One(self)

    given [M, A]: SchemaContravariant[Schema.Writer[M, A, *]] with
      override def contramap[B, C](fa: Schema.Writer[M, A, B])(f: C => B): Schema.Writer[M, A, C] = fa.contramap(f)

  given SchemaOps[Schema, Schema, Collection, Tuple] with
    extension [M, A, B](self: Schema[M, A, B])
      override def collection: Collection[M, self.type, Vector[B]] = Collection.Root(self)
      override def modify[N](f: M => N): Schema[N, A, B] = self.modify(f)
      override def optional: Schema[M, A, Option[B]] = self.optional
      override def toTuple: Tuple[M, self.type, B] = Tuple.One(self)

  given [M, A]: SchemaInvariant[Schema[M, A, *]] with
    extension [B](self: Schema[M, A, B])
      override def constraints: Chain[Constraint[?]] = self.constraints
      override def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Schema[M, A, C] =
        self.ivalidate(validation)(f)

sealed trait Collection[+M, +A, B] extends Schema[M, A, B], Collection.Reader[M, A, B], Collection.Writer[M, A, B]:
  override def modify[N](f: M => N): Collection[N, A, B] = ???
  final override def optional: Collection[M, A, Option[B]] = Collection.Optional(this)
  final def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Collection[M, A, C] =
    Collection.Modify(this, validation, f)

object Collection:
  sealed trait Reader[+M, +A, +B] extends Schema.Reader[M, A, B]:
    def constraints: Chain[Constraint[?]]
    override def modify[N](f: M => N): Collection.Reader[N, A, B] = ???
    def optional: Collection.Reader[M, A, Option[B]] = Reader.Optional(this)
    final def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Collection.Reader[M, A, C] =
      Reader.Modify(this, validation)

  object Reader:
    final case class Modify[M, A, B, V1, V2, C](
        self: Collection.Reader[M, A, B],
        validation: SchemaValidation[B, V1, V2, C]
    ) extends Collection.Reader[M, A, C]:
      override def constraints: Chain[Constraint[?]] = validation.constraints

    final case class Optional[M, A, B](self: Collection.Reader[M, A, B]) extends Collection.Reader[M, A, Option[B]]:
      export self.constraints

    final case class Root[M, +A <: Schema.Reader[M, ?, B], B](schema: A) extends Collection.Reader[M, A, Vector[B]]:
      override def constraints: Chain[Constraint[?]] = Chain.empty

  sealed trait Writer[+M, +A, -B] extends Schema.Writer[M, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[M, A, C] = Writer.Modify(this, f)
    override def modify[N](f: M => N): Collection.Writer[N, A, B] = ???
    def optional: Collection.Writer[M, A, Option[B]] = Writer.Optional(this)

  object Writer:
    final case class Modify[M, A, B, C](self: Collection.Writer[M, A, B], f: C => B) extends Collection.Writer[M, A, C]

    final case class Optional[M, A, B](self: Collection.Writer[M, A, B]) extends Collection.Writer[M, A, Option[B]]

    final case class Root[M, +A <: Schema.Writer[M, ?, B], B](schema: A) extends Collection.Writer[M, A, Vector[B]]

  final case class Modify[M, A, B, V1, V2, C](
      self: Collection[M, A, B],
      validation: SchemaValidation[B, V1, V2, C],
      f: C => B
  ) extends Collection[M, A, C]:
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[M, A, B](self: Collection[M, A, B]) extends Collection[M, A, Option[B]]:
    export self.constraints

  final case class Root[M, +A <: Schema[M, ?, B], B](schema: A) extends Collection[M, A, Vector[B]]:
    override def constraints: Chain[Constraint[?]] = Chain.empty

sealed trait Primitive[+M, A] extends Schema[M, Nothing, A], Primitive.Reader[M, A], Primitive.Writer[M, A]:
  override def modify[N](f: M => N): Primitive[N, A] = ???
  final override def optional: Primitive[M, Option[A]] = Primitive.Optional(this)
  def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): Primitive[M, B] =
    Primitive.Modify(this, validation, f)

object Primitive:
  sealed trait Required[+M, A]
      extends Primitive[M, A],
        Primitive.Required.Reader[M, A],
        Primitive.Required.Writer[M, A]:
    final override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(
        f: B => A
    ): Primitive.Required[M, B] = Required.Modify(this, validation, f)

  object Required:
    sealed trait Reader[+M, +A] extends Primitive.Reader[M, A]:
      override def validate[V1, V2, C](
          validation: SchemaValidation[A, V1, V2, C]
      ): Primitive.Required.Reader[M, C] = Reader.Modify(this, validation)

    object Reader:
      final case class Modify[M, A, V1, V2, B](
          self: Primitive.Required.Reader[M, A],
          validation: SchemaValidation[A, V1, V2, B]
      ) extends Primitive.Required.Reader[M, B]:
        export self.tpe
        override def constraints: Chain[Constraint[?]] = validation.constraints

    sealed trait Writer[+M, -A] extends Primitive.Writer[M, A]:
      final override def contramap[C](f: C => A): Primitive.Required.Writer[M, C] = Writer.Modify(this, f)

    object Writer:
      final case class Modify[M, A, B](self: Primitive.Required.Writer[M, A], f: B => A)
          extends Primitive.Required.Writer[M, B]:
        export self.tpe

    final case class Modify[+M, A, V1, V2, B](
        self: Primitive.Required[M, A],
        validation: SchemaValidation[A, V1, V2, B],
        f: B => A
    ) extends Primitive.Required[M, B]:
      export self.tpe
      override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

    final case class Root[M, A](tpe: Type[A]) extends Primitive.Required[M, A]:
      override def constraints: Chain[Constraint[?]] = Chain.empty

  sealed trait Reader[+M, +A] extends Schema.Reader[M, Nothing, A]:
    def constraints: Chain[Constraint[?]]
    def optional: Primitive.Reader[M, Option[A]] = Reader.Optional(this)
    override def modify[N](f: M => N): Primitive.Reader[N, A] = ???
    def tpe: Type[?]
    def validate[V1, V2, C](validation: SchemaValidation[A, V1, V2, C]): Primitive.Reader[M, C] =
      Reader.Modify(this, validation)

  object Reader:
    final case class Modify[M, A, V1, V2, B](self: Primitive.Reader[M, A], validation: SchemaValidation[A, V1, V2, B])
        extends Primitive.Reader[M, B]:
      export self.tpe
      override def constraints: Chain[Constraint[?]] = validation.constraints

    final case class Optional[M, A](self: Primitive.Reader[M, A]) extends Primitive.Reader[M, Option[A]]:
      export self.{constraints, tpe}

  sealed trait Writer[+M, -A] extends Schema.Writer[M, Nothing, A]:
    def contramap[C](f: C => A): Primitive.Writer[M, C] = Writer.Modify(this, f)
    override def modify[N](f: M => N): Primitive.Writer[N, A] = ???
    def optional: Primitive.Writer[M, Option[A]] = Writer.Optional(this)
    def tpe: Type[?]

  object Writer:
    final case class Modify[M, A, B](self: Primitive.Writer[M, A], f: B => A) extends Primitive.Writer[M, B]:
      export self.tpe

    final case class Optional[M, A](self: Primitive.Writer[M, A]) extends Primitive.Writer[M, Option[A]]:
      export self.tpe

  final case class Modify[M, A, V1, V2, B](self: Primitive[M, A], validation: SchemaValidation[A, V1, V2, B], f: B => A)
      extends Primitive[M, B]:
    export self.tpe
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[M, A](self: Primitive[M, A]) extends Primitive[M, Option[A]]:
    export self.{constraints, tpe}

  given PrimitiveOps[Primitive, Primitive, Collection, Tuple] with
    extension [M, A, B](self: Primitive[M, B])
      override def collection: Collection[M, self.type, Vector[B]] = Collection.Root(self)
      override def modify[N](f: M => N): Primitive[N, B] = self.modify(f)
      override def optional: Primitive[M, Option[B]] = self.optional
      override def toTuple: Tuple[M, self.type, B] = Tuple.One(self)

    extension [M, A](self: Primitive[M, A]) override def tpe: Type[?] = self.tpe

sealed trait Tuple[+M, +A, B] extends Schema[M, A, B], Tuple.Reader[M, A, B], Tuple.Writer[M, A, B]:
  final def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Tuple[M, A, C] =
    Tuple.Modify(this, validation, f)
  override def modify[N](f: M => N): Tuple[N, A, B] = ???
  final override def optional: Tuple[M, A, Option[B]] = Tuple.Optional(this)
  override def schemas: Chain[Schema[M, ?, ?]]

object Tuple:
  sealed trait Reader[+M, +A, +B] extends Schema.Reader[M, A, B]:
    def schemas: Chain[Schema.Reader[M, ?, ?]]
    def constraints: Chain[Constraint[?]]
    def modify[N](f: M => N): Tuple.Reader[N, A, B] = ???
    def optional: Tuple.Reader[M, A, Option[B]] = Reader.Optional(this)
    final def validate[C, D, E](validation: SchemaValidation[B, C, D, E]): Tuple.Reader[M, A, E] =
      Reader.Modify(this, validation)

  object Reader:
    case object Empty extends Tuple.Reader[Nothing, Nothing, Unit]:
      override def constraints: Chain[Constraint[?]] = Chain.empty
      override def schemas: Chain[Nothing] = Chain.empty

    final case class Modify[+M, A, B, V1, V2, C](
        self: Tuple.Reader[M, A, B],
        validation: SchemaValidation[B, V1, V2, C]
    ) extends Tuple.Reader[M, A, C]:
      export self.schemas
      override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

    final case class One[M, +A <: Schema.Reader[M, ?, B], B](schema: A) extends Tuple.Reader[M, A, B]:
      override def constraints: Chain[Constraint[?]] = Chain.empty
      override def schemas: Chain[A] = Chain.one(schema)

    final case class Optional[M, A, B](self: Tuple.Reader[M, A, B]) extends Tuple.Reader[M, A, Option[B]]:
      export self.{constraints, schemas}

    final case class Zip[M, A, B, +C <: Schema.Reader[M, ?, D], D](
        left: Tuple.Reader[M, A, B],
        right: C
    ) extends Tuple.Reader[M, A | C, (B, D)]:
      override def constraints: Chain[Constraint[?]] = left.constraints
      override def schemas: Chain[Schema.Reader[M, ?, ?]] = left.schemas :+ right

  sealed trait Writer[+M, +A, -B] extends Schema.Writer[M, A, B]:
    final def contramap[C](f: C => B): Tuple.Writer[M, A, C] = Writer.Modify(this, f)
    def optional: Tuple.Writer[M, A, Option[B]] = Writer.Optional(this)
    def modify[N](f: M => N): Tuple.Writer[N, A, B] = ???
    def schemas: Chain[Schema.Writer[M, ?, ?]]

  object Writer:
    case object Empty extends Tuple.Writer[Nothing, Nothing, Unit]:
      override def schemas: Chain[Nothing] = Chain.empty

    final case class Modify[M, A, B, C](self: Tuple.Writer[M, A, B], f: C => B) extends Tuple.Writer[M, A, C]:
      export self.schemas

    final case class One[M, +A <: Schema.Writer[M, ?, B], B](schema: A) extends Tuple.Writer[M, A, B]:
      override def schemas: Chain[A] = Chain.one(schema)

    final case class Optional[M, A, B](self: Tuple.Writer[M, A, B]) extends Tuple.Writer[M, A, Option[B]]:
      export self.schemas

    final case class Zip[M, A, B, +C <: Schema.Writer[M, ?, D], D](
        left: Tuple.Writer[M, A, B],
        right: C
    ) extends Tuple.Writer[M, A | C, (B, D)]:
      override def schemas: Chain[Schema.Writer[M, ?, ?]] = left.schemas :+ right

  case object Empty extends Tuple[Nothing, Nothing, Unit]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
    override def schemas: Chain[Nothing] = Chain.empty

  final case class Modify[M, A, B, V1, V2, C](
      self: Tuple[M, A, B],
      validation: SchemaValidation[B, V1, V2, C],
      f: C => B
  ) extends Tuple[M, A, C]:
    export self.schemas
    override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class One[M, +A <: Schema[M, ?, B], B](schema: A) extends Tuple[M, A, B]:
    override def constraints: Chain[Constraint[?]] = Chain.empty
    override def schemas: Chain[A] = Chain.one(schema)

  final case class Optional[M, A, B](self: Tuple[M, A, B]) extends Tuple[M, A, Option[B]]:
    export self.{constraints, schemas}

  final case class Zip[M, A, B, +C <: Schema[M, ?, D], D](left: Tuple[M, A, B], right: C)
      extends Tuple[M, A | C, (B, D)]:
    override def constraints: Chain[Constraint[?]] = left.constraints
    override def schemas: Chain[Schema[M, ?, ?]] = left.schemas :+ right

// sealed trait Union[+F[+_], +A, B] extends Schema[F, A, B], Union.Reader[F, A, B], Union.Writer[F, A, B]:
//   override def schemas: NonEmptyChain[F[Schema[F, ?, ?]]]
//   final override def ivalidate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C])(f: C => B): Union[F, A, C] =
//     Union.Modify(this, validation, f)
//   final override def optional: Union[F, A, Option[B]] = Union.Optional(this)

// object Union:
//   sealed trait Reader[+F[+_], +A, +B] extends Schema.Reader[F, A, B]:
//     def schemas: NonEmptyChain[F[Schema.Reader[F, ?, ?]]]
//     final override def validate[V1, V2, C](validation: SchemaValidation[B, V1, V2, C]): Union.Reader[F, A, C] =
//       Reader.Modify(this, validation)
//     override def optional: Union.Reader[F, A, Option[B]] = Reader.Optional(this)

//   object Reader:
//     final case class Modify[F[+_], A, B, V1, V2, C](
//         self: Union.Reader[F, A, B],
//         validation: SchemaValidation[B, V1, V2, C]
//     ) extends Union.Reader[F, A, C]:
//       export self.schemas
//       override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

//     final case class One[F[+_], +A <: F[Schema.Reader[F, ?, B]], B](schema: A) extends Union.Reader[F, A, B]:
//       override def constraints: Chain[Constraint[?]] = Chain.empty
//       override def schemas: NonEmptyChain[A] = NonEmptyChain.one(schema)

//     final case class Optional[F[+_], A, B](self: Union.Reader[F, A, B]) extends Union.Reader[F, A, Option[B]]:
//       export self.{constraints, schemas}

//     final case class OrElse[F[+_], A, B, +C <: F[Schema.Reader[F, ?, D]], D](
//         left: Union.Reader[F, A, B],
//         right: C
//     ) extends Union.Reader[F, A | C, B + D]:
//       override def constraints: Chain[Constraint[?]] = left.constraints
//       override def schemas: NonEmptyChain[F[Schema.Reader[F, ?, ?]]] = left.schemas :+ right

//   sealed trait Writer[+F[+_], +A, -B] extends Schema.Writer[F, A, B]:
//     def schemas: NonEmptyChain[F[Schema.Writer[F, ?, ?]]]
//     final override def contramap[C](f: C => B): Union.Writer[F, A, C] = Writer.Modify(this, f)
//     override def optional: Union.Writer[F, A, Option[B]] = Writer.Optional(this)

//   object Writer:
//     final case class Modify[F[+_], A, B, C](self: Union.Writer[F, A, B], f: C => B) extends Union.Writer[F, A, C]:
//       export self.schemas

//     final case class One[F[+_], +A <: F[Schema.Writer[F, ?, B]], B](schema: A) extends Union.Writer[F, A, B]:
//       override def schemas: NonEmptyChain[A] = NonEmptyChain.one(schema)

//     final case class Optional[F[+_], A, B](self: Union.Writer[F, A, B]) extends Union.Writer[F, A, Option[B]]:
//       export self.schemas

//     final case class OrElse[F[+_], A, B, +C <: F[Schema.Writer[F, ?, D]], D](
//         left: Union.Writer[F, A, B],
//         right: C
//     ) extends Union.Writer[F, A | C, B + D]:
//       override def schemas: NonEmptyChain[F[Schema.Writer[F, ?, ?]]] = left.schemas :+ right

//   final case class Modify[F[+_], A, B, V1, V2, C](
//       self: Union[F, A, B],
//       validation: SchemaValidation[B, V1, V2, C],
//       f: C => B
//   ) extends Union[F, A, C]:
//     export self.schemas
//     override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

//   final case class One[F[+_], +A <: F[Schema[F, ?, B]], B](schema: A) extends Union[F, A, B]:
//     override def constraints: Chain[Constraint[?]] = Chain.empty
//     override def schemas: NonEmptyChain[A] = NonEmptyChain.one(schema)

//   final case class Optional[F[+_], A, B](self: Union[F, A, B]) extends Union[F, A, Option[B]]:
//     export self.{constraints, schemas}

//   final case class OrElse[F[+_], A, B, +C <: F[Schema[F, ?, D]], D](left: Union[F, A, B], right: C)
//       extends Union[F, A | C, B + D]:
//     override def constraints: Chain[Constraint[?]] = left.constraints
//     override def schemas: NonEmptyChain[F[Schema[F, ?, ?]]] = left.schemas :+ right
