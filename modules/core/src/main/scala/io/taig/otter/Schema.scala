package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.Schema.Reader

sealed trait Schema[+M, +A, B] extends Schema.Reader[M, A, B], Schema.Writer[M, A, B]:
  // override def collectionWith[N >: M](metadata: N): Collection[N, this.type, Vector[B]] =
  //   Collection.Root(metadata, this)
  // def ivalidate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C])(f: C => B): Schema[M, A, C]
  override def modify[N](f: M => N): Schema[N, A, B]
  override def optional: Schema[M, A, Option[B]]

object Schema:
  sealed trait Reader[+M, +A, +B] extends Product, Serializable:
    // def collectionWith[O <: M](metadata: O): Collection.Reader[M, O, this.type, Vector[B]] =
    //   Collection.Reader.Root(metadata, this)
    // def constraints: Chain[Constraint[?]]
    def modify[N](f: M => N): Schema.Reader[N, A, B]
    def optional: Schema.Reader[M, A, Option[B]]
    // def validate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C]): Schema.Reader[M, A, C]

  sealed trait Writer[+M, +A, -B] extends Product, Serializable:
    // def collectionWith[O <: M](metadata: O): Collection.Writer[M, O, this.type, Vector[B]] =
    //   Collection.Writer.Root(metadata, this)
    def contramap[C](f: C => B): Schema.Writer[M, A, C]
    def modify[N](f: M => N): Schema.Writer[N, A, B]
    def optional: Schema.Writer[M, A, Option[B]]

sealed trait Collection[+M, +A, B] extends Schema[M, A, B], Collection.Reader[M, A, B], Collection.Writer[M, A, B]:
  // override def ivalidate[V1, V2, C](validation: SchemaValidation[M, B, V1, V2, C])(f: C => B): Collection[M, A, C] =
  //   Collection.Invariant(this, validation, f)
  override def modify[N](f: M => N): Collection[N, A, B]
  final override def optional: Collection[M, A, Option[B]] = Collection.Optional(this)
  override def schema: Schema[?, ?, ?]

object Collection:
  sealed trait Reader[+M, +A, +B] extends Schema.Reader[M, A, B]:
    // def constraints: Chain[Constraint[?]]
    override def modify[N](f: M => N): Collection.Reader[N, A, B]
    override def optional: Collection.Reader[M, A, Option[B]] = Collection.Reader.Optional(this)
    def schema: Schema.Reader[?, ?, ?]
    // final override def validate[V1, V2, C](
    //     validation: SchemaValidation[M, B, V1, V2, C]
    // ): Collection.Reader[M, A, C] = Reader.Functor(this, validation)

  object Reader:
    // final case class Functor[M, N <: M, A, B, V1, V2, C](
    //     self: Collection.Reader[M, A, B],
    //     validation: SchemaValidation[M, B, V1, V2, C]
    // ) extends Collection.Reader[M, A, C]:
    //   export self.schema
    //   // override def constraints: Chain[Constraint[?]] = validation.constraints
    //   override def modify[N](f: M => N): Collection.Reader[M, O, A, C] = copy(self = self.modify(f))
    //   override def translate[O](f: M => O): Collection.Reader[O, O, ?, C] = copy(
    //     self = self.translate(f),
    //     validation = validation.mapConstraint(_.leftMap(_.translate(f))).mapActual(_.leftMap(_.translate(f)))
    //   )

    final case class Optional[M, A, B](self: Collection.Reader[M, A, B]) extends Collection.Reader[M, A, Option[B]]:
      export self.schema
      override def modify[N](f: M => N): Collection.Reader[N, A, Option[B]] = copy(self = self.modify(f))

    final case class Root[M, A <: Schema.Reader[?, ?, B], B](metadata: M, schema: A)
        extends Collection.Reader[M, A, Vector[B]]:
      // override def constraints: Chain[Constraint[?]] = Chain.empty
      override def modify[N](f: M => N): Collection.Reader[N, A, Vector[B]] = copy(metadata = f(metadata))

  sealed trait Writer[+M, +A, -B] extends Schema.Writer[M, A, B]:
    final def contramap[C](f: C => B): Collection.Writer[M, A, C] = Writer.Contravariant(this, f)
    override def modify[N](f: M => N): Collection.Writer[N, A, B]
    def optional: Collection.Writer[M, A, Option[B]] = Writer.Optional(this)
    def schema: Schema.Writer[?, ?, ?]

  object Writer:
    final case class Contravariant[M, A, B, C](
        self: Collection.Writer[M, A, B],
        f: C => B
    ) extends Collection.Writer[M, A, C]:
      export self.schema
      override def modify[N](f: M => N): Collection.Writer[N, A, C] = copy(self = self.modify(f))

    final case class Optional[M, A, B](self: Collection.Writer[M, A, B]) extends Collection.Writer[M, A, Option[B]]:
      export self.schema
      override def modify[N](f: M => N): Collection.Writer[N, A, Option[B]] = copy(self = self.modify(f))

    final case class Root[M, A <: Schema.Writer[?, ?, B], B](metadata: M, schema: A)
        extends Collection.Writer[M, A, Vector[B]]:
      override def modify[N](f: M => N): Collection.Writer[N, A, Vector[B]] =
        copy(metadata = f(metadata))

  // final case class Invariant[M, N <: M, A, B, V1, V2, C](
  //     self: Collection[M, A, B],
  //     validation: SchemaValidation[M, B, V1, V2, C],
  //     f: C => B
  // ) extends Collection[M, A, C]:
  //   export self.schema
  //   // override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints
  //   override def modify[N](f: M => N): Collection[M, O, A, C] = copy(self = self.modify(f))
  //   override def translate[O](f: M => O): Collection[O, O, ?, C] = copy(
  //     self = self.translate(f),
  //     validation = validation.mapConstraint(_.leftMap(_.translate(f))).mapActual(_.leftMap(_.translate(f)))
  //   )

  final case class Optional[M, A, B](self: Collection[M, A, B]) extends Collection[M, A, Option[B]]:
    export self.schema
    override def modify[N](f: M => N): Collection[N, A, Option[B]] = copy(self = self.modify(f))

  final case class Root[M, A <: Schema[?, ?, B], B](metadata: M, schema: A) extends Collection[M, A, Vector[B]]:
    // override def constraints: Chain[Constraint[?]] = Chain.empty
    override def modify[N](f: M => N): Collection[N, A, Vector[B]] = copy(metadata = f(metadata))

// sealed trait Enumeration[+M, +A <: Schema[?, ?, ?], B] extends Schema[M, A, B]

sealed trait Primitive[+M, A] extends Schema[M, Nothing, A], Primitive.Reader[M, A], Primitive.Writer[M, A]:
  // override def ivalidate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): Primitive[M, B] =
  //   Primitive.Invariant(this, validation, f)
  override def modify[N](f: M => N): Primitive[N, A]
  final override def optional: Primitive[M, Option[A]] = Primitive.Optional(this)

object Primitive:
  sealed trait Required[+M, A]
      extends Primitive[M, A],
        Primitive.Required.Reader[M, A],
        Primitive.Required.Writer[M, A]:
    // final override def ivalidate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B])(
    //     f: B => A
    // ): Primitive.Required[M, B] = Required.Invariant(this, validation, f)
    override def modify[N](f: M => N): Primitive.Required[N, A]

  object Required:
    sealed trait Reader[+M, +A] extends Primitive.Reader[M, A]:
      override def modify[N](f: M => N): Primitive.Required.Reader[N, A]
      // final override def validate[V1, V2, B](
      //     validation: SchemaValidation[M, A, V1, V2, B]
      // ): Primitive.Required.Reader[M, B] = Reader.Functor(this, validation)

    // object Reader:
    // final case class Functor[M, N <: M, A, V1, V2, B](
    //     self: Primitive.Required.Reader[M, A],
    //     validation: SchemaValidation[M, A, V1, V2, B]
    // ) extends Primitive.Required.Reader[M, B]:
    //   export self.tpe
    //   // override def constraints: Chain[Constraint[?]] = validation.constraints
    //   override def modify[N](f: M => N): Primitive.Required.Reader[N, B] = copy(self = self.modify(f))

    sealed trait Writer[+M, -A] extends Primitive.Writer[M, A]:
      final override def contramap[B](f: B => A): Primitive.Required.Writer[M, B] = Writer.Contravariant(this, f)
      override def modify[N](f: M => N): Primitive.Required.Writer[N, A]

    object Writer:
      final case class Contravariant[M, A, B](self: Primitive.Required.Writer[M, A], f: B => A)
          extends Primitive.Required.Writer[M, B]:
        export self.tpe
        override def modify[N](f: M => N): Primitive.Required.Writer[N, B] = copy(self = self.modify(f))

    // final case class Invariant[M, N <: M, A, V1, V2, B](
    //     self: Primitive.Required[M, A],
    //     validation: SchemaValidation[M, A, V1, V2, B],
    //     f: B => A
    // ) extends Primitive.Required[M, B]:
    //   export self.tpe
    //   // override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints
    //   override def modify[N](f: M => N): Primitive.Required[M, O, B] = copy(self = self.modify(f))
    //   override def translate[O](f: M => O): Primitive.Required[O, O, B] = copy(
    //     self = self.translate(f),
    //     validation = validation.mapConstraint(_.leftMap(_.translate(f))).mapActual(_.leftMap(_.translate(f)))
    //   )

    final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]:
      // override def constraints: Chain[Constraint[?]] = Chain.empty
      override def modify[N](f: M => N): Primitive.Required[N, A] = copy(metadata = f(metadata))

  sealed trait Reader[+M, +A] extends Schema.Reader[M, Nothing, A]:
    override def modify[N](f: M => N): Primitive.Reader[N, A]
    override def optional: Primitive.Reader[M, Option[A]] = Reader.Optional(this)
    def tpe: Type[?]
    // override def validate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B]): Primitive.Reader[M, B] =
    //   Reader.Functor(this, validation)

  object Reader:
    // final case class Functor[M, N <: M, A, V1, V2, B](
    //     self: Primitive.Reader[M, A],
    //     validation: SchemaValidation[M, A, V1, V2, B]
    // ) extends Primitive.Reader[M, B]:
    //   export self.tpe
    //   // override def constraints: Chain[Constraint[?]] = validation.constraints
    //   override def modify[N](f: M => N): Primitive.Reader[M, O, B] = copy(self = self.modify(f))
    //   override def translate[O](f: M => O): Primitive.Reader[O, O, B] = ???

    final case class Optional[M, A](self: Primitive.Reader[M, A]) extends Primitive.Reader[M, Option[A]]:
      export self.tpe
      override def modify[N](f: M => N): Primitive.Reader[N, Option[A]] = copy(self = self.modify(f))

  sealed trait Writer[+M, -A] extends Schema.Writer[M, Nothing, A]:
    def contramap[C](f: C => A): Primitive.Writer[M, C] = Writer.Contravariant(this, f)
    override def modify[N](f: M => N): Primitive.Writer[N, A]
    def optional: Primitive.Writer[M, Option[A]] = Writer.Optional(this)
    def tpe: Type[?]

  object Writer:
    final case class Contravariant[M, A, B](self: Primitive.Writer[M, A], f: B => A) extends Primitive.Writer[M, B]:
      export self.tpe
      override def modify[N](f: M => N): Primitive.Writer[N, B] = copy(self = self.modify(f))

    final case class Optional[M, A](self: Primitive.Writer[M, A]) extends Primitive.Writer[M, Option[A]]:
      export self.tpe
      override def modify[N](f: M => N): Primitive.Writer[N, Option[A]] = copy(self = self.modify(f))

  // final case class Invariant[M, A, V1, V2, B](
  //     self: Primitive[M, A],
  //     validation: SchemaValidation[M, A, V1, V2, B],
  //     f: B => A
  // ) extends Primitive[M, B]:
  //   export self.tpe
  //   override def modify[N](f: M => N): Primitive[N, B] = copy(self = self.modify(f))
  //   // override def constraints: Chain[Constraint[?]] = self.constraints ++ validation.constraints

  final case class Optional[M, A](self: Primitive[M, A]) extends Primitive[M, Option[A]]:
    export self.tpe
    override def modify[N](f: M => N): Primitive[N, Option[A]] = copy(self = self.modify(f))

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
