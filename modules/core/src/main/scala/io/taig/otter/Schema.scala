package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import cats.data.NonEmptyChain
import io.taig.otter.validation.Validation
import io.taig.otter as Base
import scala.Product as SProduct
import io.taig.otter
import io.taig.enumeration.ext.Mapping

sealed trait Schema[+F, +O, A] extends SProduct, Serializable:
  final def collection: Collection[F, this.type, Vector[A]] = Collection.Root(Metadata.Empty, this)
  def metadata: Metadata
  def imap[B](f: A => B)(g: B => A): Schema[F, O, B]
  def optional: Schema[F, O, Option[A]]
  final def product: Product[F, this.type, A] = Product.One(Metadata.Empty, this)
  final def union: Union[F, this.type, A] = Union.Root(Metadata.Empty, this)
  def update(f: Metadata => Metadata): Schema[F, O, A]

object Schema:
  type Of[A] = Schema[Nothing, ?, A]
  type Via[F, A] = Schema[F, ?, A]

sealed trait Value[+F, +O, A] extends Schema[F, O, A]:
  override def imap[B](f: A => B)(g: B => A): Value[F, O, B]
  override def optional: Value[F, O, Option[A]]
  override def update(f: Metadata => Metadata): Value[F, O, A]

object Value:
  type Via[F, A] = Value[F, ?, A]

  sealed trait Required[+F, +B, C] extends Value[F, B, C]:
    override def imap[D](f: C => D)(g: D => C): Value.Required[F, B, D]
    override def update(f: Metadata => Metadata): Value.Required[F, B, C]

  object Required:
    type Via[F, A] = Value.Required[F, ?, A]

sealed trait Collection[+F, +O, A] extends Schema[F, O, A]:
  def constraints: Chain[Constraint.Collection]
  final override def imap[B](f: A => B)(g: B => A): Collection[F, O, B] = ivalidate(Validation.lift(f))(g)
  final def ivalidate[B, C](validation: SchemaValidation.Collection[A, B, C])(f: C => A): Collection[F, O, C] =
    Collection.Transform(this, validation, f)
  final def apply[B, C](transformation: SchemaTransformation.Collection[A, B, C]): Collection[F, O, C] =
    ivalidate(transformation.validation)(transformation.apply)
  final override def optional: Collection[F, O, Option[A]] = Collection.Optional(this)
  def schema: Schema[F, ?, ?]
  override def update(f: Metadata => Metadata): Collection[F, O, A]

object Collection:
  type Via[F, A] = Collection[F, ?, A]

  final case class Optional[F, B, C](self: Collection[F, B, C]) extends Collection[F, B, Option[C]]:
    export self.{constraints, metadata, schema}
    override def update(f: Metadata => Metadata): Collection[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, +B <: Schema[F, ?, C], C](metadata: Metadata, schema: B) extends Collection[F, B, Vector[C]]:
    override def constraints: Chain[Constraint.Collection] = Chain.empty
    override def update(f: Metadata => Metadata): Collection[F, B, Vector[C]] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D, E](
      self: Collection[F, B, C],
      validation: SchemaValidation.Collection[C, D, E],
      f: E => C
  ) extends Collection[F, B, E]:
    export self.{metadata, schema}
    override def constraints: Chain[Constraint.Collection] = self.constraints ++ validation.constraints
    override def update(f: Metadata => Metadata): Collection[F, B, E] = copy(self = self.update(f))

sealed trait Dictionary[+F, +B, C] extends Schema[F, B, C]:
  final override def imap[D](f: C => D)(g: D => C): Dictionary[F, B, D] = Dictionary.Transform(this, f, g)
  final override def optional: Dictionary[F, B, Option[C]] = Dictionary.Optional(this)
  override def update(f: Metadata => Metadata): Dictionary[F, B, C]

object Dictionary:
  type Via[F, A] = Dictionary[F, ?, A]

  final case class Optional[F, B, C](self: Dictionary[F, B, C]) extends Dictionary[F, B, Option[C]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dictionary[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, +O <: Schema[F, ?, B], A, B](metadata: Metadata, key: Primitive.Required[A], value: O)
      extends Dictionary[F, O, List[(A, B)]]:
    override def update(f: Metadata => Metadata): Dictionary[F, O, List[(A, B)]] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Dictionary[F, B, C], f: C => D, g: D => C) extends Dictionary[F, B, D]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dictionary[F, B, D] = copy(self = self.update(f))

sealed trait Dynamic[F, A] extends Schema[F, Nothing, A]:
  override def imap[B](f: A => B)(g: B => A): Dynamic[F, B] = Dynamic.Transform(this, f, g)
  override def optional: Dynamic[F, Option[A]] = Dynamic.Optional(this)
  override def update(f: Metadata => Metadata): Dynamic[F, A]

object Dynamic:
  final case class Optional[F, A](self: Dynamic[F, A]) extends Dynamic[F, Option[A]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dynamic[F, Option[A]] = copy(self = self.update(f))

  final case class Root[F](metadata: Metadata) extends Dynamic[F, F]:
    override def update(f: Metadata => Metadata): Dynamic[F, F] = copy(metadata = f(metadata))

  final case class Transform[F, A, B](self: Dynamic[F, A], f: A => B, g: B => A) extends Dynamic[F, B]:
    export self.metadata
    override def update(f: Metadata => Metadata): Dynamic[F, B] = copy(self = self.update(f))

sealed trait Enumeration[+F, +O, A] extends Value[F, O, A]:
  override def imap[B](f: A => B)(g: B => A): Enumeration[F, O, B] = Enumeration.Transform(this, f, g)
  override def optional: Enumeration[F, O, Option[A]] = Enumeration.Optional(this)
  override def update(f: Metadata => Metadata): Enumeration[F, O, A]

object Enumeration:
  type Via[F, A] = Enumeration[F, ?, A]

  sealed trait Required[+F, +B, C] extends Value.Required[F, B, C], Enumeration[F, B, C]:
    override def imap[D](f: C => D)(g: D => C): Enumeration.Required[F, B, D] = Required.Transform(this, f, g)
    override def update(f: Metadata => Metadata): Enumeration.Required[F, B, C]

  object Required:
    type Via[F, A] = Enumeration.Required[F, ?, A]

    final case class Root[F, O, A, B](metadata: Metadata, schema: Value.Required[F, O, A], mapping: Mapping[B, A])
        extends Enumeration.Required[F, O, B]:
      override def update(f: Metadata => Metadata): Enumeration.Required[F, O, B] = copy(metadata = f(metadata))

    final case class Transform[F, B, C, D](self: Enumeration.Required[F, B, C], f: C => D, g: D => C)
        extends Enumeration.Required[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Enumeration.Required[F, B, D] = copy(self = self.update(f))

  final case class Optional[F, B, C](self: Enumeration[F, B, C]) extends Enumeration[F, B, Option[C]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Enumeration[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, O <: Value[F, ?, A], A, B](metadata: Metadata, schema: O, mapping: Mapping[B, A])
      extends Enumeration[F, O, B]:
    override def update(f: Metadata => Metadata): Enumeration[F, O, B] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Enumeration[F, B, C], f: C => D, g: D => C) extends Enumeration[F, B, D]:
    export self.metadata
    override def update(f: Metadata => Metadata): Enumeration[F, B, D] = copy(self = self.update(f))

sealed trait Primitive[A] extends Value[Nothing, Nothing, A]:
  def constraints: Chain[Constraint.Primitive[?]]
  override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)
  def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
      f: D => A
  ): Primitive[D] = Primitive.Transform(this, validation, f)
  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
  def tpe: Type[?]
  override def update(f: Metadata => Metadata): Primitive[A]

object Primitive:
  sealed trait Required[A] extends Value.Required[Nothing, Nothing, A], Primitive[A]:
    final override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = ivalidate(Validation.lift(f))(g)
    override def ivalidate[B, C, D](validation: SchemaValidation.Primitive[A, B, C, D])(
        f: D => A
    ): Primitive.Required[D] = Required.Transform(this, validation, f)
    override def update(f: Metadata => Metadata): Primitive.Required[A]

  object Required:
    final case class Root[A](metadata: Metadata, tpe: Type[A]) extends Primitive.Required[A]:
      override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty
      override def update(f: Metadata => Metadata): Primitive.Required[A] = copy(metadata = f(metadata))

    final case class Transform[A, B, C, D](
        self: Primitive.Required[A],
        validation: SchemaValidation.Primitive[A, B, C, D],
        f: D => A
    ) extends Primitive.Required[D]:
      export self.{metadata, tpe}
      override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
      override def update(f: Metadata => Metadata): Primitive.Required[D] = copy(self = self.update(f))

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    export self.{constraints, metadata, tpe}
    override def update(f: Metadata => Metadata): Primitive[Option[A]] = copy(self = self.update(f))

  final case class Transform[A, B, C, D](
      self: Primitive[A],
      validation: SchemaValidation.Primitive[A, B, C, D],
      f: D => A
  ) extends Primitive[D]:
    export self.{metadata, tpe}
    override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
    override def update(f: Metadata => Metadata): Primitive[D] = copy(self = self.update(f))

sealed trait Product[+F, +B, C] extends Schema[F, B, C]:
  override def imap[D](f: C => D)(g: D => C): Product[F, B, D] = Product.Transform(this, f, g)
  override def optional: Product[F, B, Option[C]] = Product.Optional(this)
  def productWith[F1 >: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      product: Product[F1, D, E]
  ): Product[F1, B & D, (C, E)] =
    Product.Combine(merge(metadata, product.metadata), this, product)
  def schemas: Chain[Schema[F, ?, ?]]
  override def update(f: Metadata => Metadata): Product[F, B, C]

object Product:
  type Via[F, A] = Product[F, ?, A]

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Product[F, B, C], right: Product[F, D, E])
      extends Product[F, B & D, (C, E)]:
    override def schemas: Chain[Schema[F, ?, ?]] = left.schemas ++ right.schemas
    override def update(f: Metadata => Metadata): Product[F, B & D, (C, E)] = copy(metadata = f(metadata))

  case class Empty(metadata: Metadata) extends Product[Nothing, Nothing, Unit]:
    override def schemas: Chain[Nothing] = Chain.empty
    override def update(f: Metadata => Metadata): Product[Nothing, Nothing, Unit] = copy(metadata = f(metadata))

  final case class One[F, O <: Schema[F, ?, A], A](metadata: Metadata, schema: O) extends Product[F, O, A]:
    override def schemas: Chain[Schema[F, ?, ?]] = Chain.one(schema)
    override def update(f: Metadata => Metadata): Product[F, O, A] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Product[F, B, C]) extends Product[F, B, Option[C]]:
    export self.{metadata, schemas}
    override def update(f: Metadata => Metadata): Product[F, B, Option[C]] = copy(self = self.update(f))

  final case class Transform[F, B, C, D](self: Product[F, B, C], f: C => D, g: D => C) extends Product[F, B, D]:
    export self.{metadata, schemas}
    override def update(f: Metadata => Metadata): Product[F, B, D] = copy(self = self.update(f))

sealed trait Record[+F, +B, C] extends Schema[F, B, C]:
  def fields: Chain[Field[F, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Record[F, B, D] = Record.Transform(this, f, g)
  override def optional: Record[F, B, Option[C]] = Record.Optional(this)
  def productWith[F1 >: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      product: Record[F1, D, E]
  ): Record[F1, B & D, (C, E)] = Record.Combine(merge(metadata, product.metadata), this, product)
  override def update(f: Metadata => Metadata): Record[F, B, C]

object Record:
  type Via[F, A] = Record[F, ?, A]

  final case class Empty(metadata: Metadata) extends Record[Nothing, Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty
    override def update(f: Metadata => Metadata): Record[Nothing, Nothing, Unit] = copy(metadata = f(metadata))

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Record[F, B, C], right: Record[F, D, E])
      extends Record[F, B & D, (C, E)]:
    override def fields: Chain[Field[F, ?, ?]] = left.fields ++ right.fields
    override def update(f: Metadata => Metadata): Record[F, B & D, (C, E)] = copy(metadata = f(metadata))

  final case class One[F, B, C](metadata: Metadata, field: Field[F, B, C]) extends Record[F, B, C]:
    override def fields: Chain[Field[F, ?, ?]] = Chain.one(field)
    override def update(f: Metadata => Metadata): Record[F, B, C] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Record[F, B, C]) extends Record[F, B, Option[C]]:
    export self.{fields, metadata}
    override def update(f: Metadata => Metadata): Record[F, B, Option[C]] = copy(self = self.update(f))

  final case class Transform[F, B, C, D](self: Record[F, B, C], f: C => D, g: D => C) extends Record[F, B, D]:
    export self.{fields, metadata}
    override def update(f: Metadata => Metadata): Record[F, B, D] = copy(self = self.update(f))

sealed trait Sum[+F, +B, C] extends Schema[F, B, C]:
  def branches: NonEmptyChain[Branch[F, ?, ?]]
  override def imap[D](f: C => D)(g: D => C): Sum[F, B, D] = Sum.Transform(this, f, g)
  override def optional: Sum[F, B, Option[C]] = Sum.Optional(this)
  def orElseWith[F1 >: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      sum: Sum[F1, D, E]
  ): Sum[F1, B | D, Either[C, E]] =
    Sum.Combine(merge(metadata, sum.metadata), this, sum)
  override def update(f: Metadata => Metadata): Sum[F, B, C]

object Sum:
  type Via[F, A] = Sum[F, ?, A]

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Sum[F, B, C], right: Sum[F, D, E])
      extends Sum[F, B | D, Either[C, E]]:
    override def branches: NonEmptyChain[Branch[F, ?, ?]] = left.branches ++ right.branches
    override def update(f: Metadata => Metadata): Sum[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Sum[F, B, C]) extends Sum[F, B, Option[C]]:
    export self.{branches, metadata}
    override def update(f: Metadata => Metadata): Sum[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, B, C](metadata: Metadata, branch: Branch[F, B, C]) extends Sum[F, B, C]:
    override def branches: NonEmptyChain[Branch[F, B, C]] = NonEmptyChain.one(branch)
    override def update(f: Metadata => Metadata): Sum[F, B, C] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Sum[F, B, C], f: C => D, g: D => C) extends Sum[F, B, D]:
    export self.{branches, metadata}
    override def update(f: Metadata => Metadata): Sum[F, B, D] = copy(self = self.update(f))

sealed trait Union[+F, +B, C] extends Schema[F, B, C]:
  override def imap[D](f: C => D)(g: D => C): Union[F, B, D] = Union.Transform(this, f, g)
  override def optional: Union[F, B, Option[C]] = Union.Optional(this)
  def orElseWith[F1 >: F, D, E](merge: (Metadata, Metadata) => Metadata)(
      union: Union[F1, D, E]
  ): Union[F1, B | D, Either[C, E]] = Union.Combine(merge(metadata, union.metadata), this, union)
  override def update(f: Metadata => Metadata): Union[F, B, C]

object Union:
  type Via[F, A] = Union[F, ?, A]

  sealed trait Value[+F, +B, C] extends Base.Value[F, B, C], Union[F, B, C]:
    override def imap[D](f: C => D)(g: D => C): Union.Value[F, B, D] = Value.Transform(this, f, g)
    final override def optional: Union.Value[F, B, Option[C]] = Value.Optional(this)
    def orElseWith[F1 >: F, D, E](merge: (Metadata, Metadata) => Metadata)(
        union: Union.Value[F1, D, E]
    ): Union.Value[F1, B | D, Either[C, E]] = Value.Combine(merge(metadata, union.metadata), this, union)
    override def update(f: Metadata => Metadata): Union.Value[F, B, C]

  object Value:
    type Via[F, A] = Union.Value[F, ?, A]

    sealed trait Required[+F, +B, C] extends Base.Value.Required[F, B, C], Union.Value[F, B, C]:
      override def imap[D](f: C => D)(g: D => C): Union.Value.Required[F, B, D] = Required.Transform(this, f, g)
      def orElseWith[F1 >: F, D, E](merge: (Metadata, Metadata) => Metadata)(
          union: Union.Value.Required[F1, D, E]
      ): Union.Value.Required[F1, B | D, Either[C, E]] = Required.Combine(merge(metadata, union.metadata), this, union)
      override def update(f: Metadata => Metadata): Union.Value.Required[F, B, C]

    object Required:
      type Via[F, A] = Union.Value.Required[F, ?, A]

      final case class Combine[F, B, C, D, E](
          metadata: Metadata,
          left: Union.Value.Required[F, B, C],
          right: Union.Value.Required[F, D, E]
      ) extends Union.Value.Required[F, B | D, Either[C, E]]:
        override def update(f: Metadata => Metadata): Union.Value.Required[F, B | D, Either[C, E]] =
          copy(metadata = f(metadata))

      final case class Transform[F, B, C, D](self: Union.Value.Required[F, B, C], f: C => D, g: D => C)
          extends Union.Value.Required[F, B, D]:
        export self.metadata
        override def update(f: Metadata => Metadata): Union.Value.Required[F, B, D] = copy(self = self.update(f))

    final case class Combine[F, B, C, D, E](metadata: Metadata, left: Union.Value[F, B, C], right: Union.Value[F, D, E])
        extends Union.Value[F, B | D, Either[C, E]]:
      override def update(f: Metadata => Metadata): Union.Value[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

    final case class Optional[F, B, C](self: Union.Value[F, B, C]) extends Union.Value[F, B, Option[C]]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Value[F, B, Option[C]] = copy(self = self.update(f))

    final case class Transform[F, B, C, D](self: Union.Value[F, B, C], f: C => D, g: D => C)
        extends Union.Value[F, B, D]:
      export self.metadata
      override def update(f: Metadata => Metadata): Union.Value[F, B, D] = copy(self = self.update(f))

  final case class Combine[F, B, C, D, E](metadata: Metadata, left: Union[F, B, C], right: Union[F, D, E])
      extends Union[F, B | D, Either[C, E]]:
    override def update(f: Metadata => Metadata): Union[F, B | D, Either[C, E]] = copy(metadata = f(metadata))

  final case class Optional[F, B, C](self: Union[F, B, C]) extends Union[F, B, Option[C]]:
    export self.metadata
    override def update(f: Metadata => Metadata): Union[F, B, Option[C]] = copy(self = self.update(f))

  final case class Root[F, B <: Schema[F, ?, C], C](metadata: Metadata, schema: B) extends Union[F, B, C]:
    override def update(f: Metadata => Metadata): Union[F, B, C] = copy(metadata = f(metadata))

  final case class Transform[F, B, C, D](self: Union[F, B, C], f: C => D, g: D => C) extends Union[F, B, D]:
    export self.metadata
    override def update(f: Metadata => Metadata): Union[F, B, D] = copy(self = self.update(f))
