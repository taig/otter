package io.taig.otter.schema

import cats.Eval
import cats.syntax.all.*
import cats.data.{Chain, NonEmptyChain}
import io.taig.otter.validation.*
import io.taig.enumeration.ext.Mapping

import scala.annotation.targetName
import scala.collection.immutable.ListMap

sealed abstract class Schema[A]:
  type Self[a] <: Schema[a] { type Self[a] = Schema.this.Self[a] }

  def constraints: Chain[Constraint]

  abstract class Property[B]:
    def value: B
    def modify(f: B => B): Self[A]
    final def apply(b: B): Self[A] = modify(_ => b)

  object Property:
    abstract class Optional[B] extends Property[Option[B]]:
      @targetName("as")
      final def apply(b: B): Self[A] = apply(Some(b))
      final def clear: Self[A] = apply(None)

    object Optional:
      def apply[B](b: Option[B], g: (Option[B] => Option[B]) => Self[A]): Property.Optional[B] = new Optional[B]:
        override def value: Option[B] = b
        override def modify(f: Option[B] => Option[B]): Self[A] = g(f)

      def apply[B, C](
          schema: Schema[B],
          property: schema.type => schema.Property.Optional[B],
          copy: Option[B] => Self[A],
          validation: Validation[B, C],
          g: C => B
      ): Property.Optional[C] = new Optional[C]:
        override def value: Option[C] = property(schema).value.flatMap(validation(_).toOption)
        override def modify(f: Option[C] => Option[C]): Self[A] =
          copy(f(property(schema).value.flatMap(validation(_).toOption)).map(g))

  def description: Property.Optional[String]
  def example: Property.Optional[A]

  def optional: Self[Option[A]]
  def isOptional: Boolean

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

  final infix def zip[B](other: Schema[B]): Product[(A, B)] =
    Schema.Product.Zip(toProduct, other.toProduct, Schema.Product.Properties.Empty)

  final def toProduct: Product[A] = this match
    case schema: Product[?] => schema
    case schema             => Schema.Product(schema)

object Schema extends ToSchemaOps:
  sealed abstract class Value[A] extends Schema[A]:
    override type Self[a] <: Value[a] { type Self[a] = Value.this.Self[a] }

  sealed abstract class Primitive[A] extends Schema.Value[A]:
    final override type Self[a] = Primitive[a]
    def tpe: Type[?]
    def format: Property.Optional[String]
    final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
      Primitive.Validate(this, validation, g)

  object Primitive:
    final private[otter] case class Properties[+A](
        description: Option[String],
        example: Option[A],
        format: Option[String]
    )

    object Properties:
      val Empty: Primitive.Properties[Nothing] = Properties(None, None, None)

    final private[otter] case class Root[A](properties: Primitive.Properties[A], tpe: Type[A]) extends Primitive[A]:
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[A] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )
      override def format: Property.Optional[String] = Property.Optional(
        properties.format,
        f => copy(properties = properties.copy(format = f(properties.format)))
      )

    final private[otter] case class Validate[A, B](self: Primitive[A], validation: Validation[A, B], g: B => A)
        extends Primitive[B]:
      export self.{isOptional, tpe}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[B] =
        Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)
      override def format: Property.Optional[String] =
        Property.Optional(self.format.value, f => copy(self = self.format.modify(f)))

    final private[otter] case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
      export self.{constraints, tpe}
      override def isOptional: Boolean = true
      override def format: Property.Optional[String] = Property.Optional(
        self.format.value,
        f => copy(self = self.format.modify(f))
      )
      override def description: Property.Optional[String] = Property.Optional(
        self.description.value,
        f => copy(self = self.description.modify(f))
      )
      override def example: Property.Optional[Option[A]] = Property.Optional(
        self.example.value.map(_.some),
        f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
      )

    def apply[A](tpe: Type[A]): Primitive[A] = Root(Properties.Empty, tpe)

  sealed abstract class Collection[Of[a] <: Schema[a], A] extends Schema[A]:
    override type Self[a] = Collection[Of, a]

    // Nasty, but unfortunately necessary as described in https://docs.scala-lang.org/scala3/guides/migration/incompat-other-changes.html#wildcard-type-argument
    final class Reference[B](val reference: Eval[Of[B]]):
      def value: Of[B] = reference.value

    def of: Reference[?] // TODO def of: Eval[Of[?]] -> unreducible application of higher-kinded type Collection.this.Of to wildcard arguments :-(

    final override def optional: Collection[Of, Option[A]] = Collection.Optional(this)

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection[Of, B] =
      Collection.Validate(this, validation, g)

  object Collection:
    final private[otter] case class Properties[+A](description: Option[String], example: Option[A])

    object Properties:
      val Empty: Collection.Properties[Nothing] = Properties(None, None)

    final private[otter] case class Root[Of[a] <: Schema[a], A](schema: Eval[Of[A]], properties: Properties[Chain[A]])
        extends Collection[Of, Chain[A]]:
      override def of: Reference[A] = new Reference(schema)
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[Chain[A]] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )

    final private[otter] case class Validate[Of[a] <: Schema[a], A, B](
        self: Collection[Of, A],
        validation: Validation[A, B],
        g: B => A
    ) extends Collection[Of, B]:
      export self.isOptional
      override def of: Reference[?] = new Reference(self.of.reference)
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[B] =
        Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

    final private[otter] case class Optional[Of[a] <: Schema[a], A](self: Collection[Of, A])
        extends Collection[Of, Option[A]]:
      export self.constraints
      override def of: Reference[?] = new Reference(self.of.reference)
      override def isOptional: Boolean = true
      override def description: Property.Optional[String] = Property.Optional(
        self.description.value,
        f => copy(self = self.description.modify(f))
      )
      override def example: Property.Optional[Option[A]] = Property.Optional(
        self.example.value.map(_.some),
        f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
      )

    def apply[Of[a] <: Schema[a], A](schema: Eval[Of[A]]): Collection[Of, Chain[A]] = Root(schema, Properties.Empty)

  // TODO add null property
  sealed abstract class Dictionary[A] extends Schema[A]:
    override type Self[a] = Dictionary[a]
    def key: Eval[Schema.Value[?]]
    def schema: Eval[Schema[?]]
    final override def optional: Dictionary[Option[A]] = Dictionary.Optional(this)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] =
      Dictionary.Validate(this, validation, g)

  object Dictionary:
    final private[otter] case class Properties[+A](description: Option[String], example: Option[A])

    object Properties:
      val Empty: Dictionary.Properties[Nothing] = Properties(None, None)

    final private[otter] case class Root[A, B](
        key: Eval[Schema.Value[A]],
        schema: Eval[Schema[B]],
        properties: Properties[ListMap[A, B]]
    ) extends Dictionary[ListMap[A, B]]:
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[ListMap[A, B]] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )

    //    override def decode(otter: OpenApi.Object): Validated[Violations, SeqMap[A, B]] = otter.toChain
    //      .traverse { case (k, v) =>
    //        (key.value.parse(k), schema.value.decode(v)).tupled.leftMap(_.modifyHistory(k /: _))
    //      }
    //      .map(chain => SeqMap.from(chain.iterator))
    //    override def encode(abs: SeqMap[A, B]): OpenApi.Object =
    //      OpenApi.Object(abs.map { case (k, v) => (key.value.encode(k).render, schema.value.encode(v)) }.to(VectorMap))

    final private[otter] case class Validate[A, B](self: Dictionary[A], validation: Validation[A, B], g: B => A)
        extends Dictionary[B]:
      export self.{isOptional, key, schema}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[B] =
        Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

    final private[otter] case class Optional[A](self: Dictionary[A]) extends Dictionary[Option[A]]:
      export self.{constraints, key, schema}
      override def isOptional: Boolean = false
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[Option[A]] = Property.Optional(
        self.example.value.map(_.some),
        f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
      )

    def apply[A, B](key: Eval[Schema.Value[A]], schema: Eval[Schema[B]]): Dictionary[ListMap[A, B]] =
      Root(key, schema, Properties.Empty)

  // TODO add null property
  sealed abstract class Coproduct[A] extends Schema[A]:
    final override type Self[a] = Coproduct[a]
    def toNonEmptyChain: NonEmptyChain[Branch[?, ?]]

    abstract class Discriminators extends Property[Discriminator]:
      final def nested(identifier: String, value: String): Coproduct[A] =
        apply(Discriminator.Nested(identifier, value))
      final def merged(identifier: String): Coproduct[A] = apply(Discriminator.Merged(identifier))
      final def keyed: Coproduct[A] = apply(Discriminator.Keyed)
      final def none: Coproduct[A] = apply(Discriminator.None)

    object Discriminators:
      def apply(
          discriminator: Discriminator,
          g: (Discriminator => Discriminator) => Self[A]
      ): Discriminators = new Discriminators:
        override def value: Discriminator = discriminator
        override def modify(f: Discriminator => Discriminator): Self[A] = g(f)

    def discriminator: Discriminators
    final infix def orElse[B](other: Coproduct[B]): Coproduct[A + B] =
      Coproduct.OrElse(this, other, Coproduct.Properties.Empty)
    final def :+[B, C](branch: Branch[B, C]): Coproduct[A + C] = orElse(branch.toCoproduct)
    final def +:[B, C](branch: Branch[B, C]): Coproduct[C + A] = branch.toCoproduct.orElse(this)
    final override def optional: Coproduct[Option[A]] = Coproduct.Optional(this)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Coproduct[B] =
      Coproduct.Validate(this, validation, g)
    final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Coproduct[B] = imap(evidence.from)(evidence.to)

  //  final override def decode(otter: OpenApi): Validated[Violations, B] = tryDecode(otter) match
  //    case Ior.Left(violations) => violations.invalid
  //    case Ior.Right(Some(b))   => b.valid
  //    case Ior.Right(None) =>
  //      renderDiscriminator(otter) match
  //        case Some(discriminator) =>
  //          val names = toNonEmptyChain.map(branch => OpenApi.fromString(branch.renderName)).toNonEmptyVector.toVector
  //          Violations.rootNec(Constraint.collection.oneOf(OpenApi.Array(names)).toViolation(discriminator)).invalid
  //        case None => typeViolations("Sum", otter).invalid
  //    case Ior.Both(violations, b) => b.toValid(violations)
  //  final private def renderDiscriminator(otter: OpenApi): Option[OpenApi.Primitive] = discriminator match
  //    case Discriminator.Nested(identifier, _) =>
  //      otter.asObject.flatMap(_.get(identifier)).flatMap(_.asPrimitive)
  //    case Discriminator.Merged(identifier) =>
  //      otter.asObject.flatMap(_.get(identifier)).flatMap(_.asPrimitive)
  //    case Discriminator.Keyed => otter.asObject.flatMap(_.keys.headOption.map(OpenApi.fromString))
  //    case Discriminator.None  => None

  object Coproduct:
    extension [A <: Matchable](self: Coproduct[A])
      inline def |[B <: Matchable](other: Coproduct[B]): Coproduct[A | B] = self
        .orElse(other)
        .imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

    final private[otter] case class Properties[+A](
        description: Option[String],
        discriminator: Discriminator,
        example: Option[A]
    )

    object Properties:
      val Empty: Coproduct.Properties[Nothing] = Properties(None, Discriminator.Default, None)

    final private[otter] case class Root[A, B](
        branch: Branch[A, B],
        properties: Coproduct.Properties[B]
    ) extends Coproduct[B]:
      override def constraints: Chain[Constraint] = Chain.empty
      override def toNonEmptyChain: NonEmptyChain[Branch[A, B]] = NonEmptyChain.one(branch)
      override def isOptional: Boolean = false
      override def discriminator: Discriminators = Discriminators(
        properties.discriminator,
        f => copy(properties = properties.copy(discriminator = f(properties.discriminator)))
      )
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[B] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )

    final private[otter] case class OrElse[A, B](left: Coproduct[A], right: Coproduct[B], properties: Properties[A + B])
        extends Coproduct[A + B] {
      override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
      override def toNonEmptyChain: NonEmptyChain[Branch[?, ?]] = left.toNonEmptyChain ++ right.toNonEmptyChain
      override def isOptional: Boolean = left.isOptional && right.isOptional
      override def discriminator: Discriminators = Discriminators(
        properties.discriminator,
        f => copy(properties = properties.copy(discriminator = f(properties.discriminator)))
      )
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[A + B] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )
    }

    //    override def tryDecode(otter: OpenApi): Ior[Violations, Option[B + C]] = left.tryDecode(otter) match
    //      case Ior.Right(Some(b)) => b.asLeft.some.rightIor
    //      case Ior.Right(None) =>
    //        right.tryDecode(otter) match
    //          case Ior.Left(right)    => right.leftIor
    //          case Ior.Right(c)       => c.map(_.asRight).rightIor
    //          case Ior.Both(right, c) => right.leftIor.putRight(c.map(_.asRight))
    //      case Ior.Left(left)          => Ior.Left(left)
    //      case Ior.Both(left, Some(b)) => left.leftIor.putRight(b.asLeft.some)
    //      case Ior.Both(left, None) =>
    //        right.tryDecode(otter) match
    //          case Ior.Left(right)    => (left merge right).leftIor
    //          case Ior.Right(c)       => left.leftIor.putRight(c.map(_.asRight))
    //          case Ior.Both(right, c) => (left merge right).leftIor.putRight(c.map(_.asRight))

    final private[otter] case class Validate[A, B](self: Coproduct[A], validation: Validation[A, B], g: B => A)
        extends Coproduct[B]:
      export self.{isOptional, toNonEmptyChain}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def discriminator: Discriminators =
        Discriminators(self.discriminator.value, f => copy(self = self.discriminator.modify(f)))
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[B] =
        Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

    final private[otter] case class Optional[A](self: Coproduct[A]) extends Coproduct[Option[A]]:
      export self.{constraints, toNonEmptyChain}
      override def isOptional: Boolean = true
      override def discriminator: Discriminators =
        Discriminators(self.discriminator.value, f => copy(self = self.discriminator.modify(f)))
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[Option[A]] = Property.Optional(
        self.example.value.map(_.some),
        f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
      )

    def apply[A, B](branch: Branch[A, B]): Coproduct[B] = Root(branch, Properties.Empty)

  sealed abstract class Enumeration[A] extends Schema.Value[A]:
    final override type Self[a] = Enumeration[a]
    def schema: Eval[Schema.Value[?]]
    def values[B](encoder: Encoder[Schema.Value, B]): List[B]
    final override def optional: Enumeration[Option[A]] = Enumeration.Optional(this)
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] =
      Enumeration.Validate(this, validation, g)

  object Enumeration:
    final private[otter] case class Properties[+A](description: Option[String], example: Option[A])

    object Properties:
      val Empty: Enumeration.Properties[Nothing] = Properties(None, None)

    final private[otter] case class Root[A, B](
        mapping: Mapping[B, A],
        schema: Eval[Schema.Value[A]],
        properties: Enumeration.Properties[B]
    ) extends Enumeration[B]:
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def values[C](encoder: Encoder[Schema.Value, C]): List[C] =
        mapping.values.map(b => encoder.encode(schema.value, mapping.inj(b)))
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[B] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )

    final private[otter] case class Validate[A, B](
        self: Enumeration[A],
        validation: Validation[A, B],
        g: B => A
    ) extends Enumeration[B]:
      export self.{isOptional, schema, values}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[B] =
        Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

    final private[otter] case class Optional[A](self: Enumeration[A]) extends Enumeration[Option[A]]:
      export self.{constraints, schema, values}
      override def isOptional: Boolean = true
      override def description: Property.Optional[String] = Property.Optional(
        self.description.value,
        f => copy(self = self.description.modify(f))
      )
      override def example: Property.Optional[Option[A]] = Property.Optional(
        self.example.value.map(_.some),
        f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
      )

    def apply[A, B](schema: Eval[Schema.Value[A]], mapping: Mapping[B, A]): Enumeration[B] =
      Root(mapping, schema, Properties.Empty)

  sealed abstract class Product[A] extends Schema[A]:
    override type Self[a] = Product[a]
    def toChain: Chain[Eval[Schema[?]]]
    final override def optional: Product[Option[A]] = Product.Optional(this)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Product[B] =
      Product.Validate(this, validation, g)
    final def to[B](using evidence: Evidence.Product.Aux[B, A]): Product[B] = imap(evidence.from)(evidence.to)

  object Product:
    final private[otter] case class Properties[+A](description: Option[String], example: Option[A])

    object Properties:
      val Empty: Product.Properties[Nothing] = Properties(None, None)

    final private[otter] case class Empty(properties: Properties[Unit]) extends Product[Unit]:
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def toChain: Chain[Eval[Schema[?]]] = Chain.empty
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[Unit] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )

    final private[otter] case class One[A](schema: Eval[Schema[A]], properties: Properties[A]) extends Product[A]:
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def toChain: Chain[Eval[Schema[A]]] = Chain.one(schema)
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[A] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )

    final private[otter] case class Zip[A, B](left: Product[A], right: Product[B], properties: Properties[(A, B)])
        extends Product[(A, B)]:
      override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
      override def isOptional: Boolean = left.isOptional && right.isOptional
      override def toChain: Chain[Eval[Schema[?]]] = left.toChain ++ right.toChain
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[(A, B)] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )

    final private[otter] case class Validate[A, B](self: Product[A], validation: Validation[A, B], g: B => A)
        extends Product[B]:
      export self.{isOptional, toChain}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[B] =
        Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

    final private[otter] case class Optional[A](self: Product[A]) extends Product[Option[A]]:
      export self.{constraints, toChain}
      override def isOptional: Boolean = true
      override def description: Property.Optional[String] = Property.Optional(
        self.description.value,
        f => copy(self = self.description.modify(f))
      )
      override def example: Property.Optional[Option[A]] = Property.Optional(
        self.example.value.map(_.some),
        f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
      )

    val empty: Product[Unit] = Empty(Properties.Empty)
    def apply[A](schema: => Schema[A]): Product[A] = One(Eval.later(schema), Properties.Empty)

  // TODO add allow/disallow additional properties
  sealed abstract class Record[A] extends Schema[A]:
    final override type Self[a] = Record[a]
    def toChain: Chain[Field[?, ?]]

    trait Nulls extends Property[Null]:
      final def show: Record[A] = apply(Null.Show)
      final def hide: Record[A] = apply(Null.Hide)

    object Nulls:
      def apply(a: Null, g: (Null => Null) => Record[A]): Nulls = new Nulls:
        override def value: Null = a
        override def modify(f: Null => Null): Record[A] = g(f)

    def nulls: Nulls
    final infix def zip[B](right: Record[B]): Record[(A, B)] = Record.Zip(this, right, Record.Properties.Empty)
    override def optional: Record[Option[A]] = Record.Optional(this)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Record[B] =
      Record.Validate(this, validation, g)
    final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = imap(evidence.from)(evidence.to)

  object Record extends ToRecordOps:
    final private[otter] case class Properties[+A](description: Option[String], example: Option[A], nulls: Null)

    object Properties:
      val Empty: Record.Properties[Nothing] = Properties(None, None, Null.Default)

    final private[otter] case class Empty(properties: Record.Properties[Unit]) extends Record[Unit]:
      override def toChain: Chain[Field[?, ?]] = Chain.empty
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[Unit] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )
      override def nulls: Nulls = Nulls(
        properties.nulls,
        f => copy(properties = properties.copy(nulls = f(properties.nulls)))
      )

    final private[otter] case class One[A, B](field: Field[A, B], properties: Record.Properties[B]) extends Record[B]:
      override def toChain: Chain[Field[A, ?]] = Chain.one(field)
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[B] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )
      override def nulls: Nulls = Nulls(
        properties.nulls,
        f => copy(properties = properties.copy(nulls = f(properties.nulls)))
      )

    final private[otter] case class Zip[A, B](left: Record[A], right: Record[B], properties: Properties[(A, B)])
        extends Record[(A, B)]:
      override def toChain: Chain[Field[?, ?]] = left.toChain ++ right.toChain
      override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
      override def isOptional: Boolean = left.isOptional && right.isOptional
      override def description: Property.Optional[String] = Property.Optional(
        properties.description,
        f => copy(properties = properties.copy(description = f(properties.description)))
      )
      override def example: Property.Optional[(A, B)] = Property.Optional(
        properties.example,
        f => copy(properties = properties.copy(example = f(properties.example)))
      )
      override def nulls: Nulls = Nulls(
        properties.nulls,
        f => copy(properties = properties.copy(nulls = f(properties.nulls)))
      )

    final private[otter] case class Validate[A, B](self: Record[A], validation: Validation[A, B], g: B => A)
        extends Record[B]:
      export self.{isOptional, toChain}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def description: Property.Optional[String] =
        Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
      override def example: Property.Optional[B] =
        Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)
      override def nulls: Nulls = Nulls(self.nulls.value, f => copy(self = self.nulls.modify(f)))

    final private[otter] case class Optional[A](self: Record[A]) extends Record[Option[A]]:
      export self.{constraints, toChain}
      override def isOptional: Boolean = true
      override def description: Property.Optional[String] = Property.Optional(
        self.description.value,
        f => copy(self = self.description.modify(f))
      )
      override def example: Property.Optional[Option[A]] = Property.Optional(
        self.example.value.map(_.some),
        f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
      )
      override def nulls: Nulls = Nulls(self.nulls.value, f => copy(self = self.nulls.modify(f)))

    val empty: Record[Unit] = Empty(Properties.Empty)
    def apply[A, B](field: Field[A, B]): Record[B] = One(field, Properties.Empty)

  final class RecordOps[A](self: Record[A]) extends AnyVal:
    inline def :*[B, C](other: Field[B, C]): Record[(A, C)] = self.zip(other.toRecord)
    inline def *:[B, C](other: Field[B, C]): Record[(C, A)] = other.toRecord.zip(self)
    inline def :*[B](other: Field[B, Unit]): Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
    inline def *:[B](other: Field[B, Unit]): Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))

  final class RecordOpsUnit(self: Record[Unit]) extends AnyVal:
    inline def :*[A, B](other: Field[A, B]): Record[B] = self.zip(other.toRecord).imap { case (_, b) => b }(((), _))
    inline def *:[A, B](other: Field[A, B]): Record[B] = other.toRecord.zip(self).imap { case (b, _) => b }((_, ()))

  final class RecordOpsTuple[A <: Tuple](self: Record[A]) extends AnyVal:
    inline def :*[B, C](other: Field[B, C]): Record[Tuple.Append[A, C]] =
      self.zip(other.toRecord).imap { case (a, c) => a :* c }(ac => (ac.init.asInstanceOf[A], ac.last.asInstanceOf[C]))
    inline def *:[B, C](other: Field[B, C]): Record[C *: A] =
      other.toRecord.zip(self).imap { case (c, a) => c *: a } { case c *: a => (c, a) }
    inline def :*[B](other: Field[B, Unit]): Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
    inline def *:[B](other: Field[B, Unit]): Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))

  trait ToRecordOps extends ToRecordOps1:
    implicit def toRecordOpsUnit(self: Record[Unit]): RecordOpsUnit = RecordOpsUnit(self)
    implicit def toRecordOpsTuple[A <: Tuple](self: Record[A]): RecordOpsTuple[A] = RecordOpsTuple(self)

  trait ToRecordOps1:
    implicit def toRecordOps[A](self: Record[A]): RecordOps[A] = RecordOps(self)

final class SchemaOps[A](self: Schema[A]) extends AnyVal:
  inline def :*[B](other: Schema[B]): Product[(A, B)] = self.zip(other)
  inline def *:[B](other: Schema[B]): Product[(B, A)] = other.zip(self)
  inline def :*(other: Schema[Unit]): Product[A] = self.zip(other).imap { case (a, _) => a }((_, ()))
  inline def *:(other: Schema[Unit]): Product[A] = other.zip(self).imap { case (_, a) => a }(((), _))
final class SchemaOpsUnit(self: Schema[Unit]) extends AnyVal:
  inline def :*[A](other: Schema[A]): Product[A] = other :* self
  inline def *:[A](other: Schema[A]): Product[A] = other :* self
final class SchemaOpsTuple[A <: Tuple](self: Schema[A]) extends AnyVal:
  inline def :*[B](other: Schema[B]): Product[Tuple.Append[A, B]] =
    self.zip(other).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
  inline def *:[B](other: Schema[B]): Product[B *: A] =
    other.zip(self).imap { case (b, a) => b *: a } { case b *: a => (b, a) }
  inline def :*(other: Schema[Unit]): Product[A] = self.zip(other).imap { case (a, _) => a }((_, ()))
  inline def *:(other: Schema[Unit]): Product[A] = other.zip(self).imap { case (_, a) => a }(((), _))

trait ToSchemaOps extends ToSchemaOps1:
  implicit final def toSchemaOpsTuple[A <: Tuple](self: Schema[A]): SchemaOpsTuple[A] = new SchemaOpsTuple[A](self)
  implicit final def toSchemaOpsUnit(self: Schema[Unit]): SchemaOpsUnit = new SchemaOpsUnit(self)
trait ToSchemaOps1:
  implicit final def toSchemaOps[A](self: Schema[A]): SchemaOps[A] = new SchemaOps[A](self)
