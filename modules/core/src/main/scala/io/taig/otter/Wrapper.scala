package io.taig.otter

import cats.Eq
import cats.Eval
import cats.arrow.Profunctor
import cats.data.Chain
import cats.data.NonEmptyChain
import io.taig.enumeration.ext.Mapping
import io.taig.otter as Self
import io.taig.otter.operation.*
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import scala.collection.immutable.SortedMap

/** Derives the instances a format's wrapper type needs from those of the node it wraps.
  *
  * A format ties the recursive knot by wrapping each node in an [[Annotation]] and giving it its own type, so that
  * `Json.Record` is distinct from `Json.Collection`. Without this bundle every one of those wrappers would have to
  * restate the same instance for every type class it supports.
  */
abstract class Wrapper[Outer[-_, +_], Inner[-_, +_]](
    wrap: [w, r] => Annotation[Inner[w, r]] => Outer[w, r],
    unwrap: [w, r] => Outer[w, r] => Annotation[Inner[w, r]]
):
  final def annotation[W, R](self: Outer[W, R]): Annotation[Inner[W, R]] = unwrap(self)

  final def node[W, R](self: Outer[W, R]): Inner[W, R] = unwrap(self).self

  final def apply[W, R](self: Inner[W, R]): Outer[W, R] = wrap(Annotation(self))

  given annotated: [W, R] => Annotated[Outer[W, R]]:
    extension (self: Outer[W, R])
      override def lens: (Metadata, Metadata => Outer[W, R]) =
        val annotation = unwrap(self)
        (annotation.metadata, metadata => wrap(annotation.copy(metadata = metadata)))

  given profunctor: (P: Profunctor[Inner]) => Profunctor[Outer]:
    override def dimap[W0, R0, W, R](self: Outer[W0, R0])(f: W => W0)(g: R0 => R): Outer[W, R] =
      wrap(unwrap(self).map(P.dimap(_)(f)(g)))

  given zip: (Z: Zip[Inner]) => Zip[Outer]:
    override def zip[W1, R1, W2, R2](left: Outer[W1, R1], right: Outer[W2, R2]): Outer[(W1, W2), (R1, R2)] =
      wrap(Annotation(Z.zip(unwrap(left).self, unwrap(right).self)))

  given alt: (A: Alt[Inner]) => Alt[Outer]:
    override def alt[W1, R1, W2, R2](
        left: Outer[W1, R1],
        right: Outer[W2, R2]
    ): Outer[Either[W1, W2], Either[R1, R2]] = wrap(Annotation(A.alt(unwrap(left).self, unwrap(right).self)))

object Wrapper:
  abstract class Field[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Field[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Field[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Field[G, w, r]](wrap, unwrap):
    given operation: FieldOperation[Outer, G]:
      override def lift[W, R](name: String, schema: Reference[G, W, R]): Outer[W, R] =
        Field.this.apply(Self.Field.Root(name, schema))

      extension [W, R](fa: Outer[W, R])
        override def name: String = node(fa).name
        override def isOptional: Boolean = node(fa).isOptional
        override def optional: Outer[Option[W], Option[R]] = Field.this.apply(Self.Field.Optional(node(fa)))
        override def optional(default: => R): Outer[W, R] =
          Field.this.apply(Self.Field.Default(node(fa), Eval.later(default)))
        override def schema: Reference[G, ?, ?] = node(fa).schema

  abstract class Record[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Record[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Record[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Record[G, w, r]](wrap, unwrap):
    given operation: RecordOperation[Outer, G]:
      override def empty: Outer[Unit, Unit] = Record.this.apply(Self.Record.Empty)

      override def lift[W, R](field: Reference[G, W, R]): Outer[W, R] =
        Record.this.apply(Self.Record.Root(field))

      extension [W, R](fa: Outer[W, R]) override def fields: Chain[Reference[G, ?, ?]] = node(fa).fields

  abstract class Branch[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Branch[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Branch[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Branch[G, w, r]](wrap, unwrap):
    given operation: BranchOperation[Outer, G]:
      override def lift[W, R](name: String, schema: Reference[G, W, R]): Outer[W, R] =
        Branch.this.apply(Self.Branch.Root(name, schema))

      extension [W, R](fa: Outer[W, R])
        override def name: String = node(fa).name
        override def schema: Reference[G, ?, ?] = node(fa).schema

  abstract class Union[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Union[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Union[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Union[G, w, r]](wrap, unwrap):
    given operation: UnionOperation[Outer, G]:
      override def lift[W, R](branch: Reference[G, W, R]): Outer[W, R] =
        Union.this.apply(Self.Union.Root(branch))

      extension [W, R](fa: Outer[W, R]) override def branches: NonEmptyChain[Reference[G, ?, ?]] = node(fa).branches

  abstract class Collection[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Collection[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Collection[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Collection[G, w, r]](wrap, unwrap):
    given operation: CollectionOperation[Outer, G]:
      override def chained[W, R](
          schema: Reference[G, W, R],
          validation: Validation[Constraint.Collection, Chain[R]]
      ): Outer[Chain[W], Chain[R]] = Collection.this.apply(Self.Collection.Chained(schema, validation))

      override def indexed[W, R](
          schema: Reference[G, W, R],
          validation: Validation[Constraint.Collection, Vector[R]]
      ): Outer[Vector[W], Vector[R]] = Collection.this.apply(Self.Collection.Indexed(schema, validation))

      override def linked[W, R](
          schema: Reference[G, W, R],
          validation: Validation[Constraint.Collection, List[R]]
      ): Outer[List[W], List[R]] = Collection.this.apply(Self.Collection.Linked(schema, validation))

      extension [W, R](fa: Outer[W, R]) override def schema: Reference[G, ?, ?] = node(fa).schema

  abstract class Dictionary[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Dictionary[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Dictionary[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Dictionary[G, w, r]](wrap, unwrap):
    given operation: DictionaryOperation[Outer, G]:
      override def hashed[W, R](
          schema: Reference[G, W, R],
          validation: Validation[Constraint.Object, SortedMap[String, R]]
      ): Outer[SortedMap[String, W], SortedMap[String, R]] =
        Dictionary.this.apply(Self.Dictionary.Hashed(schema, validation))

      override def linked[W, R](
          schema: Reference[G, W, R],
          validation: Validation[Constraint.Object, List[(String, R)]]
      ): Outer[List[(String, W)], List[(String, R)]] =
        Dictionary.this.apply(Self.Dictionary.Linked(schema, validation))

      extension [W, R](fa: Outer[W, R]) override def schema: Reference[G, ?, ?] = node(fa).schema

  abstract class Optional[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Optional[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Optional[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Optional[G, w, r]](wrap, unwrap):
    given operation: OptionalOperation[Outer, G]:
      override def lift[W, R](schema: => Reference[G, W, R]): Outer[Option[W], Option[R]] =
        Optional.this.apply(Self.Optional.Root(schema))

      override def lift[W, R](schema: => Reference[G, W, R], default: => R): Outer[W, R] =
        Optional.this.apply(Self.Optional.Default(schema, Eval.later(default)))

      extension [W, R](fa: Outer[W, R]) override def schema: Reference[G, ?, ?] = node(fa).schema

  abstract class Tuple[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Tuple[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Tuple[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Tuple[G, w, r]](wrap, unwrap):
    given operation: TupleOperation[Outer, G]:
      override def empty: Outer[Unit, Unit] = Tuple.this.apply(Self.Tuple.Empty)

      override def lift[W, R](schema: Reference[G, W, R]): Outer[W, R] = Tuple.this.apply(Self.Tuple.Root(schema))

      extension [W, R](fa: Outer[W, R]) override def schemas: Chain[Reference[G, ?, ?]] = node(fa).schemas

  abstract class Enumeration[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Enumeration[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Enumeration[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Enumeration[G, w, r]](wrap, unwrap):
    given operation: EnumerationOperation[Outer, G]:
      override def lift[A, B](schema: Reference[G, A, A], mapping: Mapping[B, A]): Outer[B, B] =
        Enumeration.this.apply(Self.Enumeration.Root(schema, mapping))

      extension [W, R](fa: Outer[W, R]) override def schema: Reference[G, ?, ?] = node(fa).schema

  abstract class Coerce[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Coerce[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Coerce[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Coerce[G, w, r]](wrap, unwrap):
    given operation: CoerceOperation[Outer, G]:
      override def lift[W, R](schema: Reference[G, W, R]): Outer[W, R] = Coerce.this.apply(Self.Coerce.Root(schema))

      extension [W, R](fa: Outer[W, R]) override def schema: Reference[G, ?, ?] = node(fa).schema

  abstract class Constant[Outer[-_, +_], G[-_, +_]](
      wrap: [w, r] => Annotation[Self.Constant[G, w, r]] => Outer[w, r],
      unwrap: [w, r] => Outer[w, r] => Annotation[Self.Constant[G, w, r]]
  ) extends Wrapper[Outer, [w, r] =>> Self.Constant[G, w, r]](wrap, unwrap):
    given operation: ConstantOperation[Outer, G]:
      override def lift[A](schema: Reference[G, A, A], value: Eval[A], eq: Eq[A]): Outer[Unit, Unit] =
        Constant.this.apply(Self.Constant.Root(schema, value, eq))

      extension [W, R](fa: Outer[W, R]) override def schema: Reference[G, ?, ?] = node(fa).schema

  object Primitive:
    abstract class Boolean[Outer[-_, +_]](
        wrap: [w, r] => Annotation[Self.Primitive.Boolean[w, r]] => Outer[w, r],
        unwrap: [w, r] => Outer[w, r] => Annotation[Self.Primitive.Boolean[w, r]]
    ) extends Wrapper[Outer, Self.Primitive.Boolean](wrap, unwrap):
      given operation: PrimitiveOperation.Boolean[Outer]:
        override def boolean: Outer[SBoolean, SBoolean] = Boolean.this.apply(Self.Primitive.Boolean.Root)

    abstract class Number[Outer[-_, +_]](
        wrap: [w, r] => Annotation[Self.Primitive.Number[w, r]] => Outer[w, r],
        unwrap: [w, r] => Outer[w, r] => Annotation[Self.Primitive.Number[w, r]]
    ) extends Wrapper[Outer, Self.Primitive.Number](wrap, unwrap):
      given operation: PrimitiveOperation.Number[Outer]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): Outer[JBigDecimal, JBigDecimal] = Number.this.apply(Self.Primitive.Number.BigDecimal(validation))

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): Outer[JBigInteger, JBigInteger] = Number.this.apply(Self.Primitive.Number.BigInteger(validation))

        override def double(
            validation: Validation[Constraint.Primitive.Number, SDouble]
        ): Outer[SDouble, SDouble] = Number.this.apply(Self.Primitive.Number.Double(validation))

        override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): Outer[SFloat, SFloat] =
          Number.this.apply(Self.Primitive.Number.Float(validation))

        override def int(validation: Validation[Constraint.Primitive.Number, SInt]): Outer[SInt, SInt] =
          Number.this.apply(Self.Primitive.Number.Int(validation))

        override def long(validation: Validation[Constraint.Primitive.Number, SLong]): Outer[SLong, SLong] =
          Number.this.apply(Self.Primitive.Number.Long(validation))

    abstract class Text[Outer[-_, +_]](
        wrap: [w, r] => Annotation[Self.Primitive.Text[w, r]] => Outer[w, r],
        unwrap: [w, r] => Outer[w, r] => Annotation[Self.Primitive.Text[w, r]]
    ) extends Wrapper[Outer, Self.Primitive.Text](wrap, unwrap):
      given operation: PrimitiveOperation.Text[Outer]:
        override def string(validation: Validation[Constraint.Primitive.Text, String]): Outer[String, String] =
          Text.this.apply(Self.Primitive.Text.Root(validation))

        override def codec[W, R](
            name: String,
            parse: String => Either[String, R],
            print: W => String
        ): Outer[W, R] = Text.this.apply(Self.Primitive.Text.Codec(name, parse, print))
