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
  /** The same bundle, for a node that carries the type of what is inside it.
    *
    * A container's `S` is the type of its children, so its instances have to exist for every `S` rather than for one
    * fixed `Outer`. That is the only difference from [[Wrapper]], which the leaves still use.
    */
  abstract class Nested[
      Bound[-_, +_],
      Outer[_[-w, +r] <: Bound[w, r], -_, +_],
      Inner[_[-w, +r] <: Bound[w, r], -_, +_]
  ](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Inner[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Inner[s, w, r]]
  ):
    final def annotation[S[-w, +r] <: Bound[w, r], W, R](self: Outer[S, W, R]): Annotation[Inner[S, W, R]] = unwrap(
      self
    )

    final def node[S[-w, +r] <: Bound[w, r], W, R](self: Outer[S, W, R]): Inner[S, W, R] = unwrap(self).self

    final def apply[S[-w, +r] <: Bound[w, r], W, R](self: Inner[S, W, R]): Outer[S, W, R] = wrap(Annotation(self))

    given annotated: [S[-w, +r] <: Bound[w, r], W, R] => Annotated[Outer[S, W, R]]:
      extension (self: Outer[S, W, R])
        override def lens: (Metadata, Metadata => Outer[S, W, R]) =
          val annotation = unwrap(self)
          (annotation.metadata, metadata => wrap(annotation.copy(metadata = metadata)))

    given profunctor: [S[-w, +r] <: Bound[w, r]]
      => (P: Profunctor[[w, r] =>> Inner[S, w, r]]) => Profunctor[[w, r] =>> Outer[S, w, r]]:
      override def dimap[W0, R0, W, R](self: Outer[S, W0, R0])(f: W => W0)(g: R0 => R): Outer[S, W, R] =
        wrap(unwrap(self).map(P.dimap(_)(f)(g)))

    given zip: [S[-w, +r] <: Bound[w, r]] => (Z: Zip[[w, r] =>> Inner[S, w, r]]) => Zip[[w, r] =>> Outer[S, w, r]]:
      override def zip[W1, R1, W2, R2](
          left: Outer[S, W1, R1],
          right: Outer[S, W2, R2]
      ): Outer[S, (W1, W2), (R1, R2)] = wrap(Annotation(Z.zip(unwrap(left).self, unwrap(right).self)))

    given alt: [S[-w, +r] <: Bound[w, r]] => (A: Alt[[w, r] =>> Inner[S, w, r]]) => Alt[[w, r] =>> Outer[S, w, r]]:
      override def alt[W1, R1, W2, R2](
          left: Outer[S, W1, R1],
          right: Outer[S, W2, R2]
      ): Outer[S, Either[W1, W2], Either[R1, R2]] = wrap(Annotation(A.alt(unwrap(left).self, unwrap(right).self)))

  abstract class Field[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Field[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Field[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Field[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => FieldOperation[[w, r] =>> Outer[S, w, r], S]:
      override def lift[W, R](name: String, schema: Reference[S, W, R]): Outer[S, W, R] =
        Field.this.apply[S, W, R](Self.Field.Root(name, schema))

      extension [W, R](fa: Outer[S, W, R])
        override def name: String = node(fa).name
        override def isOptional: Boolean = node(fa).isOptional
        override def optional: Outer[S, Option[W], Option[R]] =
          Field.this.apply[S, Option[W], Option[R]](Self.Field.Optional(node(fa)))
        override def optional(default: => R): Outer[S, W, R] =
          Field.this.apply[S, W, R](Self.Field.Default(node(fa), Eval.later(default)))
        override def schema: Reference[S, ?, ?] = node(fa).schema

  abstract class Record[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_], G[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Record[[a, b] =>> G[s, a, b], w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Record[[a, b] =>> G[s, a, b], w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Record[[a, b] =>> G[s, a, b], w, r]](
        wrap,
        unwrap
      ):
    given operation: [S[-w, +r] <: Bound[w, r]] => RecordOperation[[w, r] =>> Outer[S, w, r], [w, r] =>> G[S, w, r]]:
      override def empty: Outer[S, Unit, Unit] = Record.this.apply[S, Unit, Unit](Self.Record.Empty)

      override def lift[W, R](field: Reference[[w, r] =>> G[S, w, r], W, R]): Outer[S, W, R] =
        Record.this.apply[S, W, R](Self.Record.Root(field))

      extension [W, R](fa: Outer[S, W, R])
        override def fields: Chain[Reference[[w, r] =>> G[S, w, r], ?, ?]] = node(fa).fields

  abstract class Branch[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Branch[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Branch[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Branch[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => BranchOperation[[w, r] =>> Outer[S, w, r], S]:
      override def lift[W, R](name: String, schema: Reference[S, W, R]): Outer[S, W, R] =
        Branch.this.apply[S, W, R](Self.Branch.Root(name, schema))

      extension [W, R](fa: Outer[S, W, R])
        override def name: String = node(fa).name
        override def schema: Reference[S, ?, ?] = node(fa).schema

  abstract class Union[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_], G[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Union[[a, b] =>> G[s, a, b], w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Union[[a, b] =>> G[s, a, b], w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Union[[a, b] =>> G[s, a, b], w, r]](
        wrap,
        unwrap
      ):
    given operation: [S[-w, +r] <: Bound[w, r]] => UnionOperation[[w, r] =>> Outer[S, w, r], [w, r] =>> G[S, w, r]]:
      override def lift[W, R](branch: Reference[[w, r] =>> G[S, w, r], W, R]): Outer[S, W, R] =
        Union.this.apply[S, W, R](Self.Union.Root(branch))

      extension [W, R](fa: Outer[S, W, R])
        override def branches: NonEmptyChain[Reference[[w, r] =>> G[S, w, r], ?, ?]] = node(fa).branches

  abstract class Collection[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Collection[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Collection[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Collection[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => CollectionOperation[[w, r] =>> Outer[S, w, r], S]:
      override def chained[W, R](
          schema: Reference[S, W, R],
          validation: Validation[Constraint.Collection, Chain[R]]
      ): Outer[S, Chain[W], Chain[R]] =
        Collection.this.apply[S, Chain[W], Chain[R]](Self.Collection.Chained(schema, validation))

      override def indexed[W, R](
          schema: Reference[S, W, R],
          validation: Validation[Constraint.Collection, Vector[R]]
      ): Outer[S, Vector[W], Vector[R]] =
        Collection.this.apply[S, Vector[W], Vector[R]](Self.Collection.Indexed(schema, validation))

      override def linked[W, R](
          schema: Reference[S, W, R],
          validation: Validation[Constraint.Collection, List[R]]
      ): Outer[S, List[W], List[R]] =
        Collection.this.apply[S, List[W], List[R]](Self.Collection.Linked(schema, validation))

      extension [W, R](fa: Outer[S, W, R]) override def schema: Reference[S, ?, ?] = node(fa).schema

  abstract class Dictionary[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Dictionary[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Dictionary[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Dictionary[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => DictionaryOperation[[w, r] =>> Outer[S, w, r], S]:
      override def hashed[W, R](
          schema: Reference[S, W, R],
          validation: Validation[Constraint.Object, SortedMap[String, R]]
      ): Outer[S, SortedMap[String, W], SortedMap[String, R]] =
        Dictionary.this.apply[S, SortedMap[String, W], SortedMap[String, R]](
          Self.Dictionary.Hashed(schema, validation)
        )

      override def linked[W, R](
          schema: Reference[S, W, R],
          validation: Validation[Constraint.Object, List[(String, R)]]
      ): Outer[S, List[(String, W)], List[(String, R)]] =
        Dictionary.this.apply[S, List[(String, W)], List[(String, R)]](Self.Dictionary.Linked(schema, validation))

      extension [W, R](fa: Outer[S, W, R]) override def schema: Reference[S, ?, ?] = node(fa).schema

  abstract class Optional[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Optional[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Optional[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Optional[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => OptionalOperation[[w, r] =>> Outer[S, w, r], S]:
      override def lift[W, R](schema: => Reference[S, W, R]): Outer[S, Option[W], Option[R]] =
        Optional.this.apply[S, Option[W], Option[R]](Self.Optional.Root(schema))

      override def lift[W, R](schema: => Reference[S, W, R], default: => R): Outer[S, W, R] =
        Optional.this.apply[S, W, R](Self.Optional.Default(schema, Eval.later(default)))

      extension [W, R](fa: Outer[S, W, R]) override def schema: Reference[S, ?, ?] = node(fa).schema

  abstract class Tuple[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Tuple[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Tuple[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Tuple[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => TupleOperation[[w, r] =>> Outer[S, w, r], S]:
      override def empty: Outer[S, Unit, Unit] = Tuple.this.apply[S, Unit, Unit](Self.Tuple.Empty)

      override def lift[W, R](schema: Reference[S, W, R]): Outer[S, W, R] =
        Tuple.this.apply[S, W, R](Self.Tuple.Root(schema))

      extension [W, R](fa: Outer[S, W, R]) override def schemas: Chain[Reference[S, ?, ?]] = node(fa).schemas

  abstract class Enumeration[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Enumeration[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Enumeration[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Enumeration[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => EnumerationOperation[[w, r] =>> Outer[S, w, r], S]:
      override def lift[A, B](schema: Reference[S, A, A], mapping: Mapping[B, A]): Outer[S, B, B] =
        Enumeration.this.apply[S, B, B](Self.Enumeration.Root(schema, mapping))

      extension [W, R](fa: Outer[S, W, R]) override def schema: Reference[S, ?, ?] = node(fa).schema

  abstract class Coerce[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Coerce[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Coerce[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Coerce[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => CoerceOperation[[w, r] =>> Outer[S, w, r], S]:
      override def lift[W, R](schema: Reference[S, W, R]): Outer[S, W, R] =
        Coerce.this.apply[S, W, R](Self.Coerce.Root(schema))

      extension [W, R](fa: Outer[S, W, R]) override def schema: Reference[S, ?, ?] = node(fa).schema

  abstract class Constant[Bound[-_, +_], Outer[_[-w, +r] <: Bound[w, r], -_, +_]](
      wrap: [s[-w, +r] <: Bound[w, r], w, r] => Annotation[Self.Constant[s, w, r]] => Outer[s, w, r],
      unwrap: [s[-w, +r] <: Bound[w, r], w, r] => Outer[s, w, r] => Annotation[Self.Constant[s, w, r]]
  ) extends Wrapper.Nested[Bound, Outer, [s[-w, +r] <: Bound[w, r], w, r] =>> Self.Constant[s, w, r]](wrap, unwrap):
    given operation: [S[-w, +r] <: Bound[w, r]] => ConstantOperation[[w, r] =>> Outer[S, w, r], S]:
      override def lift[A](schema: Reference[S, A, A], value: Eval[A], eq: Eq[A]): Outer[S, Unit, Unit] =
        Constant.this.apply[S, Unit, Unit](Self.Constant.Root(schema, value, eq))

      extension [W, R](fa: Outer[S, W, R]) override def schema: Reference[S, ?, ?] = node(fa).schema

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
