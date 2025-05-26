package io.taig.otter.operation

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import io.taig.otter.Comparison
import io.taig.otter.Metadata
import io.taig.otter.Enrichment

trait EnrichedPrimitiveSchemaInvariant[Self[_]]
    extends PrimitiveSchemaInvariant[Self],
      EnrichedPrimitiveSchemaInvariant.Boolean[Self],
      EnrichedPrimitiveSchemaInvariant.Number[Self],
      EnrichedPrimitiveSchemaInvariant.String[Self],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedPrimitiveSchemaInvariant[T] =
    new EnrichedPrimitiveSchemaInvariant[T]:
      override def boolean: T[Boolean] = fK(self.boolean)
      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): T[JBigDecimal] = fK(self.jBigDecimal(minimum, maximum, multiple))
      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): T[JBigInteger] = fK(self.jBigInteger(minimum, maximum, multiple))
      override def double(
          minimum: Option[Comparison[Double]],
          maximum: Option[Comparison[Double]],
          multiple: Option[Double]
      ): T[Double] = fK(self.double(minimum, maximum, multiple))
      override def float(
          minimum: Option[Comparison[Float]],
          maximum: Option[Comparison[Float]],
          multiple: Option[Float]
      ): T[Float] = fK(self.float(minimum, maximum, multiple))
      override def int(
          minimum: Option[Comparison[Int]],
          maximum: Option[Comparison[Int]],
          multiple: Option[Int]
      ): T[Int] = fK(self.int(minimum, maximum, multiple))
      override def long(
          minimum: Option[Comparison[Long]],
          maximum: Option[Comparison[Long]],
          multiple: Option[Long]
      ): T[Long] = fK(self.long(minimum, maximum, multiple))
      override def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): T[String] = fK(
        self.string(minimum, maximum, matches)
      )
      override def parser[A](
          name: String,
          decode: String => Either[String, A],
          encode: A => String,
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      ): T[A] = fK(self.parser(name, decode, encode, minimum, maximum, matches))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedPrimitiveSchemaInvariant:
  trait Boolean[Self[_]] extends PrimitiveSchemaInvariant.Boolean[Self], EnrichedSchemaInvariant[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): EnrichedPrimitiveSchemaInvariant.Boolean[T] =
      new EnrichedPrimitiveSchemaInvariant.Boolean[T]:
        override def boolean: T[SBoolean] = fK(self.boolean)

        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

        override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

  object Boolean:
    inline def apply[Self[_]](using
        schema: EnrichedPrimitiveSchemaInvariant.Boolean[Self]
    ): EnrichedPrimitiveSchemaInvariant.Boolean[Self] = schema

    given [Self[_]](using
        self: PrimitiveSchemaInvariant.Boolean[Self],
        enrichment: EnrichedSchemaInvariant[Enrichment[Self, *]]
    ): EnrichedPrimitiveSchemaInvariant.Boolean[Enrichment[Self, *]] =
      val schema: PrimitiveSchemaInvariant.Boolean[Enrichment[Self, *]] =
        self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

      new EnrichedPrimitiveSchemaInvariant.Boolean[Enrichment[Self, *]]:
        export schema.boolean
        export enrichment.{imap, metadata}

  trait Number[Self[_]] extends PrimitiveSchemaInvariant.Number[Self], EnrichedSchemaInvariant[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): EnrichedPrimitiveSchemaInvariant.Number[T] =
      new EnrichedPrimitiveSchemaInvariant.Number[T]:
        override def jBigDecimal(
            minimum: Option[Comparison[JBigDecimal]],
            maximum: Option[Comparison[JBigDecimal]],
            multiple: Option[JBigDecimal]
        ): T[JBigDecimal] = fK(self.jBigDecimal(minimum, maximum, multiple))

        override def jBigInteger(
            minimum: Option[Comparison[JBigInteger]],
            maximum: Option[Comparison[JBigInteger]],
            multiple: Option[JBigInteger]
        ): T[JBigInteger] = fK(self.jBigInteger(minimum, maximum, multiple))

        override def double(
            minimum: Option[Comparison[Double]],
            maximum: Option[Comparison[Double]],
            multiple: Option[Double]
        ): T[Double] = fK(self.double(minimum, maximum, multiple))

        override def float(
            minimum: Option[Comparison[Float]],
            maximum: Option[Comparison[Float]],
            multiple: Option[Float]
        ): T[Float] = fK(self.float(minimum, maximum, multiple))

        override def int(
            minimum: Option[Comparison[Int]],
            maximum: Option[Comparison[Int]],
            multiple: Option[Int]
        ): T[Int] = fK(self.int(minimum, maximum, multiple))

        override def long(
            minimum: Option[Comparison[Long]],
            maximum: Option[Comparison[Long]],
            multiple: Option[Long]
        ): T[Long] = fK(self.long(minimum, maximum, multiple))

        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

        override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

  object Number:
    inline def apply[Self[_]](using
        schema: EnrichedPrimitiveSchemaInvariant.Number[Self]
    ): EnrichedPrimitiveSchemaInvariant.Number[Self] = schema

    given [Self[_]](using
        self: PrimitiveSchemaInvariant.Number[Self],
        enrichment: EnrichedSchemaInvariant[Enrichment[Self, *]]
    ): EnrichedPrimitiveSchemaInvariant.Number[Enrichment[Self, *]] =
      val schema: PrimitiveSchemaInvariant.Number[Enrichment[Self, *]] =
        self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

      new EnrichedPrimitiveSchemaInvariant.Number[Enrichment[Self, *]]:
        export schema.{double, float, int, jBigDecimal, jBigInteger, long}
        export enrichment.{imap, metadata}

  trait String[Self[_]] extends PrimitiveSchemaInvariant.String[Self], EnrichedSchemaInvariant[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): EnrichedPrimitiveSchemaInvariant.String[T] =
      new EnrichedPrimitiveSchemaInvariant.String[T]:
        override def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): T[JString] =
          fK(self.string(minimum, maximum, matches))

        override def parser[A](
            name: JString,
            decode: JString => Either[JString, A],
            encode: A => JString,
            minimum: Option[Int],
            maximum: Option[Int],
            matches: Option[Pattern]
        ): T[A] = fK(self.parser(name, decode, encode, minimum, maximum, matches))

        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

        override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

  object String:
    inline def apply[Self[_]](using
        schema: EnrichedPrimitiveSchemaInvariant.String[Self]
    ): EnrichedPrimitiveSchemaInvariant.String[Self] = schema

    given [Self[_]](using
        self: PrimitiveSchemaInvariant.String[Self],
        enrichment: EnrichedSchemaInvariant[Enrichment[Self, *]]
    ): EnrichedPrimitiveSchemaInvariant.String[Enrichment[Self, *]] =
      val schema: PrimitiveSchemaInvariant.String[Enrichment[Self, *]] =
        self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

      new EnrichedPrimitiveSchemaInvariant.String[Enrichment[Self, *]]:
        export schema.{parser, string}
        export enrichment.{imap, metadata}

  inline def apply[Self[_]](using
      schema: EnrichedPrimitiveSchemaInvariant[Self]
  ): EnrichedPrimitiveSchemaInvariant[Self] = schema

  given [Self[_]](using
      self: PrimitiveSchemaInvariant[Self],
      enrichment: EnrichedSchemaInvariant[Enrichment[Self, *]]
  ): EnrichedPrimitiveSchemaInvariant[Enrichment[Self, *]] =
    val primitive: PrimitiveSchemaInvariant[Enrichment[Self, *]] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedPrimitiveSchemaInvariant[Enrichment[Self, *]]:
      export primitive.{boolean, double, float, int, jBigDecimal, jBigInteger, long, parser, string}
      export enrichment.{imap, metadata}
