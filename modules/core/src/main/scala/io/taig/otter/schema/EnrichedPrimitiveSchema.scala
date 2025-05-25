package io.taig.otter.schema

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import io.taig.otter.Comparison
import io.taig.otter.Metadata
import io.taig.otter.Enrichment

trait EnrichedPrimitiveSchema[Self[_]]
    extends PrimitiveSchema[Self],
      EnrichedPrimitiveSchema.Boolean[Self],
      EnrichedPrimitiveSchema.Number[Self],
      EnrichedPrimitiveSchema.String[Self],
      EnrichedSchema[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedPrimitiveSchema[T] =
    new EnrichedPrimitiveSchema[T]:
      override def boolean: T[Boolean] = fK(self.boolean)
      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): T[JBigDecimal] =
        fK(self.jBigDecimal(minimum, maximum, multiple))
      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): T[JBigInteger] =
        fK(self.jBigInteger(minimum, maximum, multiple))
      override def double(
          minimum: Option[Comparison[Double]],
          maximum: Option[Comparison[Double]],
          multiple: Option[Double]
      ): T[Double] =
        fK(self.double(minimum, maximum, multiple))
      override def float(
          minimum: Option[Comparison[Float]],
          maximum: Option[Comparison[Float]],
          multiple: Option[Float]
      ): T[Float] =
        fK(self.float(minimum, maximum, multiple))
      override def int(
          minimum: Option[Comparison[Int]],
          maximum: Option[Comparison[Int]],
          multiple: Option[Int]
      ): T[Int] =
        fK(self.int(minimum, maximum, multiple))
      override def long(
          minimum: Option[Comparison[Long]],
          maximum: Option[Comparison[Long]],
          multiple: Option[Long]
      ): T[Long] =
        fK(self.long(minimum, maximum, multiple))
      override def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): T[String] =
        fK(self.string(minimum, maximum, matches))
      override def parser[A](
          name: String,
          decode: String => Either[String, A],
          encode: A => String,
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      ): T[A] =
        fK(self.parser(name, decode, encode, minimum, maximum, matches))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedPrimitiveSchema:
  trait Boolean[Self[_]] extends PrimitiveSchema.Boolean[Self], EnrichedSchema[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): EnrichedPrimitiveSchema.Boolean[T] =
      new EnrichedPrimitiveSchema.Boolean[T]:
        override def boolean: T[SBoolean] = fK(self.boolean)

        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

        override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

  object Boolean:
    inline def apply[Self[_]](using
        schema: EnrichedPrimitiveSchema.Boolean[Self]
    ): EnrichedPrimitiveSchema.Boolean[Self] = schema

    given [Self[_]](using
        self: PrimitiveSchema.Boolean[Self],
        enrichment: EnrichedSchema[Enrichment[Self, *]]
    ): EnrichedPrimitiveSchema.Boolean[Enrichment[Self, *]] =
      val schema: PrimitiveSchema.Boolean[Enrichment[Self, *]] =
        self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

      new EnrichedPrimitiveSchema.Boolean[Enrichment[Self, *]]:
        export schema.boolean
        export enrichment.{imap, metadata}

  trait Number[Self[_]] extends PrimitiveSchema.Number[Self], EnrichedSchema[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): EnrichedPrimitiveSchema.Number[T] =
      new EnrichedPrimitiveSchema.Number[T]:
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
        schema: EnrichedPrimitiveSchema.Number[Self]
    ): EnrichedPrimitiveSchema.Number[Self] = schema

    given [Self[_]](using
        self: PrimitiveSchema.Number[Self],
        enrichment: EnrichedSchema[Enrichment[Self, *]]
    ): EnrichedPrimitiveSchema.Number[Enrichment[Self, *]] =
      val schema: PrimitiveSchema.Number[Enrichment[Self, *]] =
        self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

      new EnrichedPrimitiveSchema.Number[Enrichment[Self, *]]:
        export schema.{double, float, int, jBigDecimal, jBigInteger, long}
        export enrichment.{imap, metadata}

  trait String[Self[_]] extends PrimitiveSchema.String[Self], EnrichedSchema[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): EnrichedPrimitiveSchema.String[T] =
      new EnrichedPrimitiveSchema.String[T]:
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
        schema: EnrichedPrimitiveSchema.String[Self]
    ): EnrichedPrimitiveSchema.String[Self] = schema

    given [Self[_]](using
        self: PrimitiveSchema.String[Self],
        enrichment: EnrichedSchema[Enrichment[Self, *]]
    ): EnrichedPrimitiveSchema.String[Enrichment[Self, *]] =
      val schema: PrimitiveSchema.String[Enrichment[Self, *]] =
        self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

      new EnrichedPrimitiveSchema.String[Enrichment[Self, *]]:
        export schema.{parser, string}
        export enrichment.{imap, metadata}

  inline def apply[Self[_]](using
      schema: EnrichedPrimitiveSchema[Self]
  ): EnrichedPrimitiveSchema[Self] = schema

  given [Self[_]](using
      self: PrimitiveSchema[Self],
      enrichment: EnrichedSchema[Enrichment[Self, *]]
  ): EnrichedPrimitiveSchema[Enrichment[Self, *]] =
    val primitive: PrimitiveSchema[Enrichment[Self, *]] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedPrimitiveSchema[Enrichment[Self, *]]:
      export primitive.{boolean, double, float, int, jBigDecimal, jBigInteger, long, parser, string}
      export enrichment.{imap, metadata}
