package io.taig.otter

import cats.Eq
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Metadata

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait Schema[Self[_]] extends Invariant[Self]:
  self =>

  def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema[T] = new Schema[T]:
    extension [A](ta: T[A])
      override def metadata: Metadata = self.metadata(gK(ta))
      override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
      override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

  extension [A](self: Self[A])
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Self[A]

    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = modifyMetadata(_.put(key, value))

object Schema:
  trait Branch[Self[_], Key[_], Value[_]] extends Schema[Self]:
    self =>

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): Schema.Branch[T, Key, Value] = new Branch[T, Key, Value]:
      override def branch[A, B](name: A, key: => Key[A], value: => Value[B]): T[B] =
        fK(self.branch(name, key, value))

      extension [A](ta: T[A])
        override def key: Reference.Constant[Key, ?] = self.key(gK(ta))
        override def value: Reference[Value, ?] = self.value(gK(ta))
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

    extension [A](self: Self[A])
      def key: Reference.Constant[Key, ?]
      def value: Reference[Value, ?]

  object Branch:
    inline def apply[Self[_], Key[_], Value[_]](using
        self: Schema.Branch[Self, Key, Value]
    ): Schema.Branch[Self, Key, Value] = self

  trait Collection[Self[_], -Value[_]] extends Schema[Self]:
    self =>

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): Schema.Collection[T, Value] =
      new Collection[T, Value]:
        override def linked[A](
            schema: => Value[A],
            minimum: Option[Int],
            maximum: Option[Int],
            uniqueItems: Boolean
        ): T[List[A]] = fK(self.linked(schema, minimum, maximum, uniqueItems))

        override def indexed[A](
            schema: => Value[A],
            minimum: Option[Int],
            maximum: Option[Int],
            uniqueItems: Boolean
        ): T[Vector[A]] = fK(self.indexed(schema, minimum, maximum, uniqueItems))

        extension [A](ta: T[A])
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

    def linked[A](schema: => Value[A], minimum: Option[Int], maximum: Option[Int], uniqueItems: Boolean): Self[List[A]]

    def indexed[A](
        schema: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        uniqueItems: Boolean
    ): Self[Vector[A]]

  object Collection:
    inline def apply[Self[_], Value[_]](using self: Schema.Collection[Self, Value]): Schema.Collection[Self, Value] =
      self

  trait Constant[Self[_], Value[_]] extends Schema[Self]:
    self =>

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema.Constant[T, Value] =
      new Constant[T, Value]:
        override def constant[A](schema: => Value[A], value: A)(using Eq[A]): T[Unit] = fK(self.constant(schema, value))

        extension [A](fa: T[A])
          override def metadata: Metadata = self.metadata(gK(fa))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

    def constant[A: Eq](schema: => Value[A], value: A): Self[Unit]

  object Constant:
    inline def apply[Self[_], Value[_]](using self: Schema.Constant[Self, Value]): Schema.Constant[Self, Value] = self

  trait Dictionary[Self[_], -Key[_], -Value[_]] extends Schema[Self]:
    self =>

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): Schema.Dictionary[T, Key, Value] = new Dictionary[T, Key, Value]:
      override def dictionary[A, B](
          key: => Key[A],
          value: => Value[B],
          minimum: Option[SInt],
          maximum: Option[SInt]
      ): T[List[(A, B)]] =
        fK(self.dictionary(key, value, minimum, maximum))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    def dictionary[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int],
        maximum: Option[Int]
    ): Self[List[(A, B)]]

  object Dictionary:
    inline def apply[Self[_], Key[_], Value[_]](using
        self: Schema.Dictionary[Self, Key, Value]
    ): Schema.Dictionary[Self, Key, Value] = self

  trait Enumeration[Self[_], Value[_]] extends Schema[Self]:
    self =>

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): Schema.Enumeration[T, Value] =
      new Enumeration[T, Value]:
        override def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): T[B] = fK(
          self.enumeration(schema, mapping)
        )

        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

    def enumeration[A, B](schema: => Value[A], mapping: Mapping[B, A]): Self[B]

  object Enumeration:
    inline def apply[Self[_], Value[_]](using self: Schema.Enumeration[Self, Value]): Schema.Enumeration[Self, Value] =
      self

  // trait Field[Self[_], -Key[_], -Value[_]] extends Schema[Self]:
  //   self =>

  //   final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
  //       gK: [A] => T[A] => Self[A]
  //   ): Schema.Field[T, Key, Value] = new Field[T, Key, Value]:
  //       extension [A](ta: T[A])
  //         override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
  //         override def metadata: Metadata = self.metadata(gK(ta))
  //         override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
  //         override def optional: T[Option[A]] = fK(self.optional(gK(ta)))

  //       override def field[A, B](name: A, key: => Key[A], value: => Value[B]): T[B] =
  //         fK(self.field(name, key, value))

  //   def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

  //   extension [A](self: Self[A]) def optional: Self[Option[A]]

  // object Field:
  //   inline def apply[Self[_], Key[_], Value[_]](using
  //       self: Schema.Field[Self, Key, Value]
  //   ): Schema.Field[Self, Key, Value] = self

  trait Primitive[Self[_]]
      extends Schema.Primitive.Boolean[Self],
        Schema.Primitive.Number[Self],
        Schema.Primitive.String[Self]:
    self =>

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema.Primitive[T] =
      new Primitive[T]:
        extension [A](fa: T[A])
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
          override def metadata: Metadata = self.metadata(gK(fa))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))

        override def boolean: T[SBoolean] = fK(self.boolean)

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
            minimum: Option[Comparison[SDouble]],
            maximum: Option[Comparison[SDouble]],
            multiple: Option[SDouble]
        ): T[SDouble] = fK(self.double(minimum, maximum, multiple))

        override def float(
            minimum: Option[Comparison[SFloat]],
            maximum: Option[Comparison[SFloat]],
            multiple: Option[SFloat]
        ): T[SFloat] = fK(self.float(minimum, maximum, multiple))

        override def int(
            minimum: Option[Comparison[SInt]],
            maximum: Option[Comparison[SInt]],
            multiple: Option[SInt]
        ): T[SInt] = fK(self.int(minimum, maximum, multiple))

        override def long(
            minimum: Option[Comparison[SLong]],
            maximum: Option[Comparison[SLong]],
            multiple: Option[SLong]
        ): T[SLong] = fK(self.long(minimum, maximum, multiple))

        override def string(minimum: Option[SInt], maximum: Option[SInt], matches: Option[Pattern]): T[JString] =
          fK(self.string(minimum, maximum, matches))

        override def parser[A](
            name: JString,
            decode: JString => Either[JString, A],
            encode: A => JString,
            minimum: Option[SInt],
            maximum: Option[SInt],
            matches: Option[Pattern]
        ): T[A] = fK(self.parser(name, decode, encode, minimum, maximum, matches))

  trait Nullable[Self[_], Value[_]] extends Schema[Self]:
    self =>

    def nullable[A](schema: => Value[A]): Self[Option[A]]
    def nullable[A](schema: => Value[A], default: A): Self[A]
    def void: Self[Unit]

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema.Nullable[T, Value] =
      new Nullable[T, Value]:
        override def nullable[A](schema: => Value[A]): T[Option[A]] = fK(self.nullable(schema))
        override def nullable[A](schema: => Value[A], default: A): T[A] = fK(self.nullable(schema, default))
        override def void: T[Unit] = fK(self.void)

        extension [A](ta: T[A])
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

  object Nullable:
    inline def apply[Self[_], Value[_]](using self: Schema.Nullable[Self, Value]): Schema.Nullable[Self, Value] = self

  object Primitive:
    trait Boolean[Self[_]] extends Schema[Self]:
      self =>

      override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
          gK: [A] => T[A] => Self[A]
      ): Schema.Primitive.Boolean[T] = new Boolean[T]:
        override def boolean: T[SBoolean] = fK(self.boolean)

        extension [A](fa: T[A])
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
          override def metadata: Metadata = self.metadata(gK(fa))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))

      def boolean: Self[SBoolean]

    object Boolean:
      inline def apply[Self[_]](using self: Schema.Primitive.Boolean[Self]): Schema.Primitive.Boolean[Self] = self

    trait Number[Self[_]] extends Schema[Self]:
      self =>

      override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
          gK: [A] => T[A] => Self[A]
      ): Schema.Primitive.Number[T] = new Number[T]:
        extension [A](ta: T[A])
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

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
            minimum: Option[Comparison[SDouble]],
            maximum: Option[Comparison[SDouble]],
            multiple: Option[SDouble]
        ): T[SDouble] =
          fK(self.double(minimum, maximum, multiple))

        override def float(
            minimum: Option[Comparison[SFloat]],
            maximum: Option[Comparison[SFloat]],
            multiple: Option[SFloat]
        ): T[SFloat] =
          fK(self.float(minimum, maximum, multiple))

        override def int(
            minimum: Option[Comparison[SInt]],
            maximum: Option[Comparison[SInt]],
            multiple: Option[SInt]
        ): T[SInt] =
          fK(self.int(minimum, maximum, multiple))

        override def long(
            minimum: Option[Comparison[SLong]],
            maximum: Option[Comparison[SLong]],
            multiple: Option[SLong]
        ): T[SLong] =
          fK(self.long(minimum, maximum, multiple))

      def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Self[JBigDecimal]

      def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Self[JBigInteger]

      def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Self[SDouble]

      def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Self[SFloat]

      def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Self[SInt]

      def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Self[SLong]

    object Number:
      inline def apply[Self[_]](using self: Schema.Primitive.Number[Self]): Schema.Primitive.Number[Self] = self

    trait String[Self[_]] extends Schema[Self]:
      self =>

      override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
          gK: [A] => T[A] => Self[A]
      ): Schema.Primitive.String[T] =
        new String[T]:
          override def string(
              minimum: Option[SInt],
              maximum: Option[SInt],
              matches: Option[Pattern]
          ): T[JString] = fK(self.string(minimum, maximum, matches))

          override def parser[A](
              name: JString,
              decode: JString => Either[JString, A],
              encode: A => JString,
              minimum: Option[SInt],
              maximum: Option[SInt],
              matches: Option[Pattern]
          ): T[A] =
            fK(self.parser(name, decode, encode, minimum, maximum, matches))

          extension [A](ta: T[A])
            override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
            override def metadata: Metadata = self.metadata(gK(ta))
            override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

      def string(minimum: Option[SInt], maximum: Option[SInt], matches: Option[Pattern]): Self[JString]

      def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): Self[A]

    object String:
      inline def apply[Self[_]](using self: Schema.Primitive.String[Self]): Schema.Primitive.String[Self] = self

    inline def apply[Self[_]](using self: Schema.Primitive[Self]): Schema.Primitive[Self] = self

  trait Record[Self[_], Key[_], Value[_]] extends Schema[Self], Invariant.Product[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema.Record[T, Key, Value] =
      new Schema.Record[T, Key, Value]:
        override def record[A](field: Field[Key, Value, A]): T[A] = fK(self.record(field))

        extension [A](fa: T[A])
          override def metadata: Metadata = self.metadata(gK(fa))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
          override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(fa))(gK(schema)))
          override def optional: T[Option[A]] = fK(self.optional(gK(fa)))

    def record[A](field: Field[Key, Value, A]): Self[A]

    extension [A](self: Self[A]) def optional: Self[Option[A]]

  object Record:
    inline def apply[Self[_], Key[_], Value[_]](using
        self: Schema.Record[Self, Key, Value]
    ): Schema.Record[Self, Key, Value] = self

  trait Sum[Self[_], Branch[_]] extends Schema[Self], Invariant.Coproduct[Self]:
    self =>

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema.Sum[T, Branch] =
      new Sum[T, Branch]:
        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
          override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

  object Sum:
    inline def apply[Self[_], Branch[_]](using self: Schema.Sum[Self, Branch]): Schema.Sum[Self, Branch] = self

  trait Tuple[Self[_], Value[_]] extends Schema[Self], Invariant.Product[Self]:
    self =>

    def empty: Self[Unit]
    def one[A](codec: => Value[A]): Self[A]

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema.Tuple[T, Value] =
      new Tuple[T, Value]:
        override def empty: T[Unit] = fK(self.empty)
        override def one[A](codec: => Value[A]): T[A] = fK(self.one(codec))

        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
          override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

  object Tuple:
    inline def apply[Self[_], Field[_]](using self: Schema.Tuple[Self, Field]): Schema.Tuple[Self, Field] = self

  trait Union[Self[_], Value[_]] extends Schema[Self]:
    self =>

    def one[A](codec: => Value[A]): Self[A]

    final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema.Union[T, Value] =
      new Union[T, Value]:
        extension [A](ta: T[A])
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

        override def one[A](codec: => Value[A]): T[A] = fK(self.one(codec))

  object Union:
    inline def apply[Self[_], Value[_]](using self: Schema.Union[Self, Value]): Schema.Union[Self, Value] = self
