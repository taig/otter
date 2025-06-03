package io.taig.otter

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.Enriched
import Self.operation.PrimitiveSchemaInvariant
import Self.operation.SchemaInvariant
import java.util.regex.Pattern
import java.math.BigInteger

sealed trait Key[A] extends Key.Any[A]

object Key:
  sealed trait Any[A] extends Product, Serializable

  sealed trait Primitive[A] extends Key.Any[A]:
    def self: Self.Primitive[Key.Primitive, A]

  object Primitive:
    final case class Boolean[A](self: Self.Primitive.Boolean[A]) extends Key.Primitive[A]

    object Boolean:
      given schema: PrimitiveSchemaInvariant.Boolean[Key.Primitive.Boolean] = PrimitiveSchemaInvariant
        .Boolean[Self.Primitive.Boolean]
        .imapK(
          [A] => (self: Self.Primitive.Boolean[A]) => Boolean(self)
        )([A] => (key: Key.Primitive.Boolean[A]) => key.self)

    final case class Number[A](self: Self.Primitive.Number[A]) extends Key.Primitive[A]

    object Number:
      given schema: PrimitiveSchemaInvariant.Number[Key.Primitive.Number] = PrimitiveSchemaInvariant
        .Number[Self.Primitive.Number]
        .imapK(
          [A] => (self: Self.Primitive.Number[A]) => Number(self)
        )([A] => (key: Key.Primitive.Number[A]) => key.self)

    final case class String[A](self: Self.Primitive.String[Key.Primitive, A]) extends Key.Primitive[A], Key[A]

    object String:
      given schema: PrimitiveSchemaInvariant.String[Key.Primitive.String, Key.Primitive] = PrimitiveSchemaInvariant
        .String[Self.Primitive.String[Key.Primitive, *], Key.Primitive]
        .imapK(
          [A] => (schema: Self.Primitive.String[Key.Primitive, A]) => String(schema)
        )([A] => (key: Key.Primitive.String[A]) => key.self)

    given PrimitiveSchemaInvariant[Key.Primitive, Key.Primitive]
      with SchemaInvariant.Parseable[Key.Primitive, Key.Primitive.String]
      with
      export Boolean.schema.boolean
      export Number.schema.{double, float, int, jBigDecimal, jBigInteger, long}
      export String.schema.{parsed, parser, string}

      override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa match
        case Primitive.Boolean(self) => Primitive.Boolean(self.imap(f)(g))
        case Primitive.Number(self)  => Primitive.Number(self.imap(f)(g))
        case Primitive.String(self)  => Primitive.String(self.imap(f)(g))

      override def enriched[A]: Enriched[Primitive[A]] = new Enriched[Primitive[A]]:
        override def metadata(a: Primitive[A]): Metadata = a match
          case Primitive.Boolean(self) => self.metadata
          case Primitive.Number(self)  => self.metadata
          case Primitive.String(self)  => self.metadata

        override def modifyMetadata(a: Primitive[A])(f: Metadata => Metadata): Primitive[A] = a match
          case Primitive.Boolean(self) => Primitive.Boolean(self.metadata(f))
          case Primitive.Number(self)  => Primitive.Number(self.metadata(f))
          case Primitive.String(self)  => Primitive.String(self.metadata(f))

  final case class Constant[A](self: Self.Constant[Key.Primitive.String, A]) extends Key[A]

  // object Constant:
  //   given ConstantSchemaInvariant[Key.Constant, Key.Primitive.String] =
  //     ConstantSchemaInvariant[Self.Constant[Key.Primitive.String, *], Key.Primitive.String]
  //       .imapK(
  //         [A] => (schema: Self.Constant[Key.Primitive.String, A]) => Constant(schema)
  //       )([A] => (key: Key.Constant[A]) => key.self)

  final case class Enumeration[A](self: Self.Enumeration[Key.Primitive.String, A]) extends Key[A]

  // object Enumeration:
  //   given EnumerationSchemaInvariant[Key.Enumeration, Key.Primitive.String] =
  //     EnumerationSchemaInvariant[Self.Enumeration[Key.Primitive.String, *], Key.Primitive.String]
  //       .imapK(
  //         [A] => (schema: Self.Enumeration[Key.Primitive.String, A]) => Enumeration(schema)
  //       )([A] => (key: Key.Enumeration[A]) => key.self)

  final case class Union[A](self: Self.Union[Key, A]) extends Key[A]

  // object Union:
  //   given UnionSchemaInvariant[Key.Union, Key] = UnionSchemaInvariant[Self.Union[Key, *], Key]
  //     .imapK(
  //       [A] => (schema: Self.Union[Key, A]) => Union(schema)
  //     )([A] => (key: Key.Union[A]) => key.self)

  // given SchemaInvariant[Key] with
  //   override def enriched[A]: Enriched[Key[A]] = new Enriched[Key[A]]:
  //     override def metadata(a: Key[A]): Metadata = a match
  //       case Key.Primitive.String(self) => self.metadata
  //       case Key.Constant(self)         => self.metadata
  //       case Key.Enumeration(self)      => self.metadata
  //       case Key.Union(self)            => self.metadata

  //     override def modifyMetadata(a: Key[A])(f: Metadata => Metadata): Key[A] = a match
  //       case Key.Primitive.String(self) => Key.Primitive.String(self.metadata(f))
  //       case Key.Constant(self)         => Key.Constant(self.metadata(f))
  //       case Key.Enumeration(self)      => Key.Enumeration(self.metadata(f))
  //       case Key.Union(self)            => Key.Union(self.metadata(f))

  //   override def imap[A, B](fa: Key[A])(f: A => B)(g: B => A): Key[B] = fa match
  //     case Key.Primitive.String(self) => Key.Primitive.String(self.imap(f)(g))
  //     case Key.Constant(self)         => Key.Constant(self.imap(f)(g))
  //     case Key.Enumeration(self)      => Key.Enumeration(self.imap(f)(g))
  //     case Key.Union(self)            => Key.Union(self.imap(f)(g))
