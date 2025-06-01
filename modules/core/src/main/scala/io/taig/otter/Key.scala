package io.taig.otter

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.operation.ConstantSchemaInvariant
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.EnumerationSchemaInvariant
import io.taig.otter.operation.PrimitiveSchemaInvariant
import io.taig.otter.operation.SchemaInvariant
import io.taig.otter.operation.UnionSchemaInvariant

sealed abstract class Key[A] extends Product, Serializable

object Key:
  final case class Constant[A](self: Self.Constant[Key.Primitive, A]) extends Key[A]

  object Constant:
    given ConstantSchemaInvariant[Key.Constant, Key.Primitive] =
      ConstantSchemaInvariant[Self.Constant[Key.Primitive, *], Key.Primitive]
        .imapK(
          [A] => (schema: Self.Constant[Key.Primitive, A]) => Constant(schema)
        )([A] => (key: Key.Constant[A]) => key.self)

  final case class Enumeration[A](self: Self.Enumeration[Key.Primitive, A]) extends Key[A]

  object Enumeration:
    given EnumerationSchemaInvariant[Key.Enumeration, Key.Primitive] =
      EnumerationSchemaInvariant[Self.Enumeration[Key.Primitive, *], Key.Primitive]
        .imapK(
          [A] => (schema: Self.Enumeration[Key.Primitive, A]) => Enumeration(schema)
        )([A] => (key: Key.Enumeration[A]) => key.self)

  final case class Primitive[A](self: Self.Primitive.String[A]) extends Key[A]

  object Primitive:
    given PrimitiveSchemaInvariant.String[Key.Primitive] = PrimitiveSchemaInvariant
      .String[Self.Primitive.String]
      .imapK(
        [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
      )([A] => (key: Key.Primitive[A]) => key.self)

  final case class Union[A](self: Self.Union[Key, A]) extends Key[A]

  object Union:
    given UnionSchemaInvariant[Key.Union, Key] = UnionSchemaInvariant[Self.Union[Key, *], Key]
      .imapK(
        [A] => (schema: Self.Union[Key, A]) => Union(schema)
      )([A] => (key: Key.Union[A]) => key.self)

  given SchemaInvariant[Key] with
    override def enriched[A]: Enriched[Key[A]] = new Enriched[Key[A]]:
      override def metadata(a: Key[A]): Metadata = a match
        case Key.Constant(self)    => self.metadata
        case Key.Enumeration(self) => self.metadata
        case Key.Primitive(self)   => self.metadata
        case Key.Union(self)       => self.metadata

      override def modifyMetadata(a: Key[A])(f: Metadata => Metadata): Key[A] = a match
        case Key.Constant(self)    => Key.Constant(self.metadata(f))
        case Key.Enumeration(self) => Key.Enumeration(self.metadata(f))
        case Key.Primitive(self)   => Key.Primitive(self.metadata(f))
        case Key.Union(self)       => Key.Union(self.metadata(f))

    override def imap[A, B](fa: Key[A])(f: A => B)(g: B => A): Key[B] = fa match
      case Key.Constant(self)    => Key.Constant(self.imap(f)(g))
      case Key.Enumeration(self) => Key.Enumeration(self.imap(f)(g))
      case Key.Primitive(self)   => Key.Primitive(self.imap(f)(g))
      case Key.Union(self)       => Key.Union(self.imap(f)(g))
