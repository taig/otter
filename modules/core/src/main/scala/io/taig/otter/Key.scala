package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.schema.ConstantSchema
import io.taig.otter.schema.EnumerationSchema
import io.taig.otter.schema.PrimitiveSchema
import io.taig.otter.schema.Schema
import io.taig.otter.schema.UnionSchema

sealed abstract class Key[A] extends Product with Serializable

object Key:
  final case class Constant[A](self: Self.Constant[Key.Primitive, A]) extends Key[A]

  object Constant:
    given ConstantSchema[Key.Constant, Key.Primitive] =
      ConstantSchema[Self.Constant[Key.Primitive, *], Key.Primitive]
        .imapK(
          [A] => (schema: Self.Constant[Key.Primitive, A]) => Constant(schema)
        )([A] => (key: Key.Constant[A]) => key.self)

  final case class Enumeration[A](self: Self.Enumeration[Key.Primitive, A]) extends Key[A]

  object Enumeration:
    given EnumerationSchema[Key.Enumeration, Key.Primitive] =
      EnumerationSchema[Self.Enumeration[Key.Primitive, *], Key.Primitive]
        .imapK(
          [A] => (schema: Self.Enumeration[Key.Primitive, A]) => Enumeration(schema)
        )([A] => (key: Key.Enumeration[A]) => key.self)

  final case class Primitive[A](self: Self.Primitive.String[A]) extends Key[A]

  object Primitive:
    given PrimitiveSchema.String[Key.Primitive] = PrimitiveSchema
      .String[Self.Primitive.String]
      .imapK(
        [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
      )([A] => (key: Key.Primitive[A]) => key.self)

  final case class Union[A](self: Self.Union[Key, A]) extends Key[A]

  object Union:
    given UnionSchema[Key.Union, Key] = UnionSchema[Self.Union[Key, *], Key]
      .imapK(
        [A] => (schema: Self.Union[Key, A]) => Union(schema)
      )([A] => (key: Key.Union[A]) => key.self)

  given Schema[Key] with
    override def imap[A, B](fa: Key[A])(f: A => B)(g: B => A): Key[B] = fa match
      case Constant(self)    => Constant(self.imap(f)(g))
      case Enumeration(self) => Enumeration(self.imap(f)(g))
      case Primitive(self)   => Primitive(self.imap(f)(g))
      case Union(self)       => Union(self.imap(f)(g))

    extension [A](self: Key[A])
      override def metadata: Metadata = self match
        case Constant(self)    => self.metadata
        case Enumeration(self) => self.metadata
        case Primitive(self)   => self.metadata
        case Union(self)       => self.metadata

      override def modifyMetadata(f: Metadata => Metadata): Key[A] = self match
        case Constant(self)    => Constant(self.modifyMetadata(f))
        case Enumeration(self) => Enumeration(self.modifyMetadata(f))
        case Primitive(self)   => Primitive(self.modifyMetadata(f))
        case Union(self)       => Union(self.modifyMetadata(f))
