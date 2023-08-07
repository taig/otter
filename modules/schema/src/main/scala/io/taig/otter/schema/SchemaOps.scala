package io.taig.otter.schema

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
