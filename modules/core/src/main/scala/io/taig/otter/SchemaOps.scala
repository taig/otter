package io.taig.otter

// package io.taig.otter.schema

// final class SchemaOps[A](self: Schema[A]) extends AnyVal:
//   inline def :*[B](other: Schema[B]): Product[(A, B)] = other.toProduct.prepend(self)
//   inline def *:[B](other: Schema[B]): Product[(B, A)] = self.toProduct.prepend(other)
//   inline def :*(other: Schema[Unit]): Product[A] = other.toProduct.prepend(self).imap { case (a, _) => a }((_, ()))
//   inline def *:(other: Schema[Unit]): Product[A] = self.toProduct.prepend(other).imap { case (_, a) => a }(((), _))
// final class SchemaOpsUnit(self: Schema[Unit]) extends AnyVal:
//   inline def :*[A](other: Schema[A]): Product[A] = other.toProduct.prepend(self).imap { case (_, a) => a }(((), _))
//   inline def *:[A](other: Schema[A]): Product[A] = self.toProduct.prepend(other).imap { case (a, _) => a }((_, ()))
// final class SchemaOpsTuple[A <: Tuple](self: Schema[A]) extends AnyVal:
//   inline def :*[B](other: Schema[B]): Product[Tuple.Append[A, B]] =
//     other.toProduct
//       .prepend(self)
//       .imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
//   inline def *:[B](other: Schema[B]): Product[B *: A] =
//     self.toProduct.prepend(other).imap { case (b, a) => b *: a } { case b *: a => (b, a) }
//   inline def :*(other: Schema[Unit]): Product[A] = other.toProduct.prepend(self).imap { case (a, _) => a }((_, ()))
//   inline def *:(other: Schema[Unit]): Product[A] = self.toProduct.prepend(other).imap { case (_, a) => a }(((), _))

// trait ToSchemaOps extends ToSchemaOps1:
//   implicit final def toSchemaOpsTuple[A <: Tuple](self: Schema[A]): SchemaOpsTuple[A] = new SchemaOpsTuple[A](self)
//   implicit final def toSchemaOpsUnit(self: Schema[Unit]): SchemaOpsUnit = new SchemaOpsUnit(self)
// trait ToSchemaOps1:
//   implicit final def toSchemaOps[A](self: Schema[A]): SchemaOps[A] = new SchemaOps[A](self)
