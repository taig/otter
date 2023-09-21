// package io.taig.otter.schema

// final class FieldOps[A](self: Field[A]) extends AnyVal:
//   inline def :*[B](field: Field[B]): Record[(A, B)] = self.toRecord :* field
//   inline def *:[B](field: Field[B]): Record[(B, A)] = field *: self.toRecord
//   inline def :*(field: Field[Unit]): Record[A] = self.toRecord :* field
//   inline def *:(field: Field[Unit]): Record[A] = field *: self.toRecord
// final class FieldOpsUnit(self: Field[Unit]) extends AnyVal:
//   inline def :*[B](field: Field[B]): Record[B] = ???
//   inline def *:[B](field: Field[B]): Record[B] = ???
// trait ToFieldOps extends ToFieldOps1:
//   implicit final def toFieldOpsUnit(self: Field[Unit]): FieldOpsUnit = FieldOpsUnit(self)
// trait ToFieldOps1:
//   implicit final def toFieldOps[A](self: Field[A]): FieldOps[A] = FieldOps(self)
