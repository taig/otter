package io.taig.otter.schema

final class FieldOps[A](self: Field[A]) extends AnyVal:
  inline def :*[B](other: Field[B]): Record[(A, B)] = ??? // self.toRecord :* other
  inline def *:[B](other: Field[B]): Record[(B, A)] = ??? // other *: self.toRecord
  inline def :*(other: Field[Unit]): Record[A] = ??? // self.toRecord :* other
  inline def *:(other: Field[Unit]): Record[A] = ??? // other *: self.toRecord
final class FieldOpsUnit(self: Field[Unit]) extends AnyVal:
  inline def :*[B](other: Field[B]): Record[B] = ???
  inline def *:[B](other: Field[B]): Record[B] = ???
trait ToFieldOps extends ToFieldOps1:
  implicit final def toFieldOpsUnit(self: Field[Unit]): FieldOpsUnit = FieldOpsUnit(self)
trait ToFieldOps1:
  implicit final def toFieldOps[A](self: Field[A]): FieldOps[A] = FieldOps(self)
