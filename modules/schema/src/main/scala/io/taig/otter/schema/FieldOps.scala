package io.taig.otter.schema

final class FieldOps[A](self: Field[A]) extends AnyVal:
  inline def :*[B](other: Field[B]): Record[(A, B)] = ??? // self.toRecord :* other
  inline def *:[B](other: Field[B]): Record[(B, A)] = ??? // other *: self.toRecord
  inline def :*(other: Field[Unit]): Record[A] = ??? // self.toRecord :* other
  inline def *:(other: Field[Unit]): Record[A] = ??? // other *: self.toRecord
//final class FieldOpsUnit[A](self: Field[A, Unit]) extends AnyVal:
//  inline def :*[B, C](other: Field[B, C]): Record[C] = self.toRecord :* other
//  inline def *:[B, C](other: Field[B, C]): Record[C] = other *: self.toRecord
//
trait ToFieldOps extends ToFieldOps1
//  implicit final def toFieldOpsUnit[A](self: Field[A, Unit]): FieldOpsUnit[A] = new FieldOpsUnit(self)
trait ToFieldOps1:
  implicit final def toFieldOps[A](self: Field[A]): FieldOps[A] = FieldOps(self)
