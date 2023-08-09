package io.taig.otter.schema

//final class FieldOps[A, B](self: Field[A, B]) extends AnyVal:
//  inline def :*[C, D](other: Field[C, D]): Record[(B, D)] = self.toRecord :* other
//  inline def *:[C, D](other: Field[C, D]): Record[(D, B)] = other *: self.toRecord
//  inline def :*[C](other: Field[C, Unit]): Record[B] = self.toRecord :* other
//  inline def *:[C](other: Field[C, Unit]): Record[B] = other *: self.toRecord
//final class FieldOpsUnit[A](self: Field[A, Unit]) extends AnyVal:
//  inline def :*[B, C](other: Field[B, C]): Record[C] = self.toRecord :* other
//  inline def *:[B, C](other: Field[B, C]): Record[C] = other *: self.toRecord
//
trait ToFieldOps extends ToFieldOps1
//  implicit final def toFieldOpsUnit[A](self: Field[A, Unit]): FieldOpsUnit[A] = new FieldOpsUnit(self)
trait ToFieldOps1
//  implicit final def toFieldOps[A, B](self: Field[A, B]): FieldOps[A, B] = new FieldOps(self)
