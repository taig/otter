//package io.taig.otter
//
//final class FieldOps[A](self: Field[?, A]) extends AnyVal:
//  inline def :*[B](field: Field[?, B]): Schema.Record[(A, B)] = self.toRecord :* field
//  inline def *:[B](field: Field[?, B]): Schema.Record[(B, A)] = field *: self.toRecord
//  inline def :*(field: Field[?, Unit]): Schema.Record[A] = self.toRecord :* field
//  inline def *:(field: Field[?, Unit]): Schema.Record[A] = field *: self.toRecord
//final class FieldOpsUnit(self: Field[?, Unit]) extends AnyVal:
//  inline def :*[B](field: Field[?, B]): Schema.Record[B] = ???
//  inline def *:[B](field: Field[?, B]): Schema.Record[B] = ???
//trait ToFieldOps extends ToFieldOps1:
//  implicit final def toFieldOpsUnit(self: Field[?, Unit]): FieldOpsUnit = FieldOpsUnit(self)
//trait ToFieldOps1:
//  implicit final def toFieldOps[A](self: Field[?, A]): FieldOps[A] = FieldOps(self)
