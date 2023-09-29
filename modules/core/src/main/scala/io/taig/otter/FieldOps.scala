package io.taig.otter

final class FieldOps[A](self: Field[A]) extends AnyVal:
  inline def :*[B](field: Field[B]): Record[(A, B)] = ???
  inline def *:[B](field: Field[B]): Record[(B, A)] = ???
  inline def *:(field: Field[Unit]): Record[A] = ???

final class FieldOpsUnit(self: Field[Unit]) extends AnyVal:
  inline def :*[A](field: Field[A]): Record[A] = ???

trait ToFieldOps:
  implicit def toFieldOps[A](self: Field[A]): FieldOps[A] = new FieldOps(self)
  implicit def toFieldOpsUnit(self: Field[Unit]): FieldOpsUnit = new FieldOpsUnit(self)
