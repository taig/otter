package io.taig.otter

final class RecordOpsUnit(self: Record[Unit]) extends AnyVal:
  inline def zip[A](record: Record[A]): Record[A] = ???
  inline def :*[A](field: Field[A]): Record[A] = zip(field.toRecord)

final class RecordOpsTuple[A <: Tuple](self: Record[A]) extends AnyVal:
  inline def zip[B](record: Record[B]): Record[Tuple.Append[A, B]] = ???
  inline def :*[B](field: Field[B]): Record[Tuple.Append[A, B]] = zip(field.toRecord)

final class RecordOps[A](self: Record[A]) extends AnyVal:
  inline def zip[B](record: Record[B]): Record[(A, B)] = self.product(record)
  inline def :*[B](field: Field[B]): Record[(A, B)] = zip(field.toRecord)

trait ToRecordOps:
  implicit def toRecordOpsUnit(self: Record[Unit]): RecordOpsUnit = RecordOpsUnit(self)
  implicit def toRecordOpsTuple[A <: Tuple](self: Record[A]): RecordOpsTuple[A] = RecordOpsTuple(self)
  implicit def toRecordOps[A](self: Record[A]): RecordOps[A] = RecordOps(self)
