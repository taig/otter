//package io.taig.otter.schema
//
//import io.taig.otter.schema.Schema.Record
//
//final class RecordOps[A](self: Record[A]) extends AnyVal:
//  inline def :*[B, C](other: Field[B, C]): Record[(A, C)] = self.zip(other.toRecord)
//  inline def *:[B, C](other: Field[B, C]): Record[(C, A)] = other.toRecord.zip(self)
//  inline def :*[B](other: Field[B, Unit]): Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
//  inline def *:[B](other: Field[B, Unit]): Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))
//final class RecordOpsUnit(self: Record[Unit]) extends AnyVal:
//  inline def :*[A, B](other: Field[A, B]): Record[B] = self.zip(other.toRecord).imap { case (_, b) => b }(((), _))
//  inline def *:[A, B](other: Field[A, B]): Record[B] = other.toRecord.zip(self).imap { case (b, _) => b }((_, ()))
//final class RecordOpsTuple[A <: Tuple](self: Record[A]) extends AnyVal:
//  inline def :*[B, C](other: Field[B, C]): Record[Tuple.Append[A, C]] =
//    self.zip(other.toRecord).imap { case (a, c) => a :* c }(ac => (ac.init.asInstanceOf[A], ac.last.asInstanceOf[C]))
//  inline def *:[B, C](other: Field[B, C]): Record[C *: A] =
//    other.toRecord.zip(self).imap { case (c, a) => c *: a } { case c *: a => (c, a) }
//  inline def :*[B](other: Field[B, Unit]): Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
//  inline def *:[B](other: Field[B, Unit]): Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))
//
//trait ToRecordOps extends ToRecordOps1:
//  implicit def toRecordOpsUnit(self: Record[Unit]): RecordOpsUnit = RecordOpsUnit(self)
//  implicit def toRecordOpsTuple[A <: Tuple](self: Record[A]): RecordOpsTuple[A] = RecordOpsTuple(self)
//trait ToRecordOps1:
//  implicit def toRecordOps[A](self: Record[A]): RecordOps[A] = RecordOps(self)
