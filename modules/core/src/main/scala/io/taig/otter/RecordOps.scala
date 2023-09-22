package io.taig.otter

final class RecordOps[A](self: Schema.Record[A]) extends AnyVal:
  inline def :*[B](other: Field[?, B]): Schema.Record[(A, B)] = self.zip(other.toRecord)
  inline def *:[B](other: Field[?, B]): Schema.Record[(B, A)] = other.toRecord.zip(self)
  inline def :*(other: Field[?, Unit]): Schema.Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
  inline def *:(other: Field[?, Unit]): Schema.Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))
final class RecordOpsUnit(self: Schema.Record[Unit]) extends AnyVal:
  inline def :*[A](other: Field[?, A]): Schema.Record[A] = self.zip(other.toRecord).imap { case (_, b) => b }(((), _))
  inline def *:[A](other: Field[?, A]): Schema.Record[A] = other.toRecord.zip(self).imap { case (b, _) => b }((_, ()))
final class RecordOpsTuple[A <: Tuple](self: Schema.Record[A]) extends AnyVal:
  inline def :*[B](other: Field[?, B]): Schema.Record[Tuple.Append[A, B]] =
    self.zip(other.toRecord).imap { case (a, c) => a :* c }(ac => (ac.init.asInstanceOf[A], ac.last.asInstanceOf[B]))
  inline def *:[B](other: Field[?, B]): Schema.Record[B *: A] =
    other.toRecord.zip(self).imap { case (c, a) => c *: a } { case c *: a => (c, a) }
  inline def :*(other: Field[?, Unit]): Schema.Record[A] = self.zip(other.toRecord).imap { case (a, _) => a }((_, ()))
  inline def *:(other: Field[?, Unit]): Schema.Record[A] = other.toRecord.zip(self).imap { case (_, a) => a }(((), _))

trait ToRecordOps extends ToRecordOps1:
  implicit def toRecordOpsUnit(self: Schema.Record[Unit]): RecordOpsUnit = RecordOpsUnit(self)
  implicit def toRecordOpsTuple[A <: Tuple](self: Schema.Record[A]): RecordOpsTuple[A] = RecordOpsTuple(self)
trait ToRecordOps1:
  implicit def toRecordOps[A](self: Schema.Record[A]): RecordOps[A] = RecordOps(self)
