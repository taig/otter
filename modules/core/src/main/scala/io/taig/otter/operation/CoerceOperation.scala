package io.taig.otter.operation

import io.taig.otter.InvariantK
import io.taig.otter.Reference

trait CoerceOperation[Self[_], Read[_], Write[_], Value[_]]
    extends CoerceOperation.Read[Self, Value]
    with CoerceOperation.Write[Self, Value]:
  self =>

  override def imapK[H[_]](fK: [A] => Self[A] => H[A])(
      gK: [A] => H[A] => Self[A]
  ): CoerceOperation[H, Read, Write, Value] =
    new CoerceOperation[H, Read, Write, Value]:
      override def coerce[A](schema: => Value[A]): H[A] = fK(self.coerce(schema))

      override def schema[A](ha: H[A]): Reference[Value, ?] = self.schema(gK(ha))

object CoerceOperation:
  trait Read[Self[_], Value[_]]:
    self =>

    def coerce[A](schema: => Value[A]): Self[A]

    def schema[A](self: Self[A]): Reference[Value, ?]

    def imapK[H[_]](fK: [A] => Self[A] => H[A])(gK: [A] => H[A] => Self[A]): CoerceOperation.Read[H, Value] =
      new CoerceOperation.Read[H, Value]:
        override def coerce[A](schema: => Value[A]): H[A] = fK(self.coerce(schema))

        override def schema[A](ha: H[A]): Reference[Value, ?] = self.schema(gK(ha))

  object Read:
    inline def apply[Self[_], Value[_]](using
        operation: CoerceOperation.Read[Self, Value]
    ): CoerceOperation.Read[Self, Value] = operation

    given [Value[_]]: InvariantK[[s[_]] =>> CoerceOperation.Read[s, Value]] with
      extension [G[_]](operation: CoerceOperation.Read[G, Value])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CoerceOperation.Read[H, Value] =
          operation.imapK(fK)(gK)

  trait Write[Self[_], Value[_]]:
    self =>

    def coerce[A](schema: => Value[A]): Self[A]

    def schema[A](self: Self[A]): Reference[Value, ?]

    def imapK[H[_]](fK: [A] => Self[A] => H[A])(gK: [A] => H[A] => Self[A]): CoerceOperation.Write[H, Value] =
      new CoerceOperation.Write[H, Value]:
        override def coerce[A](schema: => Value[A]): H[A] = fK(self.coerce(schema))

        override def schema[A](ha: H[A]): Reference[Value, ?] = self.schema(gK(ha))

  object Write:
    inline def apply[Self[_], Value[_]](using
        operation: CoerceOperation.Write[Self, Value]
    ): CoerceOperation.Write[Self, Value] = operation

    given [Value[_]]: InvariantK[[s[_]] =>> CoerceOperation.Write[s, Value]] with
      extension [G[_]](operation: CoerceOperation.Write[G, Value])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): CoerceOperation.Write[H, Value] =
          operation.imapK(fK)(gK)

  inline def apply[Self[_], Read[_], Write[_], Value[_]](using
      operation: CoerceOperation[Self, Read, Write, Value]
  ): CoerceOperation[Self, Read, Write, Value] = operation

  given [Read[_], Write[_], Value[_]]: InvariantK[[s[_]] =>> CoerceOperation[s, Read, Write, Value]] with
    extension [G[_]](operation: CoerceOperation[G, Read, Write, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(
          gK: [A] => H[A] => G[A]
      ): CoerceOperation[H, Read, Write, Value] =
        operation.imapK(fK)(gK)
