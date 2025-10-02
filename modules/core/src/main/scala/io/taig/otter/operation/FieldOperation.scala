package io.taig.otter.operation

import io.taig.otter.InvariantK

trait FieldOperation[Self[_], -Value[_]]:
  self =>

  def apply[A](name: String, value: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def optional: Self[Option[A]]

    def optional(default: => A): Self[A]

  def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): FieldOperation[G, Value] =
    new FieldOperation[G, Value]:
      def apply[A](name: String, value: => Value[A]): G[A] = fK(self.apply(name, value))

      extension [A](ga: G[A])
        override def optional: G[Option[A]] = fK(self.optional(gK(ga)))

        override def optional(default: => A): G[A] = fK(self.optional(gK(ga))(default))

object FieldOperation:
  inline def apply[Self[_], Value[_]](using operation: FieldOperation[Self, Value]): FieldOperation[Self, Value] =
    operation

  given [Value[_]]: InvariantK[[s[_]] =>> FieldOperation[s, Value]] with
    extension [G[_]](self: FieldOperation[G, Value])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): FieldOperation[H, Value] =
        self.imapK(fK)(gK)
