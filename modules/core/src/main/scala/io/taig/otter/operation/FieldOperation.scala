package io.taig.otter.operation

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
