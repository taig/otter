package io.taig.otter

trait OperationInvariant[F[A[_], B[_[a] <: A[a], _]]]:
  extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: F[Shape, Self])
    def imapK[T[_[a] <: Shape[a], _]](
        fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
    )(
        gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
    ): F[Shape, T]

object OperationInvariant:
  trait Simple[F[_[_]]] extends OperationInvariant[[A[_], B[_[a] <: A[a], _]] =>> F[[a] =>> B[Nothing, a]]]:
    extension [Self[_]](operation: F[Self])
      def imapKSimple[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): F[T]
