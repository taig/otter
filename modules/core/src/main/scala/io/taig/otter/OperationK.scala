package io.taig.otter

trait OperationK[F[A[_], B[_[a] <: A[a], _]]]:
  extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: F[Shape, Self])
    def imapK[T[_[a] <: Shape[a], _]](
        fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
    )(
        gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
    ): F[Shape, T]

object OperationK:
  inline def apply[F[A[_], B[_[a] <: A[a], _]]](using operation: OperationK[F]): OperationK[F] = operation
