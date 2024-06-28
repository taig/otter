package io.taig.otter

trait TransformationOps[Self[_, _]]

object TransformationOps:
  trait Isomorphic[Self[_, _], Writer[_], Constraint[_]] extends TransformationOps[Self]:
    extension [A, B](self: Self[A, B])
      def transform[C, D, E](
          transformation: Transformation[B, Constraint[(Writer[C], C)], (Writer[D], D), E]
      ): Self[A, E]
      final def apply[C, D, E](transformation: Transformation[B, C, D, E]): Self[A, E] = ???

  trait Reader[Self[_, _], Writer[_], Constraint[_]] extends TransformationOps[Self]:
    extension [A, B](self: Self[A, B])
      def transform[C, D, E](
          transformation: Transformation.Reader[B, Constraint[(Writer[C], C)], (Writer[D], D), E]
      ): Self[A, E]

  trait Writer[Self[_, _]] extends TransformationOps[Self]:
    extension [A, B](self: Self[A, B]) def transform[C](transformation: Transformation.Writer[B, C]): Self[A, C]
