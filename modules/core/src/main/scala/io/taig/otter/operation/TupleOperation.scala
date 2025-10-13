package io.taig.otter.operation

trait TupleOperation[Self[_], -Value[_]]
    extends EmptyOperation[Self],
      LiftOperation[Self, Value],
      ZipOperation[Self, Value]

object TupleOperation:
  inline def apply[Self[_], Value[_]](using
      operation: TupleOperation[Self, Value]
  ): TupleOperation[Self, Value] = operation
