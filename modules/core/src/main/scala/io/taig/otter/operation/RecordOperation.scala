package io.taig.otter.operation

trait RecordOperation[Self[_], -Value[_]]
    extends EmptyOperation[Self],
      LiftOperation[Self, Value],
      ZipOperation[Self, Value]

object RecordOperation:
  inline def apply[Self[_], Value[_]](using
      operation: RecordOperation[Self, Value]
  ): RecordOperation[Self, Value] = operation
