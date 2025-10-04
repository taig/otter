package io.taig.otter.component

import io.taig.otter.operation.RecordOperation

trait RecordComponent[-Shape[_], Self[_[a] <: Shape[a], _], Field[_[a] <: Shape[a], _]]:
  extension [Value[a] <: Shape[a], A](self: Field[Value, A])
    def toRecord(using operation: RecordOperation[Self[Value, *], Field[Value, *]]): Self[Value, A] =
      operation.lift(self)
