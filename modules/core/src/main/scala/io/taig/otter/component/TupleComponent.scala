package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[+Self[_], -Value[_]](using TupleOperation[Self, Value])
