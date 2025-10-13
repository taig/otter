package io.taig.otter.component

import io.taig.otter.operation.NullableOperation

trait NullableComponent[+Self[_], -Value[_]](using NullableOperation[Self, Value])
