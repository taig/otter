package io.taig.otter.component

import io.taig.otter.operation.UnionOperation

trait UnionComponent[+Self[_], -Value[_]](using UnionOperation[Self, Value])
