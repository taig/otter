package io.taig.otter.component

import io.taig.otter.operation.DictionaryOperation

trait DictionaryComponent[+Self[_], -Value[_]](using DictionaryOperation[Self, Value])
