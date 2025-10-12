package io.taig.otter.component

import io.taig.otter.operation.DictionaryOperation

trait DictionaryComponent[-Shape[_], +Self[_[a] <: Shape[a], _]](using DictionaryOperation[Shape, Self])
