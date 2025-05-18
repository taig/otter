package io.taig.otter.component

import io.taig.otter.schema.UnionSchema

import scala.annotation.targetName

trait UnionComponent[Self[_], -Value[_]](using UnionSchema[Self, Value])
