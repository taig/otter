package io.taig.otter

import io.taig.otter as Base

trait Plain extends Dsl:
  override object metadata extends Metadata:
    override type Schema = Unit
    override type Collection = Unit
    override type Primitive = Unit
    override type Tuple = Unit

object Plain extends Plain
