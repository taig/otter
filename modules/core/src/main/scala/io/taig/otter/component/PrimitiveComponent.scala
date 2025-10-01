package io.taig.otter.component

trait PrimitiveComponent[+Self[_]] extends BooleanComponent[Self], NumberComponent[Self], StringComponent[Self]
