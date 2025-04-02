package io.taig.otter

abstract class TypescriptCodecs // extends Primitives.Defaults[Typescript]:
// final override protected def lift[A](codec: Primitive[A]): Typescript[A] = Typescript(self = codec)

object TypescriptCodecs extends TypescriptCodecs
