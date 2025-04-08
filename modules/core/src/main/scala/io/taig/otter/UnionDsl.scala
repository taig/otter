package io.taig.otter

trait UnionDsl[+Self[_], -Value[_]] extends UnionDsl.Untagged[Self, Value]

object UnionDsl:
  trait Untagged[+Self[_], -Value[_]](using codec: Codec.Union.Untagged[Self, Value]):
    self =>

    final def branch[A](name: String, codec: => Value[A]): Self[A] = self.codec.branch(name, codec)
