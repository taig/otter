package io.taig.otter

final case class Value[A](codec: Constant[Value, A] | Enumeration[Value, A] | Primitive[A] | Union.Untagged[Value, A])
    extends AnyVal
