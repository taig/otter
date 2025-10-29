package io.taig.otter

object ZodKey:
  val Namespace: String = "zod"

  def apply[A](name: String): Metadata.Key[A] = Metadata.Key(namespace = Namespace, name)
