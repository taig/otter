package io.taig.otter

object ZodJson:
  val Namespace: "zod-json" = "zod-json"

  def key[A](name: String): Metadata.Key[A] = Metadata.Key(namespace = Namespace, name)

  object Key:
    val name: Metadata.Key[String] = Keys.name @@ Namespace
