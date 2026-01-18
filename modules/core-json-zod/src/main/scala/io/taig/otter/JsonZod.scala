package io.taig.otter

object JsonZod:
  val Namespace: "json-zod" = "json-zod"

  def key[A](name: String): Metadata.Key[A] = Metadata.Key(namespace = Namespace, name)

  object Key:
    val name: Metadata.Key[String] = Keys.name @@ Namespace
