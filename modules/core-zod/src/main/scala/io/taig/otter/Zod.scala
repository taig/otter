package io.taig.otter

object Zod:
  val Namespace: "zod" = "zod"

  def key[A](name: String): Metadata.Key[A] = Metadata.Key(namespace = Namespace, name)

  object Key:
    val name: Metadata.Key[String] = Keys.name @@ Zod.Namespace
