package io.taig.otter

object JsonSchemaKey:
  val Namespace: String = "json-schema"

  def apply[A](identifier: String): Metadata.Key[A] =
    Metadata.Key(namespace = Namespace, identifier = identifier)
