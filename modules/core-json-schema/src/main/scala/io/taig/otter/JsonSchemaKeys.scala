package io.taig.otter

trait JsonSchemaKeys:
  val title: Metadata.Key[String] = JsonSchemaKeys("title")
  val description: Metadata.Key[String] = JsonSchemaKeys("description")
  val name: Metadata.Key[String] = JsonSchemaKeys("name")

object JsonSchemaKeys extends JsonSchemaKeys:
  val Namespace: String = "json-schema"

  def apply[A](identifier: String): Metadata.Key[A] =
    Metadata.Key(namespace = Namespace, identifier = identifier)
