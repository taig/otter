package io.taig.otter

trait JsonSchemaKeys:
  val title: Metadata.Key[String] = JsonSchemaKey("title")
  val description: Metadata.Key[String] = JsonSchemaKey("description")
  val name: Metadata.Key[String] = JsonSchemaKey("name")

object JsonSchemaKeys extends JsonSchemaKeys
