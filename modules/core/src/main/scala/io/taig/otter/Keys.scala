package io.taig.otter

trait Keys:
  val description: Metadata.Key[String] = Metadata.Key("description")
  val name: Metadata.Key[String] = Metadata.Key("name")
  val namespace: Metadata.Key[String] = Metadata.Key("namespace")
  val zod: Metadata.Key[String] = Metadata.Key("zod")

object Keys extends Keys
