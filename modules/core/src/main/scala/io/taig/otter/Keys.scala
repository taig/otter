package io.taig.otter

trait Keys:
  val description: Metadata.Key[String] = Metadata.Key("description")
  val hidden: Metadata.Key[Boolean] = Metadata.Key("hidden")
  val name: Metadata.Key[String] = Metadata.Key("name")
  val namespace: Metadata.Key[String] = Metadata.Key("namespace")

object Keys extends Keys
