package io.taig.otter

trait Keys:
  val name: Metadata.Key[String] = Metadata.Key("name")
  val namespace: Metadata.Key[String] = Metadata.Key("namespace")

object Keys extends Keys
