package io.taig.otter

trait Keys:
  val name: Metadata.Key[String] = Metadata.Key("name")
  val namespace: Metadata.Key[String] = Metadata.Key("namespace")
  val typescript: Metadata.Key[String] = Metadata.Key("typescript")

object Keys extends Keys
