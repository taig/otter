package io.taig.otter

trait Keys:
  val description: Metadata.Key[String] = Metadata.Key("description")
  val title: Metadata.Key[String] = Metadata.Key("title")

object Keys extends Keys
