package io.taig.otter

trait Keys:
  def key[A](value: String): Metadata.Key[A] = Metadata.Key(value)

  val name: Metadata.Key[String] = key("name")

object Keys extends Keys
