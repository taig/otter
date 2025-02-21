package io.taig.otter

trait Keys:
  def key[A](value: String): Metadata.Key[A] = Metadata.Key(value)

  val name: Metadata.Key[String] = key("name")
  val namespace: Metadata.Key[String] = key("namespace")
  val typescript: Metadata.Key[String] = key("typescript")

object Keys extends Keys
