package io.taig.otter

trait Keys:
  def key[A](value: String): Metadata.Key[A] = Metadata.Key(value)

  val discriminator: Metadata.Key[Discriminator] = key("discriminator")
  val nulls: Metadata.Key[Null] = key("null")

object Keys extends Keys
