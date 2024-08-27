package io.taig.otter

trait Keys:
  def key[A](value: String): Metadata.Key[A] = Metadata.Key(value)

  object discriminator:
    val nested: Metadata.Key[Discriminator.Nested] = key("discriminator")
    val merged: Metadata.Key[Discriminator.Merged] = key("discriminator")

  val nulls: Metadata.Key[Null] = key("null")

object Keys extends Keys
