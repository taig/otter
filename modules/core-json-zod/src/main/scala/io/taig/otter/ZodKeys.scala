package io.taig.otter

trait ZodKeys:
  val name: Metadata.Key[String] = ZodKey("name")

object ZodKeys extends ZodKeys
