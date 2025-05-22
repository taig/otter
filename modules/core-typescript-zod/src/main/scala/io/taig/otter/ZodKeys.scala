package io.taig.otter

trait ZodKeys:
  val zod: Metadata.Key[String] = Metadata.Key("zod")

object ZodKeys extends ZodKeys
