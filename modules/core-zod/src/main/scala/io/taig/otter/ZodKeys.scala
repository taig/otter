package io.taig.otter

trait ZodKeys:
  val zod: Metadata.Key[Zod] = Metadata.Key("zod")

object ZodKeys extends ZodKeys
