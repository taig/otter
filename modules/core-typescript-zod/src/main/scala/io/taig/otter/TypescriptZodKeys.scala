package io.taig.otter

trait TypescriptZodKeys:
  val zod: Metadata.Key[String] = Metadata.Key("zod")

object TypescriptZodKeys extends TypescriptZodKeys
