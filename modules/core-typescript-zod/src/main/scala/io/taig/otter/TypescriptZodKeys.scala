package io.taig.otter

trait TypescriptZodKeys:
  val zod: Metadata.Key[Typescript.Value] = Metadata.Key("zod")

object TypescriptZodKeys extends TypescriptZodKeys
