package io.taig.otter

trait TypescriptKeys:
  val typescript: Metadata.Key[Typescript[Typescript.Value]] = Metadata.Key("typescript")

object TypescriptKeys extends TypescriptKeys
