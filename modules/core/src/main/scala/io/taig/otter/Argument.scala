package io.taig.otter

type Argument[A] = A | Argument.Default

object Argument:
  type Default = Argument.Default.type
  case object Default
