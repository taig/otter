package io.taig.otter

type Value[A] = A | Value.Default

object Value:
  type Default = Value.Default.type
  case object Default
