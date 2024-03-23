package io.taig.otter

abstract class Dsl[C <: Context](val context: C) extends Types[C] with Schemas[C] with Syntax[C]

object Dsl:
  def apply[C <: Context](context: C): Dsl[C] = new Dsl[C](context) {}
