package io.taig.otter.codec

import io.taig.otter.Typescript

/** The three things a schema library spells its own way, and everything [[JsonStateTypescriptRenderer]] needs to know
  * about one.
  *
  * Hoisting a definition, noticing a cycle and deciding which declaration can infer its type are the same problem
  * whatever the library; only the words differ. Keeping them behind this is what stops a second target from having to
  * copy the fixpoint.
  */
trait JsonTypescriptTarget:
  /** The type a definition declares when it can be inferred from the value, given the name that value is bound to. */
  def inferred(symbol: Typescript.Expression): Typescript.Type

  /** The type a definition's constant is ascribed to when inference cannot see through its cycle. */
  def annotation(name: String): Typescript.Type

  /** How a definition refers to a name whose value does not exist yet. */
  def suspend(self: Typescript.Expression): Typescript.Expression
