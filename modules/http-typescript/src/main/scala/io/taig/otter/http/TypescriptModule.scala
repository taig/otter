package io.taig.otter.http

import io.taig.otter.Typescript

/** Generated TypeScript, and where the renderer fell short of what the endpoints said.
  *
  * The pairing [[OpenApiDocument]] uses, and for the same reason: a module always comes back, so a caller writes it out
  * and reads the issues rather than catching something.
  */
final case class TypescriptModule(declarations: List[Typescript.Statement], issues: List[TypescriptIssue]):
  /** The module as source, one declaration per paragraph. */
  def render: String = declarations.map(_.render).mkString("\n\n")
