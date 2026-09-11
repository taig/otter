package io.taig.otter.http.component

import io.taig.otter.http.Method

/** The methods worth having a name for.
  *
  * Here rather than on [[Method]], which is the division the rest of this vocabulary is built on: the type says what a
  * method is, and the dsl says which ones are worth spelling as a word. [[apply]] names the rest, so the list being
  * only the methods with defined semantics costs nothing.
  */
trait MethodComponent:
  /** A method by name, which is how a method this list has never heard of is still named. */
  inline def apply(name: String): Method = Method(name)

  val delete: Method = Method("DELETE")

  val get: Method = Method("GET")

  val head: Method = Method("HEAD")

  val options: Method = Method("OPTIONS")

  val patch: Method = Method("PATCH")

  val post: Method = Method("POST")

  val put: Method = Method("PUT")

  val trace: Method = Method("TRACE")
