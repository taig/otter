package io.taig.otter.http.syntax

import io.taig.otter.http.Method

trait MethodSyntax:
  inline def apply(value: String): Method = Method(value)

  val post: Method = apply("POST")
  val delete: Method = apply("DELETE")
  val get: Method = apply("GET")
  val head: Method = apply("HEAD")
  val options: Method = apply("OPTIONS")
  val patch: Method = apply("PATCH")
  val put: Method = apply("PUT")
  val trace: Method = apply("TRACE")

object MethodSyntax extends MethodSyntax
