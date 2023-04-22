package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*

private def collectAndRemoveFirst[A, B](chain: Chain[A])(pf: PartialFunction[A, B]): (Chain[A], Option[B]) =
  var result: Option[B] = None

  val filtered = chain.filter { a =>
    if result.isEmpty && pf.isDefinedAt(a) then
      result = Some(pf.apply(a))
      false
    else true
  }

  (filtered, result)

private def printPath(path: Chain[String]): String = "/" + path.mkString_("/")

private def printQueries(queries: Chain[String]): String = queries.mkString_("&")

private def printUrl(path: Chain[String], queries: Chain[String]): String =
  printPath(path) + (if queries.isEmpty then "" else "?" + printQueries(queries))
