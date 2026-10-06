package io.taig.otter.sample

/** A declared failure. The JSON union writes the case as `kind`, so clients narrow on the same choice Scala makes. */
enum Problem(val title: String, val detail: List[String]):
  case Malformed(override val title: String, override val detail: List[String]) extends Problem(title, detail)
  case Internal(override val title: String, override val detail: List[String]) extends Problem(title, detail)
  case Conflict(override val title: String, override val detail: List[String]) extends Problem(title, detail)
  case Missing(override val title: String, override val detail: List[String]) extends Problem(title, detail)
  case Unrouted(override val title: String, override val detail: List[String]) extends Problem(title, detail)

object Problem:
  def malformed(detail: List[String]): Problem =
    Problem.Malformed("The request does not hold what this endpoint describes", detail)

  def conflict(title: String): Problem = Problem.Conflict(title, Nil)

  def missing(title: String): Problem = Problem.Missing(title, Nil)

  val internal: Problem = Problem.Internal("Internal server error", Nil)

  val notFound: Problem = Problem.Unrouted("No endpoint answers this path", Nil)

  def methodNotAllowed(allowed: List[String]): Problem =
    Problem.Unrouted("No endpoint answers this method on this path", allowed)
