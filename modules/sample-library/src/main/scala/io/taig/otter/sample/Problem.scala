package io.taig.otter.sample

/** What this API says when it cannot say what was asked for.
  *
  * One shape for every failure, and a discriminated union underneath it so a caller can branch on the kind rather than
  * matching on prose. The three cases are the three genuinely different things that go wrong here, and each carries
  * what a caller would otherwise have to parse back out of a sentence.
  *
  * The composed API contract also uses this schema for execution failures.
  */
final case class Problem(kind: Problem.Kind, title: String, detail: List[String])

object Problem:
  enum Kind:
    /** The request did not hold what the endpoint describes. Carries one line per violation. */
    case Malformed

    /** The request was understood and refused, because the catalogue is in a state that forbids it. */
    case Conflict

    /** The request named something that is not there. */
    case Missing

    /** An internal execution failure, without diagnostic details. */
    case Internal

  def malformed(detail: List[String]): Problem =
    Problem(Problem.Kind.Malformed, "The request does not hold what this endpoint describes", detail)

  def conflict(title: String): Problem = Problem(Problem.Kind.Conflict, title, Nil)

  def missing(title: String): Problem = Problem(Problem.Kind.Missing, title, Nil)

  val internal: Problem = Problem(Problem.Kind.Internal, "Internal server error", Nil)
