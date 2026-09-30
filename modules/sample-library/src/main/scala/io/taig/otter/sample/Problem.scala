package io.taig.otter.sample

/** What this API says when it cannot say what was asked for.
  *
  * One shape for every failure, and a discriminated union underneath it so a caller can branch on the kind rather than
  * matching on prose. Each case is a genuinely different thing that goes wrong here, and each carries what a caller
  * would otherwise have to parse back out of a sentence.
  *
  * The composed API contract also uses this schema for execution failures, and for a request no endpoint answers.
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

    /** No endpoint answers the method and path the request arrived with. Carries the methods that path does take.
      *
      * Not [[Problem.Kind.Missing]], because a `405` names a path that is there, and because an endpoint that answers
      * `404` with a [[Problem]] of its own would otherwise read "no such route" as "no such thing".
      */
    case Unrouted

  def malformed(detail: List[String]): Problem =
    Problem(Problem.Kind.Malformed, "The request does not hold what this endpoint describes", detail)

  def conflict(title: String): Problem = Problem(Problem.Kind.Conflict, title, Nil)

  def missing(title: String): Problem = Problem(Problem.Kind.Missing, title, Nil)

  val internal: Problem = Problem(Problem.Kind.Internal, "Internal server error", Nil)

  val notFound: Problem = Problem(Problem.Kind.Unrouted, "No endpoint answers this path", Nil)

  def methodNotAllowed(allowed: List[String]): Problem =
    Problem(Problem.Kind.Unrouted, "No endpoint answers this method on this path", allowed)
