package io.taig.otter.http

import cats.effect.Concurrent
import fs2.Stream
import io.taig.otter.http.codec.Http4sStreamFailure

private[http] object Http4sStreaming:
  def isStream(value: Body.Value[?, ?, ?]): Boolean = value match
    case Body.Value.Modify(self, _, _)                    => isStream(self)
    case Body.Value.Streamed(_, _, _) | Body.Value.Raw(_) => true
    case _                                                => false

  def validate(endpoint: Endpoint.Node): Unit =
    endpoint.request.bodies.foreach(reference => validateBodies(Bodies.branches(reference.value).toList))
    val responses = Responses.branches(endpoint.responses).toList
    responses.foreach(_.bodies.foreach(reference => validateBodies(Bodies.branches(reference.value).toList)))
    responses
      .groupBy(_.status)
      .values
      .foreach: alternatives =>
        val choices = alternatives.flatMap: response =>
          response.bodies.fold(List((Option.empty[MediaType], false)))(reference =>
            Bodies
              .branches(reference.value)
              .toList
              .map(body => (Some(body.mediaType.essence), isStream(body.self.self)))
          )
        choices.zipWithIndex.foreach: (left, index) =>
          choices
            .drop(index + 1)
            .foreach: right =>
              require(
                !(left._2 || right._2) || (left._1.nonEmpty && right._1.nonEmpty && left._1 != right._1),
                "Streamed response alternatives must have distinct status and media type"
              )

  private def validateBodies(bodies: List[Body.Node[?, ?]]): Unit =
    bodies.zipWithIndex.foreach: (left, index) =>
      bodies
        .drop(index + 1)
        .foreach: right =>
          require(
            !(isStream(left.self.self) || isStream(
              right.self.self
            )) || left.mediaType.essence != right.mediaType.essence,
            "Streamed body alternatives must have distinct media types"
          )

  def context[F[_]: Concurrent, A](
      endpoint: Endpoint.Node,
      direction: Http4sFailure.Direction,
      category: Failure.Category = Failure.Category.EntityRead
  )(stream: Stream[F, A]): Stream[F, A] =
    stream.handleErrorWith:
      case failure: Http4sFailure.Streaming    => Stream.raiseError(failure)
      case Http4sStreamFailure(index, failure) =>
        val located = failure.copy(violations = "body" /: (index.toInt /: failure.violations))
        Stream.raiseError(Http4sFailure.Streaming(endpoint, direction, Some(index), located.failure))
      case cause =>
        Stream.raiseError(Http4sFailure.Streaming(endpoint, direction, None, Failure(category, cause = Some(cause))))
