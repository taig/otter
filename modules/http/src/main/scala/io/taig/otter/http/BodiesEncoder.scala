package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.Accept
import io.taig.otter.http.header.MediaRange

final class BodiesEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def apply[A](bodies: Bodies[S, A], accept: Option[Accept], a: A): Option[Array[Byte]] =
    val mediaRanges = accept.flatMap(_.toResult.right).fold(Nil)(_.toList)
    apply(bodies, accept = mediaRanges, a)

  def apply[A](bodies: Bodies[S, A], accept: List[MediaRange], a: A): Option[Array[Byte]] = bodies match
    case Bodies.Modify(self, _, g) => apply(bodies = self, accept, g(a))
    case Bodies.Or(left, right) =>
      accept.collectFirstSome: mediaRange =>
        if left.satisfies(mediaRange) then apply(bodies = left, accept, a)
        else if right.satisfies(mediaRange) then apply(bodies = right, accept, a)
        else none
    case Bodies.OrElse(left, right) =>
      a.fold(apply(bodies = left, accept, _), apply(bodies = right, accept, _))
    case Bodies.Root(body) => BodyEncoder(encoder)(body, accept, a)
