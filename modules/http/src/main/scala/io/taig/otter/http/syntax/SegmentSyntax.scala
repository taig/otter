package io.taig.otter.http.syntax

import io.taig.otter.http.operation.PathableOperation
import io.taig.otter.Append

trait SegmentSyntax:
  extension [F[_], G[_], A](fa: F[A])(using F: PathableOperation[F, G]) def toPath: G[A] = 
    F.toPath(fa)

  extension [F[_], G[_], A](fa: F[A])(using F: PathableOperation[F, G])
    def :*[B](fb: F[B]): G[Append[A, B]] = ???

object SegmentSyntax extends SegmentSyntax
