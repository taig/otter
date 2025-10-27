package io.taig.otter.syntax

import io.taig.otter.Annotated
import io.taig.otter.Metadata

trait AnnotatedSyntax

object AnnotatedSyntax extends AnnotatedSyntax:
  extension [A](self: A)(using annotated: Annotated[A])
    def attr[B](key: Metadata.Key[B], value: B): A =
      annotated.update(self, _.put(key, value))

    def attr[B](key: Metadata.Key[B]): Option[B] = annotated.get(self).get(key)
