package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Field
import io.taig.otter.Record
import io.taig.otter.Merge
import cats.Invariant
import cats.syntax.all.*

trait RecordSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Record[F, G])
    def fields: Chain[Field[G, ?]] = F.fields(self)

    def zip[B](schema: => F[G, B]): F[G, (A, B)] = F.zip(self, schema)

  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using Record[F, G], Invariant[F[G, *]])
    def :*[B](schema: => F[G, B])(using merge: Merge[A, B]): F[G, merge.Out] =
      self.zip(schema).imap(merge.apply)(merge.unapply)

object RecordSyntax extends RecordSyntax
