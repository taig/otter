package io.taig.otter.component
import io.taig.otter.Branch
import io.taig.otter.Reference

trait BranchComponent[F[+_[a] <: H[a], _], H[_]](using F: Branch[F, H]):
  object branch:
    def apply[I[a] <: H[a], A](name: String, schema: => I[A]): F[I, A] =
      F.apply(name, schema = Reference.later(schema))
