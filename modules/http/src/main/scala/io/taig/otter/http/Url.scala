package io.taig.otter.http

sealed trait Url[+F[+_], +G[+_], +H[+_], +A]:
  def path: Path[F, H, ?]
  def queries: Queries[G, H, ?]

object Url:
  final case class Combine[F[+_], G[+_], H[+_], A, B](left: Url[F, G, H, A], right: Url[F, G, H, B])
      extends Url[F, G, H, (A, B)]:
    override def path: Path[F, H, ?] = left.path.zip(right.path)
    override def queries: Queries[G, H, ?] = left.queries.zip(right.queries)

  final case class Root[F[+_], G[+_], H[+_], A, B](path: Path[F, H, A], queries: Queries[G, H, B])
      extends Url[F, G, H, (A, B)]

  final case class Transform[F[+_], G[+_], H[+_], A, B](self: Url[F, G, H, A], f: A => B) extends Url[F, G, H, B]:
    export self.{path, queries}
