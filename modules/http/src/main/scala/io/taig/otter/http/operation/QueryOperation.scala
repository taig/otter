package io.taig.otter.http.operation

import io.taig.otter.Reference
import scala.annotation.targetName
import io.taig.otter.http.Http
import io.taig.otter.InvariantK

trait QueryOperation[F[_]]:
  self =>

  def lift[A](name: String, parameter: Reference[Http.Query.Parameter, A]): F[A]

  def mapK[G[_]](fK: [A] => F[A] => G[A]): QueryOperation[G] = new QueryOperation[G]:
    override def lift[A](name: String, parameter: Reference[Http.Query.Parameter, A]): G[A] =
      fK(self.lift(name, parameter))

object QueryOperation:
  trait Read[F[_]] extends QueryOperation[F]:
    self =>

    @targetName("liftRead")
    def lift[A](name: String, parameter: Reference[Http.Query.Parameter.Read, A]): F[A]

    final override def lift[A](name: String, parameter: Reference[Http.Query.Parameter, A]): F[A] =
      lift(name, parameter: Reference[Http.Query.Parameter.Read, A])

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): QueryOperation.Read[G] = new Read[G]:
      @targetName("liftRead")
      override def lift[A](name: String, parameter: Reference[Http.Query.Parameter.Read, A]): G[A] =
        fK(self.lift(name, parameter))

  object Read:
    inline def apply[F[_]](using self: QueryOperation.Read[F]): QueryOperation.Read[F] = self

    given InvariantK[QueryOperation.Read]:
      extension [G[_]](self: QueryOperation.Read[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Read[H] = self.mapK(fK)

  trait Write[F[_]] extends QueryOperation[F]:
    self =>

    @targetName("liftWrite")
    def lift[A](name: String, parameter: Reference[Http.Query.Parameter.Write, A]): F[A]

    final override def lift[A](name: String, parameter: Reference[Http.Query.Parameter, A]): F[A] =
      lift(name, parameter: Reference[Http.Query.Parameter.Write, A])

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): QueryOperation.Write[G] = new Write[G]:
      @targetName("liftWrite")
      override def lift[A](name: String, parameter: Reference[Http.Query.Parameter.Write, A]): G[A] =
        fK(self.lift(name, parameter))

  object Write:
    inline def apply[F[_]](using self: QueryOperation.Write[F]): QueryOperation.Write[F] = self

    given InvariantK[QueryOperation.Write]:
      extension [G[_]](self: QueryOperation.Write[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H] = self.mapK(fK)

  inline def apply[F[_]](using self: QueryOperation[F]): QueryOperation[F] = self

  given InvariantK[QueryOperation]:
    extension [G[_]](self: QueryOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): QueryOperation[H] = self.mapK(fK)
