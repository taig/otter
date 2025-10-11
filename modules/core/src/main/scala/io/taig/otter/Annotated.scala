package io.taig.otter

import scala.deriving.Mirror
import scala.Tuple.fromProductTyped

trait Annotated[F[_]]:
  self =>

  def get[A](self: F[A]): Metadata

  def update[A](self: F[A], metadata: Metadata => Metadata): F[A]

  def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Annotated[G] = new Annotated[G]:
    override def get[A](ga: G[A]): Metadata = self.get(gK(ga))

    override def update[A](ga: G[A], metadata: Metadata => Metadata): G[A] = fK(self.update(gK(ga), metadata))

object Annotated:
  inline def apply[F[_]](using annotated: Annotated[F]): Annotated[F] = annotated

  def derived[F[_] <: Product, G[_], B](using
      mirror: Mirror.ProductOf[F[Any]] { type MirroredElemTypes = G[B] *: EmptyTuple },
      annotated: Annotated[G]
  ): Annotated[F] = new Annotated[F]:
    override def get[A](self: F[A]): Metadata =
      val gb = fromProductTyped(self)(using
        mirror.asInstanceOf[Mirror.ProductOf[F[A]] { type MirroredElemTypes = G[B] *: EmptyTuple }]
      ).head
      annotated.get(gb)

    override def update[A](self: F[A], metadata: Metadata => Metadata): F[A] =
      val gb = fromProductTyped(self)(using
        mirror.asInstanceOf[Mirror.ProductOf[F[A]] { type MirroredElemTypes = G[B] *: EmptyTuple }]
      ).head
      val update = annotated.update(gb, metadata)
      mirror.fromProduct(update *: EmptyTuple).asInstanceOf[F[A]]
