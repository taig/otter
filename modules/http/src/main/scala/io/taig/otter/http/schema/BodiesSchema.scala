package io.taig.otter.http.schema

import io.taig.otter.+
import io.taig.otter.schema.Schema

trait BodiesSchema[Self[+_[_], _], Body[_]] extends SchemaK[Self]:
  self =>

  def lift[S[_], A](body: Body[A]): Self[S, A]

  extension [S[_], A](self: Self[S, A]) def orElse[T[_], B](schema: Self[T, B]): Self[S + T, Either[A, B]]

  // override def imapK[T[+_[_], _]](fK: [S[_], A] => Self[S, A] => T[S, A])(
  //     gK: [S[_], A] => T[S, A] => Self[S, A]
  // ): BodiesSchema[T, Body] = new BodiesSchema[T, Body]:
  //   override def apply[S[_], A](body: Body[S, A]): T[S, A] = fK(self(body))

  //   extension [S[_], A](ta: T[S, A])
  //     override def orElse[U[_], B](schema: T[U, B]): T[S + U, Either[A, B]] =
  //       fK(self.orElse(gK(ta))(gK(schema)))

  //   override def algebra[S[_]]: Schema[T[S, *]] = self
  //     .algebra[S]
  //     .imapK(
  //       [A] => (self: Self[S, A]) => fK(self)
  //     )([A] => (value: T[S, A]) => gK(value))
