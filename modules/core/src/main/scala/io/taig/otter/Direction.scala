package io.taig.otter

sealed trait Isomorphic[+F[_], A] extends Reader[F, A], Writer[F, A]

object Isomorphic:
  final case class Root[+F[_], A](fa: F[A]) extends Isomorphic[F, A]

  final case class Modify[+F[_], A, B](fa: Isomorphic[F, A], f: A => B, g: B => A) extends Isomorphic[F, B]

sealed trait Reader[+F[_], +A] extends Product, Serializable

sealed trait Writer[+F[_], -A] extends Product, Serializable
