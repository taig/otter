package io.taig.otter

sealed trait Isomorphic[+O[_[_], _], +S[_, _], A, B] extends Writer[O, S, A, B], Reader[O, S, A, B]

object Isomorphic:
  final case class Root[O[_[_], _], S[_, _], A, B](fa: O[S[A, *], B]) extends Isomorphic[O, S, A, B]

sealed trait Writer[+O[_[_], _], +S[_, _], A, -B]

object Writer:
  final case class Root[O[_[_], _], S[_, _], A, B](fa: O[S[A, *], B]) extends Writer[O, S, A, B]

sealed trait Reader[+O[_[_], _], +S[_, _], A, +B]

object Reader:
  final case class Root[O[_[_], _], S[_, _], A, B](fa: O[S[A, *], B]) extends Reader[O, S, A, B]
