package io.taig.otter

sealed abstract class Collection[+S[_], A] extends Collection.Read[S, A], Collection.Write[S, A]

object Collection:
  sealed trait Read[+S[_], +A]

  sealed trait Write[+S[_], -A]
