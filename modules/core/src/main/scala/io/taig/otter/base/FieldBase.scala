package io.taig.otter.base

sealed abstract class FieldBase[+S[_], A] extends FieldBase.Read[S, A], FieldBase.Write[S, A]

object FieldBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable

  sealed trait Write[+S[_], -A] extends Product, Serializable
