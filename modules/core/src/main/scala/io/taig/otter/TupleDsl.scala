package io.taig.otter

trait TupleDsl[+Self[_], -Value[_]]:
  def TNil: Self[Unit]

  extension [A](self: Value[A]) def toTuple: Self[A]
