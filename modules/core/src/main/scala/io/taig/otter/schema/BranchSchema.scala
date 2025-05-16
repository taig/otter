package io.taig.otter.schema

import io.taig.otter.Reference

trait BranchSchema[Self[_], Key[_], Value[_]] extends Schema[Self]:
  def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

  def key[A](self: Self[A]): Reference.Constant[Key, ?]
  def value[A](self: Self[A]): Reference[Value, ?]
