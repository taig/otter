package io.taig.otter.operation

trait OrElseOperation[Self[_]]:
  def orElse[A, B](left: Self[A], right: Self[B]): Self[Either[A, B]]
