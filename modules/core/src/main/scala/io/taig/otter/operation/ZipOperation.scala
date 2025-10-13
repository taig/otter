package io.taig.otter.operation

trait ZipOperation[Self[_], -Value[_]]:
  def zip[A, B](left: Self[A], right: Self[B]): Self[(A, B)]
