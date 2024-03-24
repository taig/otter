package io.taig.otter

abstract class Metadata[S[+a] <: Schema[a], A, M <: Singleton]:
  def values: HMap[M]

  def set(values: HMap[M]): Cofree[S, A, M]

  // def apply[A <: M & HMap.Key[B] & Singleton, B](key: A) = values.apply(key)

  // def apply[A <: M & HMap.Key[B] & Singleton, B](key: A, value: B): S = set(values.put(key, value))

  // def update[A <: M & HMap.Key[B] & Singleton, B](key: A, f: B => B): S = set(values.update(key, f))
