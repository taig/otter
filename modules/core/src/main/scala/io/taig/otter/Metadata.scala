package io.taig.otter

import io.taig.hmap.HMap
import io.taig.hmap.Key

abstract class Metadata[S[+a] <: Schema[a], A, M]:
  def values: HMap[M]

  def set(values: HMap[M]): Cofree[S, A, M]

  final def apply[B](key: Key[B] & Singleton & M): B = values.apply(key)
  final def apply[B](key: Key[B] & Singleton & M, value: B): Cofree[S, A, M] = set(values.put(key, value))
  final def update[B](key: Key[B] & Singleton & M)(f: B => B): Cofree[S, A, M] = set(values.update(key)(f))
