package io.taig.otter.http

trait MethodDsl:
  object method:
    val post: Method = Method.Post
    val delete: Method = Method.Delete
    val get: Method = Method.Get
    val head: Method = Method.Head
    val options: Method = Method.Options
    val patch: Method = Method.Patch
    val put: Method = Method.Put
    val trace: Method = Method.Trace

object MethodDsl extends MethodDsl
