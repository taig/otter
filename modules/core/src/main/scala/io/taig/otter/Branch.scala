package io.taig.otter

sealed abstract class Branch[+O <: Data, A] extends Branch.Reader[O, A], Branch.Writer[O, A]:
  def imap[B](f: A => B): Branch[O, B]

object Branch:
  sealed trait Reader[+O <: Data, +A]:
    def name: String
    def metadata: Metadata
    def codec: Codec[?, ?, ?]
    def map[B](f: A => B): Branch.Reader[O, B]
    def decode(data: Data): Codec.Result[A]

  sealed trait Writer[+O <: Data, -A]:
    def name: String
    def metadata: Metadata
    def codec: Codec[?, ?, ?]
    def contramap[B](f: B => A): Branch.Writer[O, B]
    def encode(a: A): O
