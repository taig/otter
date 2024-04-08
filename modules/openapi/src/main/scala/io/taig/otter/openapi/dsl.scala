package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity
import io.taig.otter.Attribute
import io.taig.otter.openapi as OpenApi
import io.taig.otter.Metadatas

object dsl extends Dsl:
  self =>

  override object Metadata extends Metadatas:
    override type Schema = OpenApi.Metadata[Identity]
    override type Value = OpenApi.Metadata.Value[Identity]
    override type Primitive = OpenApi.Metadata.Primitive[Identity]
    override val primitive: Metadata.Primitive = OpenApi.Metadata.Primitive.Default
    override type Tuple = OpenApi.Metadata.Tuple[Identity]

  given [
      S <: Plain.Schema[M[Identity], A] { type Self[+m, a] = T[m, a] },
      T[+m, a] <: Plain.Schema[m, a],
      M[f[_]] <: OpenApi.Metadata[f] { type Self[f[_]] = N[f] },
      N[f[_]] <: OpenApi.Metadata[f] { type Self[f[_]] = O[f] },
      O[f[_]] <: OpenApi.Metadata[f],
      A
  ]: Conversion[S, O[Attribute[T[O[Identity], A], *]]] = self =>
    self.metadata.asSelf.toAttributes(m => self.update(_ => m))

object Playground:
  import dsl.*
  import dsl.given

  val x: Primitive.Required[String] = string
  val y: Primitive[Int] = x.imap(_.length)(_.toString)
  val z: Schema[String] = y.imap(_.toString)(_.length)

  val a: Primitive.Required[String] = x.name.clear
