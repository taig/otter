package io.taig.otter.http

import cats.data.NonEmptyList
import io.taig.otter.Metadata

/** What every TypeScript renderer of an endpoint agrees on, whatever library reads its payloads. */
object HttpTypescript:
  /** The [[Metadata.Namespace]] the endpoint renderers read their attributes from. */
  val Namespace: Metadata.Namespace = Metadata.Namespace("http-typescript")

  /** The layers a target prepends to whatever chain its payload generator already reads.
    *
    * Two of them, most specific first, and then the target's own: what an endpoint says to a TypeScript renderer wins
    * over what it says to HTTP, which wins over everything a payload had already said to a schema generator. Kept as a
    * prefix rather than a whole chain so that a second target splices its own layers in without restating these, and so
    * that a schema which has told one generator how to render itself has told this one too.
    *
    * [[Http.Namespace]] is here for the reason it is in [[OpenApi.Namespaces]]: an attribute like [[HttpKeys.filename]]
    * belongs to HTTP and to no document format.
    */
  val Layers: NonEmptyList[Metadata.Namespace] = NonEmptyList.of(HttpTypescript.Namespace, Http.Namespace)
