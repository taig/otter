package io.taig.otter.http

import io.taig.otter.JsonSchemaIssue

/** A way in which a rendered OpenAPI document is not everything the endpoints it came from say.
  *
  * The same contract [[JsonSchemaIssue]] carries, one tier up: a renderer that cannot say something says nothing and
  * records it, so a document always comes back and the caller decides whether to care. `operation` names the endpoint
  * the issue was found in, as `METHOD /path`, which is the only name every endpoint has.
  */
enum OpenApiIssue:
  /** A payload written in an alphabet the renderer was given no schema renderer for.
    *
    * The body is still listed, with its media type and no schema, which is the honest answer: something is sent, and
    * this document cannot say what.
    */
  case Undescribed(operation: String, mediaType: String)

  /** A streamed body, listed by its media type and the schema of one element.
    *
    * OpenAPI has no vocabulary for framing. A reader is told what arrives and not how to find the boundaries between
    * one element and the next, which the media type usually implies and never states.
    */
  case Framed(operation: String, mediaType: String)

  /** Two endpoints on the same method and path. The first is described and the second dropped, because a paths object
    * is keyed by both and cannot hold them.
    */
  case Duplicate(operation: String)

  /** An issue the payload's own renderer reported, kept with the operation it was found in. */
  case Payload(operation: String, issue: JsonSchemaIssue)

  /** An issue a parameter's schema reported. */
  case Parameter(operation: String, name: String, issue: JsonSchemaIssue)

  /** Two different schemas asked to be declared under the same name.
    *
    * A document has one `components/schemas` and a name in it means one shape, so the first is kept and the second
    * reported. The usual cause is a schema whose two sides differ -- an optional field, or one holding a default --
    * used in a request and in a response, where what a reader accepts genuinely is not what a writer produces. The fix
    * is on the schema's side: name the two apart, or make them agree.
    */
  case Conflict(name: String)
