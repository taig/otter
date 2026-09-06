package io.taig.otter.http

/** A way in which generated TypeScript is not everything the endpoints it came from say.
  *
  * The contract [[OpenApiIssue]] carries, in another language: a renderer that cannot say something says nothing and
  * records it, so a module always comes back and the caller decides whether to care. `operation` names the endpoint the
  * issue was found in, as `METHOD /path`, which is the only name every endpoint has.
  */
enum TypescriptIssue:
  /** A payload written in an alphabet the renderer was given no renderer for.
    *
    * The descriptor still carries the media type and names no schema for it, which is the honest answer: something is
    * sent, and this module cannot say what it is shaped like.
    */
  case Undescribed(operation: String, mediaType: String)

  /** A streamed body, which a descriptor has no way to describe.
    *
    * A sequence of elements arriving over time is not a value a schema decodes, and the framing that says where one
    * ends is not something the payload alphabet knows. Reported rather than half emitted, on the reasoning
    * [[Http4sIssue]] records for the same case.
    */
  case Streamed(operation: String, mediaType: String)

  /** A [[Multipart]] payload, which is a structure of bodies rather than a document, and which a `Schema` cannot stand
    * in for.
    */
  case Multipart(operation: String)

  /** Two endpoints claiming one name. The first is described and the second dropped, because a module cannot bind one
    * name twice.
    */
  case Duplicate(operation: String, name: String)

  /** Two different schemas asked to be declared under the same name.
    *
    * The usual cause is a schema whose two sides differ -- an optional field, or one holding a default -- used in a
    * request and in a response, where what a reader accepts genuinely is not what a writer produces. The fix is on the
    * schema's side: name the two apart, or make them agree.
    */
  case Conflict(name: String)
