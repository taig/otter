package io.taig.otter.http.codec

import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Pipe
import fs2.Pull
import fs2.Stream
import io.taig.otter.http.Frame
import scodec.bits.ByteVector

/** Incremental framing. Only the unfinished frame is retained between upstream pulls. */
private[http] object Http4sFraming:
  def decode[F[_]: Concurrent](frame: Frame, limit: Int): Pipe[F, Byte, ByteVector] = frame match
    case Frame.Lines =>
      bytes =>
        split(ByteVector(10), limit, lines = true)(bytes).zipWithIndex.evalMap: (record, index) =>
          val (bytes, terminated) = record
          val line = if terminated && bytes.lastOption.contains(13.toByte) then bytes.dropRight(1) else bytes
          Either
            .cond(
              line.nonEmpty && !line.containsSlice(ByteVector(13)),
              line,
              Http4sStreamFailure.syntax(index, "nonempty line without literal carriage returns")
            )
            .liftTo[F]
    case Frame.Delimited(separator) => bytes => split(utf8(separator), limit)(bytes).map(_._1)
    case Frame.Events               => events(limit)
    case Frame.Raw                  => _ => Stream.raiseError(Http4sStreamFailure.syntax(0, "raw byte body"))

  private def split[F[_]: Concurrent](
      separator: ByteVector,
      limit: Int,
      lines: Boolean = false
  ): Pipe[F, Byte, (ByteVector, Boolean)] = input =>
    val delimiter = separator.toArray.toVector
    // A suffix matching the delimiter is lookahead, not part of the unfinished payload.
    def go(
        rest: Stream[F, Byte],
        pending: Vector[Byte],
        matched: Int,
        index: Long
    ): Pull[F, (ByteVector, Boolean), Unit] =
      rest.pull.uncons1.flatMap:
        case None =>
          if pending.isEmpty then Pull.done
          else if pending.size <= limit then Pull.output1((ByteVector(pending), false))
          else Pull.raiseError(Http4sStreamFailure.syntax(index, "frame size limit"))
        case Some((byte, tail)) =>
          val prefix = delimiter.take(matched) :+ byte
          val matching = (math.min(prefix.size, delimiter.size) to 0 by -1)
            .find(length => prefix.endsWith(delimiter.take(length)))
            .getOrElse(0)
          val cr =
            if lines && (byte == 13 || (matching == delimiter.size && pending.lastOption.contains(13.toByte))) then 1
            else 0
          val payloadSize = pending.size.toLong + 1 - matching - cr
          if payloadSize > limit then Pull.raiseError(Http4sStreamFailure.syntax(index, "frame size limit"))
          else if matching == delimiter.size then
            val value = pending.dropRight(delimiter.size - 1)
            Pull.output1((ByteVector(value), true)) >> go(tail, Vector.empty, 0, index + 1)
          else go(tail, pending :+ byte, matching, index)
    go(input, Vector.empty, 0, 0).stream

  private def events[F[_]: Concurrent](limit: Int): Pipe[F, Byte, ByteVector] = input =>
    def lineEnd(
        rest: Stream[F, Byte],
        line: Vector[Byte],
        data: Vector[String],
        size: Int,
        first: Boolean,
        index: Long,
        skipLf: Boolean
    ): Pull[F, ByteVector, Unit] =
      val text = new String(line.toArray, java.nio.charset.StandardCharsets.UTF_8)
      val value = if first then text.stripPrefix("\ufeff") else text
      if value.isEmpty then
        val emit = if data.isEmpty then Pull.done else Pull.output1(utf8(data.mkString("\n")))
        emit >> go(rest, Vector.empty, Vector.empty, 0, false, index + (if data.isEmpty then 0 else 1), skipLf)
      else
        val colon = value.indexOf(':')
        val name = if colon < 0 then value else value.take(colon)
        val content = if colon < 0 then "" else value.drop(colon + 1).stripPrefix(" ")
        val next = if name == "data" then data :+ content else data
        go(rest, Vector.empty, next, size, false, index, skipLf)

    def go(
        rest: Stream[F, Byte],
        line: Vector[Byte],
        data: Vector[String],
        size: Int,
        first: Boolean,
        index: Long,
        skipLf: Boolean
    ): Pull[F, ByteVector, Unit] =
      rest.pull.uncons1.flatMap:
        case None                       => Pull.done // SSE dispatch requires a terminating blank line.
        case Some((10, tail)) if skipLf => go(tail, line, data, size, first, index, false)
        case Some((byte, tail))         =>
          if size >= limit then Pull.raiseError(Http4sStreamFailure.syntax(index, "event size limit"))
          else if byte == 10 || byte == 13 then lineEnd(tail, line, data, size + 1, first, index, byte == 13)
          else go(tail, line :+ byte, data, size + 1, first, index, false)
    go(input, Vector.empty, Vector.empty, 0, true, 0, false).stream

  def encode(frame: Frame, limit: Int, bytes: ByteVector, index: Long): Either[Http4sStreamFailure, ByteVector] =
    def refuse(reason: String) = Left(Http4sStreamFailure.encoding(index, reason))
    if bytes.size > limit then refuse("frame size limit")
    else
      frame match
        case Frame.Lines =>
          if bytes.isEmpty || bytes.containsSlice(ByteVector(10)) || bytes.containsSlice(ByteVector(13)) then
            refuse("an element must occupy exactly one nonempty line")
          else Right(bytes ++ ByteVector(10))
        case Frame.Delimited(separator) =>
          val delimiter = utf8(separator)
          val framed = bytes ++ delimiter
          if framed.indexOfSlice(delimiter) != bytes.size then refuse("element contains an ambiguous delimiter")
          else Right(framed)
        case Frame.Events =>
          bytes.decodeUtf8
            .leftMap(_ => Http4sStreamFailure.encoding(index, "UTF-8 event"))
            .flatMap: value =>
              val encoded = utf8(value.split("\r\n|\r|\n", -1).map("data: " + _ + "\n").mkString + "\n")
              if encoded.size > limit then refuse("event size limit") else Right(encoded)
        case Frame.Raw => refuse("Use a raw byte body")

  private def utf8(value: String): ByteVector = ByteVector.view(value.getBytes(java.nio.charset.StandardCharsets.UTF_8))
