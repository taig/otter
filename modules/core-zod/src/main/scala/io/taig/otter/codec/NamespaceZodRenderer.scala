package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys.*
import io.taig.otter.ZodConst
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState
import io.taig.otter.schema.EnrichedSchema

final class NamespaceZodRenderer[S[_]: EnrichedSchema](renderer: Renderer[S, ZodState[String]])
    extends Renderer[S, ZodState[ZodExpression]]:
  val expression = ZodRenderer[S](renderer)

  override def render[A](schema: S[A]): ZodState[ZodExpression] = State: state =>
    schema.metadata(name) match
      case Some(name) =>
        println(s"Render: $name")
        if state.recursion then
          println(s"Oh boy we gotta act: $name")
          (state, ZodExpression.Referenced(ZodConst(name), "z.lazy(???)"))
        else if state.stack.contains_(name)
        then
          println(s"Abort: $name")
          (
            state.copy(recursion = true),
            ZodExpression.Referenced(ZodConst(name), value = "doesnt matter will be ignore")
          )
        else
          val const = ZodConst(namespace = schema.metadata(namespace), symbol(name))
          state.references.get(const) match
            case Some(value) => (state, ZodExpression.Referenced(const, value))
            case None =>
              val (update, result) = expression.render(schema).run(initial = state.put(name)).value
              println(s"Rendered $name: " + result)
              (
                update.modifyReferences(_.updatedWith(const)(_ => Some(result))).remove(name),
                ZodExpression.Referenced(const, result)
              )
      case None => expression.render(schema).run(initial = state).value.map(ZodExpression.Inline.apply)

  def symbol(value: String): String = value.replace(".", "").replace(" ", "")
