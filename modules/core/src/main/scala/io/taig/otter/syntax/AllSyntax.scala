package io.taig.otter.syntax

trait AllSyntax
    extends AnnotatedSyntax,
      CoerceSyntax,
      CollectionSyntax,
      ConstantSyntax,
      DictionarySyntax,
      EnumerationSyntax,
      InvariantSyntax,
      NullableSyntax,
      RecordSyntax,
      TupleSyntax,
      UnionSyntax

object AllSyntax extends AllSyntax
