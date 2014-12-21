package org.rembo.unxml

import io.Source

import org.scalatest._

class StreamingNodeTraversableTest extends WordSpecLike with Matchers with OptionValues with Inside with Inspectors
  with DefaultXmlReads {
  import StreamingNodeTraversable._

  "The parser" should {
    "return an traversable of all nodes at level 1" in {
      val xmlSource = Source.fromString(
        """<top>
          | <children>
            | <child>One</child>
            | <child>Two</child>
            | <child>Three</child>
          | </children>
          |</top>
        """.stripMargin)
      val sourceReader = readManyFromSource[String](XmlPath \ "top" \ "children"\ "child") _
      val traversable = sourceReader(xmlSource).map(_.getOrElse("ERROR"))
      assert(traversable.toList === List("One", "Two", "Three"))
    }
  }

}
