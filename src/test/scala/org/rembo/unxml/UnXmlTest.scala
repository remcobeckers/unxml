package org.rembo.unxml

import io.Source

import org.scalatest._

class UnXmlTest extends WordSpecLike with Matchers with OptionValues with Inside with Inspectors
    with DefaultXmlReads {

  "UnXml.traversableFromXml" should {
    "return a traversable of all nodes at the requested path" in {
      val xmlSource = Source.fromString(
        """<top>
          | <children>
          |   <child>One</child>
          |   <child>Two</child>
          |   <child>Three</child>
          | </children>
          |</top>
        """.stripMargin)
      val traversable = UnXml.traversableFromXml[String](XmlPath \ "top" \ "children" \ "child", xmlSource).map(_.getOrElse("ERROR"))
      assert(traversable.toList === List("One", "Two", "Three"))
    }

    "return an empty traversable it there are no nodes at the requested path" in {
      val xmlSource = Source.fromString(
        """<top>
          | <children>
          | </children>
          |</top>
        """.stripMargin)
      val traversable = UnXml.traversableFromXml[String](XmlPath \ "top" \ "children" \ "child", xmlSource)
      assert(traversable.toList === List.empty)
    }
  }

  "UnXml.traversableWithHeaderFromXml" should {
    "return a header and a traversable of the nodes" ignore {
      val xmlSource = Source.fromString(
        """<top>
          | <header>
          |   testHeader
          | </header>
          | <children>
          |   <child>One</child>
          |   <child>Two</child>
          |   <child>Three</child>
          | </children>
          |</top>
        """.stripMargin)
      val (header, traversable) = UnXml.traversableWithHeaderFromXml[String, String](XmlPath \ "top" \ "header", XmlPath \ "top" \ "children" \ "child", xmlSource)
      header shouldBe (XmlSuccess("testHeader"))
      assert(traversable.map(_.getOrElse("ERROR")).toList === List("One", "Two", "Three"))
    }
  }
}
