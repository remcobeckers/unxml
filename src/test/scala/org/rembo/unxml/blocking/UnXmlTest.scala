package org.rembo.unxml
package blocking

import org.scalatest._

import scala.io.Source

import XmlBlockingTypes._

class UnXmlTest extends WordSpecLike with Matchers with OptionValues with Inside with Inspectors {

  "UnXml.fromXml" should {
    "be able to read a traversable" in {
      val xml = <top>
                  <children>
                    <child>One</child>
                    <child>Two</child>
                    <child>Three</child>
                  </children>
                </top>

      val reader = (XmlPath \ "children").readAll[Vector, String]("child")
      val result = UnXml.fromXml(xml)(reader)
      result shouldBe XmlSuccess(Vector("One", "Two", "Three"))
    }
  }

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
    "return a header and a traversable of the nodes" in {
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
      val children = traversable.map(_.getOrElse("ERROR")).toList
      assert(children === List("One", "Two", "Three"))
    }
  }
}
