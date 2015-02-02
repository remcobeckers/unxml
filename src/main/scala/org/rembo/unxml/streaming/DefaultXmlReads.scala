package org.rembo.unxml
package streaming

import XmlStreamingTypes._

object DefaultXmlReads extends DefaultXmlReads

trait DefaultXmlReads extends XmlReaderTupleInstances {
  implicit val stringReads: XmlReads[String] = XmlReads(s ⇒ XmlResult(s))
  implicit val intReads: XmlReads[Int] = stringReads.map(_.toInt)
  implicit val doubleReads: XmlReads[Double] = stringReads.map(_.toDouble)
  implicit val booleanReads: XmlReads[Boolean] = stringReads.map(_.toBoolean)
}
