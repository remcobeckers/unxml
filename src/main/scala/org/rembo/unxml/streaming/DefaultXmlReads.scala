package org.rembo.unxml
package streaming

import XmlStreamingTypes._

object DefaultXmlReads extends DefaultXmlReads

trait DefaultXmlReads extends XmlReaderTupleInstances {
  implicit val stringReads: XmlReads[String] = XmlReads(s ⇒ XmlResult(s))
  implicit val intReads: XmlReads[Int] = XmlReads(s ⇒ XmlResult(s.toInt))
  implicit val doubleReads: XmlReads[Double] = XmlReads(s ⇒ XmlResult(s.toDouble))
  implicit val booleanReads: XmlReads[Boolean] = XmlReads(s ⇒ XmlResult(s.toBoolean))
}
