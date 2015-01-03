package org.rembo.unxml
package blocking

import XmlBlockingTypes._

object DefaultXmlReads extends DefaultXmlReads

trait DefaultXmlReads extends XmlReaderTupleInstances {
  implicit val stringReads: XmlReads[String] = XmlReads(s ⇒ XmlResult(s))
  implicit val intReads: XmlReads[Int] = XmlReads(s ⇒ XmlResult(s.toInt))
  implicit val doubleReads: XmlReads[Double] = XmlReads(s ⇒ XmlResult(s.toDouble))
  implicit val booleanReads: XmlReads[Boolean] = XmlReads(s ⇒ XmlResult(s.toBoolean))

  implicit def optionReads[T: XmlReads]: XmlReads[Option[T]] = implicitly[XmlReads[T]].mapResult(_.map(Some(_)).orElse(XmlSuccess(None)))
}
