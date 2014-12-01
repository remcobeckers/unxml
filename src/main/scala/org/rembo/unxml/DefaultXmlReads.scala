package org.rembo.unxml

trait DefaultXmlReads {

  implicit val readString: XmlRead[String] = XmlRead(n ⇒ n.text)
  implicit val readInt: XmlRead[Int] = XmlRead(n ⇒ n.text.toInt)
  implicit val readDouble: XmlRead[Double] = XmlRead(n ⇒ n.text.toDouble)
  implicit val readBoolean: XmlRead[Boolean] = XmlRead(n ⇒ n.text.toBoolean)
}
