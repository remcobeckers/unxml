package org.rembo.unxml
package streaming

import XmlStreamingTypes._
import akka.stream.scaladsl._
import akka.util.ByteString
import org.rembo.unxml.streaming.transformers.XmlTransformer

object UnXml {

  /**
   * Parse data from source as Xml into type T with the (implicitly) provided XmlReads.
   * @param source A source of ByteStrings
   * @param reads The reader to use to parse T
   * @tparam T The type to parse
   * @return A Source of XmlResult[T]
   */
  def fromXml[T](source: Source[ByteString, Unit])(implicit reads: XmlReads[T]): Source[XmlResult[T], Unit] = {
    val xmlSource = source.transform(() ⇒ new XmlTransformer())
    reads.reads(xmlSource)
  }
}
