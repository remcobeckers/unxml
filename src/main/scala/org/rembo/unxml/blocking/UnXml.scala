package org.rembo.unxml
package blocking

import scala.io.Source
import scala.xml.NodeSeq

import XmlBlockingTypes._

object UnXml {
  import StreamingNodeTraversable._

  /**
   * Read type T from nodeSeq at the root path
   *
   * @param node The node from whic to read
   * @tparam T The type that is read
   * @return An XmlResult containing a value of type T or an error
   */
  def fromXml[T: XmlReads](node: NodeSeq): XmlResult[T] = implicitly[XmlReads[T]].reads(node)

  /**
   * Read type T from source at the root path
   * @param source The source, will not be automatically closed
   * @tparam T The type to read
   * @return Success with a value of type T or Failure with an error
   */
  def fromXml[T: XmlReads](source: Source): XmlResult[T] = {
    val reads = implicitly[XmlReads[T]]
    reads.reads(readFromSource(List(XmlPath.empty))(source).head)
  }

  /**
   * Read a sequence of the same elements as type T from source at the specified path. This
   * uses a streaming parser to avoid allocating large amounts of memory for the entire
   * document.
   *
   * @param at The path
   * @param source The source, will not be automatically closed
   * @tparam T The type to read
   * @return a Traversable producing Success values containing type T or Failure values containing an error
   */
  def traversableFromXml[T: XmlReads](at: XmlPath, source: Source): TraversableOnce[XmlResult[T]] = {
    val reads = implicitly[XmlReads[T]]
    readFromSource(List(at))(source).map(reads.reads(_).addErrorPathPrefix(at))
  }

  /**
   * First read a "header" of type H at headerAt followed by a sequence
   * of the same elements as type T from source at the specified path. This
   * uses a streaming parser to avoid allocating large amounts of memory for the entire
   * document.
   *
   * @param headerAt The path for the header
   * @param at The path for the sequence
   * @param source The source, will not be automatically closed
   * @tparam H Type of the header result value
   * @tparam T Type of the sequence result values
   * @return a pair of the header XmlResult[H] and a Traversable producing Success values containing type T or Failure values containing an error
   */
  def traversableWithHeaderFromXml[H: XmlReads, T: XmlReads](headerAt: XmlPath, at: XmlPath, source: Source): (XmlResult[H], TraversableOnce[XmlResult[T]]) = {
    val headerReads = implicitly[XmlReads[H]]
    val reads = implicitly[XmlReads[T]]
    val traversable = readFromSource(List(headerAt, at))(source)

    val (head, tail) = traversable.splitAt(1)
    (headerReads.reads(head.head).addErrorPathPrefix(at), tail.map(reads.reads(_).addErrorPathPrefix(at)))
  }

  /**
   * Automatically close the provided source after usage in the process method
   * @param source The source
   * @param process The processor
   * @tparam T
   * @return
   */
  def autoClose[T](source: Source)(process: Source ⇒ T) = {
    try {
      process(source)
    } finally {
      source.close()
    }
  }
}
