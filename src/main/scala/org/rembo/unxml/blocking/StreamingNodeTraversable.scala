package org.rembo.unxml
package blocking

import scala.io.Source
import scala.xml._

import XmlBlockingTypes._

/**
 * Convert a scala.io.Source to an Iterator of NodeSeq's from the
 * provided XmlPath.
 * Based on the Stack Overflow question:
 * http://stackoverflow.com/questions/8525675/how-to-get-a-streaming-iteratornode-from-a-large-xml-document
 */
object StreamingNodeTraversable {

  // Desired external API:
  // at(path).readOne[Header] followedBy
  // at(path).readMany[TraversableOnce[Bla]]
  // Return value is a TupleN with max 1 TraversableOnce

  // When returning as a tuple or case class nothing is possible anymore after returning the traversable
  // Other option: Return value is a TraversableOnce[Any] and pattern match on output. Allows for
  // getting multiple sections as traversables but less convenient to work with.

  def readFromSource(targetPaths: List[XmlPath])(input: Source): Traversable[NodeSeq] =
    generatorToTraversable(processSource(input, targetPaths))

  def processSource[T](input: Source, targetPaths: List[XmlPath])(f: NodeSeq ⇒ T) {
    val parser = new scala.xml.parsing.ConstructingParser(input, false) {
      var currentPath = XmlPath.empty
      var targets = targetPaths

      override def elemStart(pos: Int, pre: String, label: String,
        attrs: MetaData, scope: NamespaceBinding) {
        super.elemStart(pos, pre, label, attrs, scope)
        if (label != null) currentPath = currentPath \ label
      }
      override def elemEnd(pos: Int, pre: String, label: String) {
        if (label != null) currentPath = currentPath.init
        super.elemEnd(pos, pre, label)
      }

      override def elem(pos: Int, pre: String, label: String, attrs: MetaData,
        pscope: NamespaceBinding, empty: Boolean, nodes: NodeSeq): NodeSeq = {
        val node = super.elem(pos, pre, label, attrs, pscope, empty, nodes)
        if (targets.head == currentPath) {
          if (targets.tail != Nil) targets = targets.tail
          f(node)
          NodeSeq.Empty
        } else if (targets.head.startsWith(currentPath)) {
          node
        } else {
          node
        }
      }
    }
    parser.nextch // initialize per documentation
    parser.document // trigger parsing by requesting document
  }

  private def generatorToTraversable[T](func: (T ⇒ Unit) ⇒ Unit) =
    new Traversable[T] {
      def foreach[X](f: T ⇒ X) {
        func(f(_))
      }
    }
}

