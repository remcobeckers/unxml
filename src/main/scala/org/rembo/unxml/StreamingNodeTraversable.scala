package org.rembo.unxml

import io.Source
import scala.xml._

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

  def readManyFromSource[T: XmlReads](path: XmlPath)(input: Source): TraversableOnce[XmlResult[T]] = {
    val reads = implicitly[XmlReads[T]]
    generatorToTraversable(processSource(input, path :: Nil)).map(reads.reads(_))
  }

  def readManyWithHeaderFromSource[H: XmlReads, T: XmlReads](headerPath: XmlPath, path: XmlPath)(input: Source): (XmlResult[H], TraversableOnce[XmlResult[T]]) = {
    val headerReads = implicitly[XmlReads[H]]
    val reads = implicitly[XmlReads[T]]
    val traversable = generatorToTraversable(processSource(input, headerPath :: path :: Nil))

    val header = headerReads.reads(traversable.head)
    (header, traversable.map(reads.reads(_)))
  }

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
          f(node)
          if (targets.tail != Nil) targets = targets.tail
          NodeSeq.Empty
          //        } else if (targetPath.startsWith(currentPath)) {
          //          node
        } else {
          <dummy/> // All other nodes are not interesting
          //          node
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

