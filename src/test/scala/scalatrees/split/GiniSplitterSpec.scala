package com.scalatrees.split

import org.scalatest._
import com.scalatrees.internals.ImplicitFractionals._

class GiniSplitterSpec extends FlatSpec with Matchers {
  val testMatrix = Seq(
    Seq(1.0),
    Seq(2.0),
    Seq(3.0)
  )
  val testLabels = Seq(false, false, true)

  ".buildTree" should "build a tree which partitions cleanly sliced data when expensive" in {
    val splitter = new GiniSplitter[Boolean](maxDepth = 3)
    val testTree = splitter.buildTree(testMatrix, testLabels)
    testTree.predict(Seq(5)) should be(true)
    testTree.predict(Seq(-10)) should be(false)
  }

  it should "build a tree which partitions evenly sliced data when cheap" in {
    val splitter = new GiniSplitter[Boolean](maxDepth = 3, splitTestsOpt = Some(2))
    val testTree = splitter.buildTree(testMatrix, testLabels)
    testTree.predict(Seq(5)) should be(true)
    testTree.predict(Seq(-10)) should be(false)
  }
}
