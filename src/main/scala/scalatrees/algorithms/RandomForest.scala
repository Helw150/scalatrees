package com.scalatrees.algorithms

import com.scalatrees.internals._
import com.scalatrees.internals.ImplicitFractionals._
import com.scalatrees.split.GiniSplitter

import scala.collection.parallel.ParSeq
import scala.collection.parallel.CollectionConverters._
import scala.util.Random

class RandomForest[InputType, LabelType: Fractional](
  trees: ParSeq[Tree[InputType, LabelType]]
) {

  def predict(input: InputType): LabelType = {
    val weakPredictions = trees.map(tree => tree.predict(input))
    weakPredictions.head match {
      case _: Boolean => weakPredictions.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
      case _ => Fractional[LabelType].div(weakPredictions.sum, Fractional[LabelType].fromInt(weakPredictions.size))
    }
  }
}

object RandomForest {
  import com.scalatrees.internals.ImplicitFractionals._
  def train = train[Boolean] _

  def train[LabelType: Fractional](
    inputData: Seq[Seq[Double]],
    labels: Seq[LabelType],
    numTrees: Int,
    numFeaturesPerTree: Int = 5,
    percentageDataPerTree: Double = 1
  ): RandomForest[Seq[Double], LabelType] = {
    val splitter = new GiniSplitter[LabelType]()
    val treeIndices = (0 to numTrees).toList.par
    val numRows = Option((inputData.length * percentageDataPerTree).toInt).filter(_ != 0).getOrElse(1)
    val featureIndices = (0 to inputData.headOption.getOrElse(Seq.empty).length)
    val forest = treeIndices.map { _ =>
      val (treeData, treeLabels) = Random.shuffle(inputData.zip(labels)).take(numRows).unzip
      val randomlySelectedFeatures =
        Random.shuffle(featureIndices).take(numFeaturesPerTree)
      splitter.buildTree(treeData, treeLabels)
    }

    new RandomForest[Seq[Double], LabelType](forest)
  }
}
