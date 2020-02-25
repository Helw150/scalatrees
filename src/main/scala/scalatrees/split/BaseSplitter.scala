package com.scalatrees.split

import com.scalatrees.internals._

import scala.collection.immutable._

abstract class BaseSplitter[LabelType: Fractional](
  maxDepth: Int = scala.Int.MaxValue,
  minGroupSize: Int = 1,
  splitTestsOpt: Option[Int] = None,
  useMeanValue: Boolean = false
) {
  type GenericNode = Node[Seq[Double], LabelType]
  case class LabeledInput(input: Seq[Double], label: LabelType)

  protected def splitEval(
    split1: Seq[LabeledInput],
    split2: Seq[LabeledInput]
  ): Double

  def buildTree(
    data: Seq[Seq[Double]],
    labels: Seq[LabelType],
    featuresToUse: Seq[Int] = Seq.empty
  ): Tree[Seq[Double], LabelType] = {
    val labeledData = data.zip(labels).map { case (input, label) => LabeledInput(input, label) }
    Tree[Seq[Double], LabelType](findBestTraversal(labeledData, 0, featuresToUse))
  }

  private def findBestTraversal(labeledData: Seq[LabeledInput], depth: Int, featuresToUse: Seq[Int]): GenericNode = {
    if (depth >= maxDepth || labeledData.size <= minGroupSize) {
      createTeminalNode(labeledData)
    } else {
      val buckets = splitTestsOpt.getOrElse(labeledData.length)
      val featureColumns = labeledData
        .map(_.input)
        .transpose

      val featureSplits = if (buckets == labeledData.length) {
        featureColumns
      } else {
        featureColumns.map(featureColumn => computeQuantileSplits(featureColumn, buckets))
      }
      val splitPairs = featureSplits.zipWithIndex
        .filter { featureColumnAndIndex =>
          featuresToUse match {
            case Seq() => true
            case _ => featuresToUse.contains(featureColumnAndIndex._2)
          }

        }
        .flatMap(feature => feature._1.map((_, feature._2)))

      val split = splitPairs
        .map(splitInfo => createSplit(splitInfo, labeledData))
        .map(split => (split, splitEval(split._1, split._2)))
        .minBy(splitEvaluation => splitEvaluation._2)
        ._1
      split match {
        case (Seq(), _, _) | (_, Seq(), _) => createTeminalNode(labeledData)
        case _ =>
          val (splitThreshold, featureIndex) = split._3
          TraversalNode[Seq[Double], LabelType](
            _(featureIndex),
            splitThreshold,
            findBestTraversal(split._1, depth + 1, featuresToUse),
            findBestTraversal(split._2, depth + 1, featuresToUse)
          )
      }
    }
  }

  private def createSplit(
    splitInfo: (Double, Int),
    dataToSplit: Seq[LabeledInput]
  ): (Seq[LabeledInput], Seq[LabeledInput], (Double, Int)) = {
    val (partition1, partition2) = dataToSplit
      .partition(row => row.input(splitInfo._2) > splitInfo._1)

    (partition1, partition2, splitInfo)
  }

  private def createTeminalNode(group: Seq[LabeledInput]): GenericNode = {
    val groupLabels = group.map(_.label)
    val groupAverage = useMeanValue match {
      case true => Fractional[LabelType].div(groupLabels.sum, Fractional[LabelType].fromInt(groupLabels.size))
      case false => groupLabels.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
    }
    TerminalNode[Seq[Double], LabelType](groupAverage)
  }

  private def computeQuantileSplits(
    target: Seq[Double],
    bucketCount: Int
  ): Seq[Double] = {
    val binPercentileWidth = 1.0 / bucketCount
    Range
      .BigDecimal(0, 1.0 + (binPercentileWidth / 2), binPercentileWidth)
      .map(_.toDouble)
      .map(convertPercentileToValue(target, _))
      .sliding(2)
      .toSeq
      .flatten
  }

  private def convertPercentileToValue(target: Seq[Double], percentile: Double): Double = {
    val f = (target.length + 1) * percentile
    val i = f.toInt
    if (i == 0) target.head
    else if (i >= target.length) target.last
    else {
      target(i - 1) + (f - i) * (target(i) - target(i - 1))
    }
  }
}
