package markov

import scala.annotation.tailrec
import scala.util.Random

object MarkovChain {
  type MarkovMatrix = Map[NGram, Map[Option[Char], Double]]

  case class WChainSpec(n: Int, skip: Int, weight: Double)

  private def takeNGramWithFollowing(n: Int, skip: Int)(string: String): (NGram, Option[Char]) = {
    val (ngram, next) = string.splitAt(n*skip)
    (NGram.fromString(n, skip, ngram), next.headOption)
  }

  private def takeAllNGramsWithFollowing(n: Int, skip: Int)(string: String): Vector[(NGram, Option[Char])] =
    string.tails.dropRight(n*skip).map(takeNGramWithFollowing(n, skip)).toVector

  private def toMarkovMatrix(ngrams: Vector[(NGram, Option[Char])]): MarkovMatrix =
    ngrams.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.size.toDouble))

  def normalizeRow(row: Map[Option[Char], Double]): Map[Option[Char], Double] = {
    val cts = row.values.sum
    row.mapValues( _ / cts )
  }

  def cumulateRow(row: Map[Option[Char], Double]): Vector[(Option[Char], Double)] = {
    row.toVector.foldLeft((Vector.empty, 0.0d)) { case ((acc, prob), (k, v)) =>
      val newProb = prob + v
      (acc :+ (k, newProb), newProb)
    }._1
  }

  private def normalizeMatrix(matrix: MarkovMatrix): MarkovMatrix = matrix.mapValues(row => normalizeRow(row))

  def empty(chainSize: Int): MarkovChain = SingleMarkovChain(chainSize, Map.empty)

  def empty(weights: Seq[WChainSpec]): MarkovChain = MarkovChains(weights.map(w => WeightedChain(w.weight, empty(w.n))))

  def apply(chainSize: Int, docs: Vector[String]): MarkovChain = {
    val matrix = toMarkovMatrix(docs.flatMap(takeAllNGramsWithFollowing(chainSize)))
    SingleMarkovChain(chainSize, normalizeMatrix(matrix))
  }
}

sealed trait MarkovChain {
  def generateFromDocs(docs: Vector[String]): MarkovChain
  def nextChar(string: String): (Option[Char], Double)
}

case class SingleMarkovChain(chainSize: Int, probs: MarkovChain.MarkovMatrix) extends MarkovChain {
  private def splitOption[A](vec: Vector[A]): Option[(A, Vector[A])] =
    if (vec.isEmpty) None else Some((vec.head, vec.tail))

  def generateFromDocs(docs: Vector[String]): MarkovChain = MarkovChain(docs)

  def getRow(string: String): Map[Option[Char], Double] = probs.getOrElse(NGram.fromString(chainSize, string), Map.empty)

  def nextChar(string: String): (Option[Char], Double) = {
    val prob = Random.nextDouble()
    val row = MarkovChain.cumulateRow(getRow(string))
    row.dropWhile(_._2 < prob).headOption.map(_._1)
  }
}

case class WeightedChain(weight: Double, chain: MarkovChain)

case class MarkovChains(wchains: Seq[WeightedChain]) extends MarkovChain {
  def generateFromDocs(docs: Vector[String]): MarkovChain = {
    MarkovChains(wchains.map(_.chain.generateFromDocs(docs)))
  }

  def nextChar(string: String): (Option[Char], Double) = {
    val charsWithProbs: Seq[(Option[Char], Double)] =
      wchains.map( wchain => {
                    val (char, prob) = wchain.chain.nextChar(string)
                    (char, prob * wchain.weight)
                  }
      )
    val normalizedRow: Vector[(Option[Char], Double)] = MarkovChain.cumulateRow(normalizeRow(charsWithProbs.toMap))
    val rand = Random.nextDouble()
    normalizedRow.dropWhile( _._2 < rand ).headOption.map(_._1)
  }
}
