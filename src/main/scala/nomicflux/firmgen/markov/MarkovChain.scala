package markov

import scala.annotation.tailrec
import scala.util.Random

object MarkovChain {
  type MarkovMatrix = Map[NGram, Map[Option[Char], Double]]

  private def takeNGramWithFollowing(n: Int)(string: String): (NGram, Option[Char]) = {
    val (ngram, next) = string.splitAt(n)
    (NGram.fromString(n, ngram), next.headOption)
  }

  private def takeAllNGramsWithFollowing(n: Int)(string: String): Vector[(NGram, Option[Char])] =
    string.tails.map(takeNGramWithFollowing(n)).toVector

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

  def apply(chainSize: Int, docs: Vector[String]): MarkovChain = {
    val matrix = toMarkovMatrix(docs.flatMap(takeAllNGramsWithFollowing(chainSize)))
    MarkovChain(chainSize, normalizeMatrix(matrix))
  }
}

case class MarkovChain(chainSize: Int, probs: MarkovChain.MarkovMatrix) {
  private def splitOption[A](vec: Vector[A]): Option[(A, Vector[A])] =
    if (vec.isEmpty) None else Some((vec.head, vec.tail))

  def getRow(string: String): Map[Option[Char], Double] = probs.getOrElse(NGram.fromString(chainSize, string), Map.empty)

  def nextChar(string: String): (Option[Char], Double) = {
    val prob = Random.nextDouble()
    val row = MarkovChain.cumulateRow(getRow(string))
    row.dropWhile(_._2 < prob).headOption.map(_._1)
  }
}
