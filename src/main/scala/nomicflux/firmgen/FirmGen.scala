package firmgen

import scala.io.Source

import markov._

object FirmGen {
  import MarkovChain._

  def names(filename: String): Vector[String] = Source.fromFile(filename).getLines.map(_.trim).toVector

  def mkWeight(n: Int, skip: Int, weight: Double): WChainSpec = WChainSpec(n, skip, weight)

  def mkMarkov(weights: Seq[WChainSpec], filename: String): MarkovChain = MarkovChain.empty(weights).generateFromDocs(names(filename))
}
