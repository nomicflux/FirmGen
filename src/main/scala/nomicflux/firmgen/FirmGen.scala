package firmgen

import scala.io.Source

import markov._

object FirmGen {
  import MarkovChain._

  def names(filename: String): Vector[String] = Source.fromFile(filename).getLines.map(_.trim.toLowerCase).toVector

  def mkWeight(n: Int, skip: Int, weight: Double): WChainSpec = WChainSpec(n, skip, weight)

  def mkMarkov(weights: Seq[WChainSpec], filename: String): MarkovChain = MarkovChain.empty(weights).generateFromDocs(names(filename))

  def defaultMarkov: MarkovChain = mkMarkov(Vector(mkWeight(1,1,1.0), mkWeight(2,1,2.0), mkWeight(3,1,4.0)), "names")

  def skippingMarkov: MarkovChain = mkMarkov(Vector(mkWeight(1,1,1.0), mkWeight(2,1,2.0), mkWeight(2,2,2.0), mkWeight(2,3,2.0), mkWeight(3,1,4.0)), "names")
}
