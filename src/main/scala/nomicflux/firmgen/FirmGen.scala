package FirmGen

import nomicflux.firmgen.markov._

object FirmGen {
  import MarkovChain._

  def names(filename: String): Vector[String] = Source.fromFile(filename).getLines.map(_.trim).toVector

  def mkWeight(n: Int, skip: Int, weight: Double): WChainSpec = WChainSpec(n, skip, weight)

  def mkMarkov(weights: WChainSpec, filename: filename): MarkovChain = MarkovChain.empty(weights).generateFromDocs(names(filename))
}
