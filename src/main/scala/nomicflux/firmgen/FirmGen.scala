package firmgen

import scala.io.Source

import markov._

object FirmGen {
  import MarkovChain._

  def names(filename: String): Vector[String] = Source.fromFile(filename).getLines.map(_.trim.toLowerCase).toVector

  def mkWeight(n: Int, skip: Int, weight: Double): WChainSpec = WChainSpec(n, skip, weight)

  def mkMarkov(weights: Seq[WChainSpec], filename: String): MarkovChain = MarkovChain.empty(weights).generateFromDocs(names(filename))

  def defaultMarkov: MarkovChain = mkMarkov(Vector(mkWeight(1,1,1.0), mkWeight(2,1,2.0), mkWeight(3,1,4.0)), "names")

  def skippingMarkov: MarkovChain = mkMarkov(Vector(mkWeight(1,1,1.0), mkWeight(2,1,2.0), mkWeight(2,2,1.0), mkWeight(3,1,8.0), mkWeight(3,2,1.0), mkWeight(3,3,1.0)), "names")

  def defaultPomo: MarkovChain = mkMarkov(Vector(mkWeight(1,1,1.0), mkWeight(2,1,2.0), mkWeight(3,1,4.0), mkWeight(4,1,8.0), mkWeight(5,1,16.0), mkWeight(6,1,32.0), mkWeight(7,1,64.0)), "derrida_lines")

  def testPomo: MarkovChain = mkMarkov(Vector(mkWeight(1,1,1.0), mkWeight(2,1,2.0), mkWeight(3,1,4.0), mkWeight(4,1,8.0), mkWeight(5,1,16.0), mkWeight(2,2,1.0)), "derrida_lines")

  def skippingPomo: MarkovChain = mkMarkov(Vector(mkWeight(1,1,1.0), mkWeight(2,1,2.0), mkWeight(3,1,8.0), mkWeight(3,2,1.0), mkWeight(4,1,16.0), mkWeight(4,2,1.0), mkWeight(4,3,1.0), mkWeight(5,1,32.0), mkWeight(5,2,1.0), mkWeight(5,3,1.0), mkWeight(5,4,1.0), mkWeight(6,1,64.0)), "derrida_lines")
}
