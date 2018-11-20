package markov

object NGram {
  def fromString(n: Int, skip: Int, string: String): NGram =
    NGram(n, skip, string.grouped(skip).map(_.head).take(n).mkString)
}

case class NGram(n: Int, skip: Int, value: String)
