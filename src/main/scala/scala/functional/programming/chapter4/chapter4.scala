package scala.functional.programming.chapter4

object chapter4 {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatmap (m =>
      mean(xs map (x => Math.pow(x - m, 2))))
  }
}
