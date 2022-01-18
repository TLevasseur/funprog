package scala.functional.programming.chapter6

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG) = {
    val (n, rng) = nextInt
    if (n == Int.MinValue)
      (0, rng)
    else if (n < 0)
      (-n, rng)
    else
      (n, rng)
  }

  /**
   *
   * @return a double between 0 and 1
   */
  def nextDouble: (Double, RNG) = {
    val (n, rng) = nonNegativeInt
    (n.toDouble / (Int.MaxValue.toDouble + 1), rng)
  }

  def intDouble: ((Int, Double), RNG) = {
    val (i, tmpRng) = nextInt
    val (d, rng) = tmpRng.nextDouble
    ((i, d), rng)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val (d, tmpRng) = nextDouble
    val (i, rng) = tmpRng.nextInt
    ((d, i), rng)
  }

  def double3: ((Double, Double, Double), RNG) = {
    val (d1, tmpRng) = nextDouble
    val (d2, tmpRng2) = tmpRng.nextDouble
    val (d3, rng) = tmpRng2.nextDouble
    ((d1, d2, d3), rng)
  }

  def int(count: Int): (List[Int], RNG) = {
    val (i, tmprng) = nextInt
    if (count > 1) {
      val (list, rng) = tmprng.int(count - 1)
      (i :: list, rng)
    } else {
      (List(i), tmprng)
    }
  }
}


case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => rng => (f(a), rng))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(g => rng => (g % n, rng))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))
  }

  def ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def double: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, tmpRng) = rng.nextInt
    if (n == Int.MinValue)
      (0, tmpRng)
    else if (n < 0)
      (-n, tmpRng)
    else
      (n, tmpRng)
  }
}

