package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (value, rng) if value < 0 && value == Int.MinValue => (-(value + 1), rng)
    case (value, rng) if value < 0 => (-value, rng)
    case (value, rng) => (value, rng)
  }

  def double(rng: RNG): (Double, RNG) = map(int)(_.toDouble/Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (valInt, newRng) = int(rng)
    val (valDouble, newnRng) = double(newRng)
    ((valInt, valDouble), newnRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((valInt, valDouble), newRng) = intDouble(rng)
    ((valDouble, valInt), newRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (valDouble1, newRng) = double(rng)
    val (valDouble2, new2Rng) = double(newRng)
    val (valDouble3, new3Rng) = double(new2Rng)
    ((valDouble1, valDouble2, valDouble3), new3Rng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (value, newRng) = int(rng)
    val (lst, result) = ints(count - 1)(newRng)
    (value +: lst, result)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a, b), rb2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val (a, rng2) = fs.head(rng)
      val (lst, r) = sequence(fs.tail)(rng2)
      (a :: lst, r)
    }


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      val (b, rng3) = g(a)(rng2)
      (b, rng3)
    }
}

