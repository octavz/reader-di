
trait Functor[F[_]] {
  def fmap[A, B](f: A ⇒ B)(fa: F[A]): F[B]
}


trait MonadBind[M[_]] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](ma: M[A])(f: A ⇒ M[B]): M[B]

  //for free
  def map[A, B](f: A ⇒ B)(ma: M[A]): M[B] = flatMap(ma)(f andThen pure)
}

trait Monad[M[_]] extends Functor[M] {

  def pure[A](a: A): M[A]

  def join[A](ma: M[M[A]]): M[A]

  //for free
  def flatMap[A, B](ma: M[A])(f: A ⇒ M[B]): M[B] = join(fmap(f)(ma))

  def map[A, B](f: A ⇒ B)(ma: M[A]): M[B] = fmap(f)(ma)

}

sealed trait Maybe[+A] {
  def map[B](f: A ⇒ B)(implicit ev: Monad[Maybe]): Maybe[B] = ev.map(f)(this)

  def flatMap[B](f: A ⇒ Maybe[B])(implicit ev: Monad[Maybe]): Maybe[B] = ev.flatMap(this)(f)
}

case class Just[A](value: A) extends Maybe[A]

case object JustNothing extends Maybe[Nothing]

sealed case class Reader[R, A](runReader: R => A)


object Helpers {

  object Implicits {

    implicit object FunctorMaybe extends Functor[Maybe] {

      def fmap[A, B](f: A ⇒ B)(fa: Maybe[A]): Maybe[B] = fa match {
        case Just(v) ⇒ Just(f(v))
        case _ ⇒ JustNothing
      }
    }

    class FunctorReader[R] extends Functor[({type Alias[a] = Reader[R, a]})#Alias] {

      type Alias[A] = Reader[R, A]

      override def fmap[A, B](f: A => B)(fa: Alias[A]): Alias[B] =
        Reader[R, B](x => f(fa.runReader(x)))
    }

    implicit object MonadMaybe extends Monad[Maybe] {
      override def pure[A](a: A): Maybe[A] = Just(a)

      override def join[A](ma: Maybe[Maybe[A]]): Maybe[A] = ma match {
        case Just(Just(v)) ⇒ Just(v)
        case _ ⇒ JustNothing
      }

      override def fmap[A, B](f: (A) ⇒ B)(fa: Maybe[A]): Maybe[B] = FunctorMaybe.fmap(f)(fa)
    }


    //    implicit object WrapperMaybe extends WrapperWithBind[Maybe] {
    //
    //      override def pure[A](a: A): Maybe[A] = if (a == null) JustNothing else Just(a)
    //
    //      override def flatMap[A, B](ma: Maybe[A])(f: A ⇒ Maybe[B]): Maybe[B] = ma match {
    //        case Just(v) ⇒ f(v)
    //        case _ ⇒ JustNothing
    //      }
    //    }

  }

  object HelloWorld extends App {

    import Helpers.Implicits._

    val maybeTen = Just(10)
    val maybeNothing = JustNothing
    val maybeTest = Just("test")

    def f(x: Int): Maybe[Int] = Just(x + 1)

    def g(x: Int): Maybe[Int] = Just(x + 2)

    def ff(x: Int): Int = x * 2

    def gg(x: Int): Int = x * 3

    //f-laws
    import FunctorMaybe._

    assert(fmap(identity[Int])(maybeTen) == identity(maybeTen))
    assert(fmap(identity[Int])(maybeNothing) == identity(maybeNothing))

    val v1 = fmap(ff _ andThen gg)(Just(1))
    val v2 = (fmap(ff) _ andThen fmap(gg)) (Just(1))
    println(s"$v1 $v2")

    val a = for {
      v1 ← maybeTen
      v2 ← maybeTest
    } yield v1.toString ++ v2

    println(a)

    import MonadMaybe._

    val a1 = flatMap(pure(3))(f)
    val a2 = f(3)
    println(s"$a1 $a2")

    val m = Just(1)
    val b1 = flatMap(m)(pure)
    val b2 = m
    println(s"$b1 $b2")

    val c1 = (m flatMap f) flatMap g
    val c2 = m flatMap (f(_) flatMap g)

    println(s"$c1 $c2")

  }
