package com.github.everpeace

import scalaz._
import Scalaz._

/**
 * BiKleisli arrow
 * W: comonad
 * M: monad
 */
trait BiKleisli[W[_], M[_], A, B] {
  def apply(wa: W[A]): M[B]

  import BiKleislis.{bikleisli, bikleisliFn}

  /**
   * bikleisli arrow composition
   * f ## g = cojoin;W(f);t;M(g);join
   *        = m.bind(t(w.fmap(w.cojoin(_),this)),g)
   *        = t(_ =>> this) >>= g
   */
  def ##[C](g: BiKleisli[W, M, B, C])(implicit w: Comonad[W], m: Monad[M], t: Distributes[W, M]): BiKleisli[W, M, A, C] = bikleisli((wa: W[A]) => t(wa =>> this) >>= g)

}

object BiKleislis {
  /**
   * constructor
   */
  def bikleisli[W[_], M[_], A, B](f: W[A] => M[B])(implicit w: Comonad[W], m: Monad[M], t: Distributes[W, M]): BiKleisli[W, M, A, B] = new BiKleisli[W, M, A, B] {
    def apply(wa: W[A]) = f(wa)
  }

  /* alias for constructor */
  // ★ for CoKleisli and ☆ for Kleisli in Scalaz
  def ★☆[W[_], M[_], A, B](f: W[A] => M[B])(implicit w: Comonad[W], m: Monad[M], t: Distributes[W, M]) = bikleisli(f)

  /**
   * implicit conversion: bikleisli to function
   */
  implicit def bikleisliFn[W[_], M[_], A, B](bk: BiKleisli[W, M, A, B]): W[A] => M[B] = bk(_)

  /**
   * implicit conversion: bikleisli to scalaz.MAB
   */
  implicit def bikleislimab[W[_], M[_], A, B](bk: BiKleisli[W, M, A, B]) = mab[({type λ[α, β] = BiKleisli[W, M, α, β]})#λ, A, B](bk)


  /**
   * implicit bikleisli category (for MAB methods)
   */
  implicit def BiKleisliCategory[W[_], M[_]](implicit w: Comonad[W], m: Monad[M], t: Distributes[W, M]): Category[({type λ[α, β] = BiKleisli[W, M, α, β]})#λ]
  = new Category[({type λ[α, β] = BiKleisli[W, M, α, β]})#λ] {
    /**
     * id = η○ε
     */
    def id[A] = bikleisli((wa: W[A]) => m.pure(w.copure(wa)))

    /**
     * compose(f,g) = g ## f
     */
    def compose[A, B, C](f: BiKleisli[W, M, B, C], g: BiKleisli[W, M, A, B]) = g ## f
  }
}
