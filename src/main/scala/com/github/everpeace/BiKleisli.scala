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
  def ★☆[W[_], M[_], A, B](f: W[A] => M[B])(implicit w: Comonad[W], m: Monad[M], t: Distributes[W, M]): BiKleisli[W, M, A, B] = bikleisli(f)

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

  /**implicit bikleisli arrows */
  implicit def BiKleisliArrow[W[_],M[_]](implicit w: Comonad[W], m: Monad[M], t: Distributes[W, M]):Arrow[({type λ[α, β] = BiKleisli[W, M, α, β]})#λ]
  = new Arrow[({type λ[α, β] = BiKleisli[W, M, α, β]})#λ] {
    def arrow[B, C](f: B => C) = ★☆( (wb:W[B]) => f(wb copure) pure )

    /** (W[B] => M[C]) => W[(B,D)] => M[(C,D)] */
    def first[B, C, D](a: ({type λ[α, β] = BiKleisli[W, M, α, β]})#λ[B, C]) =  ★☆((x:W[(B,D)]) => a(wfst(x)) >>= ( (c:C)=> (c , wsnd(x) copure) pure ) )

    /** (W[B] => M[C]) => (W[(D,B)] => M[(D,C)]) */
    def second[B, C, D](a: ({type λ[α, β] = BiKleisli[W, M, α, β]})#λ[B, C]) = ★☆((x:W[(D,B)]) => a(wsnd(x)) >>= ( (c:C)=> (wfst(x) copure, c ) pure ) )

    /** W[(X,Y)] => W[X] */
    def wfst[X,Y](t:W[(X,Y)]):W[X]= w.fmap(t,(x:(X,Y))=>x._1)

    /** W[(X,Y)] => W[Y] */
    def wsnd[X,Y](t:W[(X,Y)]):W[Y]= w.fmap(t,(x:(X,Y))=>x._2)

    val category = BiKleisliCategory
  }
}
