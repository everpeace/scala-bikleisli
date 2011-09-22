package com.github.everpeace

import scalaz.{Monad, Comonad, Distributes}

/**
 *
 * @author everpeace _at_ gmail _dot_ com
 * @date 11/09/22
 */

object DistributiveLaws {

  /**
   * comonad: (A × _)
   * monad: E + _
   */
  implicit def Tuple2EitherDistributes[A, E](implicit w: Comonad[({type λ[α] = (A, α)})#λ], m: Monad[({type λ[α] = Either[E, α]})#λ]): Distributes[({type λ[α] = (A, α)})#λ, ({type λ[α] = Either[E, α]})#λ]
  = new Distributes[({type λ[α] = (A, α)})#λ, ({type λ[α] = Either[E, α]})#λ] {
    /**
     *  A×(E+C) => E+(A×C)
     *  (a,e) -> e
     *  (a,c) -> (a,c)
     */
    def apply[C](f: (A, Either[E, C])): Either[E, (A, C)] = f match {
      case (a, x) => x match {
        case Left(e) => Left(e)
        case Right(c) => Right((a, c))
      }
    }
  }
}