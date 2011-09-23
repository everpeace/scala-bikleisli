package com.github.everpeace

/**
 * original file is located at https://gist.github.com/1236981
 * commented by @kmizu
 * @author everpeace _at_ gmail _dot_ com
 * @date 11/09/23
 */

class BiKleisliSampleTypeError {
  def main(args:Array[String]):Unit = {
   import scalaz._
   import Scalaz._
   import scalaz.Comonad._ // ScalazにComonadはミックスインされてないので。
   import BiKleislis._
   import DistributiveLaws._

   case class User(val name: String)                   //アクセストークンとして用いる
   case class Person(val name: String)                 // Person:サンプルエンティティ
   case class AccessRejectedError(val message: String) // アクセス拒否エラーメッセージ

   type AccessTokenComonad[X] = (User, X)                            // Userを直積スタンピングするコモナド
   type AuthorizationErrorMonad[X] = Either[AccessRejectedError, X]  // エラーを直和スタンピングするモナド

   // 関数の型を上で宣言した型エイリアスで定義。
   // OKを★☆に渡した際にはエラーにならない。
   def OK: AccessTokenComonad[Person] => AuthorizationErrorMonad[Person] = in => in match{
       case (u,p) => Left(AccessRejectedError("rejected."))
     }

   // 関数の型を生のタプルとEitherで定義。
   // NG を★☆に渡した際にエラー
   // by @kmizu: Scalaでは、「2要素のタプルを受け取る関数」と「2引数の関数」が異なるので、NGを「2要素のタプルを受け取る関数」を返すメソッドとして定義
   def NG: ((User, Person)) => Either[AccessRejectedError, Person]
     = { case (u, p) => Left(AccessRejectedError("rejected")) }

   // これはOK
   // ★☆の定義をBiKleisli.scalaから転載
   // def ★☆[W[_], M[_], A, B](f: W[A] => M[B])(implicit w: Comonad[W], m: Monad[M], t: Distributes[W, M]): BiKleisli[W, M, A, B] = bikleisli(f)
   ★☆(OK)

   // エラー
   // found   : (User, Person) => Either[AccessRejectedError,Person]
   // required: ? => ?
   // by @kmizu: 
   // 修正後は、普通の型エラーに変わる。これは、★☆の型コンストラクタパラメータであるW, Mをこのケースではうまく推論できないため
   // Tuple2は2引数型コンストラクタなので、↓のように明示的に型コンストラクタを部分適用しないと推論できない
   //★☆(NG)

   // エラー
   // ★☆に型パラメータを指定して読んでやると型が違うとメッセージがでるが、メッセージでは同じように見える。。。
   // found   : (User, Person) => Either[AccessRejectedError,Person]
   // required: (User, Person) => Either[AccessRejectedError,Person]
   // by @kmizu: Scalaでは、「2要素のタプルを受け取る関数」と「2引数の関数」が異なるために、エラーが起きていた
   // required は、正確には
   // required: ((User, Person)) => Either[AccessRejectedError,Person]
   // であるべき
   ★☆[PartialApply1Of2[Tuple2,User]#Apply, PartialApply1Of2[Either,AccessRejectedError]#Apply,Person,Person](NG)
 }
}