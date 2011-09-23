package com.github.everpeace

/**
 * 
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
   ★☆(OK)

   // 関数の型を生のタプルとEitherで定義。
   // NG を★☆に渡した際にエラー
   def NG: (User, Person) => Either[AccessRejectedError, Person]
     = (u, p) => Left(AccessRejectedError("rejected"))

   //これはOK
   ★☆(OK)

   // エラー
   // found   : (User, Person) => Either[AccessRejectedError,Person]
   // required: ? => ?
   ★☆(NG)

   // エラー
   // ★☆に型パラメータを指定して読んでやると型が違うとメッセージがでるが、メッセージでは同じように見える。。。
   // found   : (User, Person) => Either[AccessRejectedError,Person]
   // required: (User, Person) => Either[AccessRejectedError,Person]
   ★☆[PartialApply1Of2[Tuple2,User]#Apply, PartialApply1Of2[Either,AccessRejectedError]#Apply,Person,Person](NG)
 }
}