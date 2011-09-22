package com.github.everpeace

object BiKleisliSample {

  def main(args:Array[String]): Unit = {

    import scalaz.Scalaz._
    import scalaz.Monad._
    import scalaz.Comonad._
    import com.github.everpeace.BiKleislis._
    import com.github.everpeace.DistributiveLaws._

    /** ユーザ：アクセストークンとして用いる */
    case class User(val name: String)

    /** Person:サンプルエンティティ  */
    case class Person(val name: String)

    /**サンプルPerson レポジトリ */
    object PersonRepository {
      def search(s: String): Person = Person(s)
      def update(p: Person, s: String): Person = Person(s)
    }

    case class AccessRejectedError(val message: String)

    // 直積スタンピングコモナド(Tuple2Comonadはscalaz.Comonadでimplicitに定義されている)
    type AccessTokenComonad[X] = (User, X)
    // 直和スタンピングモナド (EitherMonadはscalaz.Monadでimplicitに定義されている)
    type AuthorizationErrorMonad[X] = Either[AccessRejectedError, X]

    // サンプル操作
    val search = (s: String) => PersonRepository.search(s)
    val updateMrY = (p: Person) => PersonRepository.update(p, "Mr.Y")

    // サンプル権限テーブル（どの関数を誰が実行していいかのマップ）
    val authMap: Map[ScalaObject, Map[User, Boolean]]
    = Map(search -> Map(User("bob") -> true, //search は bob, alice 両方OK
                        User("alice") -> true),
          updateMrY -> Map(User("bob") -> true)) //update は bob だけOK


    /**
     * 操作に対する認可関数(上記権限テーブルを参照するような関数)
     */
    def authorize[X, Y](f: X => Y): AccessTokenComonad[X] => Boolean = in => in match {
      case (u: User, x: X) => authMap.getOrElse(f, Map.empty).getOrElse(u, false)
    }

    /**
     * 操作に認可チェックをデコレートする。
     * X=>Y がデコレートされると (User,X) => Either[AccessRejectedError,Y] になる。
     */
    def withAccessCheck[X, Y](f: X => Y): AccessTokenComonad[X] => AuthorizationErrorMonad[Y]
    = in => in match {
      case (u: User, x: X) => if (authorize(f)(u, x)) {
        Right(f(x))
      } else {
        Left(AccessRejectedError(u.name + " is not granted."))
      }
    }

    // BiKleisli 結合
    // (Tuple2Comonad, EitherMonadに対するDistributesはDistributiveLawsでimplicitに定義されている)
    val searchAndUpdateToMrY = ★☆(withAccessCheck(search)) ## ★☆(withAccessCheck(updateMrY))

    // alice はupdateが許可されていない。
    println(searchAndUpdateToMrY(User("alice"), "alice")) // => Left(AccessRejectedError(alice is not granted.))

    // bob は両方認証成功し、処理成功。
    println(searchAndUpdateToMrY(User("bob"), "bob"))     // => Right(Person(Mr.Y))
  }
}