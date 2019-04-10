package arrow.recursion.typeclasses

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.tuple2.applicative.applicative
import arrow.core.extensions.tuple2.functor.functor
import arrow.core.extensions.tuple2.traverse.traverse
import arrow.free.Cofree
import arrow.recursion.*
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import arrow.typeclasses.Monoid
import arrow.typeclasses.Traverse

/**
 * ank_macro_hierarchy(arrow.recursion.typeclasses.Recursive)
 *
 * Typeclass for types that can be generically folded with algebras.
 */
interface Recursive<T, F> {

  fun FF(): Functor<F>

  /**
   * Implementation for project.
   */
  fun T.projectT(): Kind<F, T>

  /**
   * Creates a coalgebra given a functor.
   */
  fun project(): Coalgebra<F, T> = { it.projectT() }

  /**
   * Fold over any datatype using that datatypes base-functor.
   *
   * ```kotlin:ank:playground
   * import arrow.Kind
   * import arrow.data.ListK
   * import arrow.data.k
   * import arrow.recursion.Algebra
   * import arrow.recursion.extensions.listk.recursive.recursive
   * import arrow.recursion.pattern.ListF
   * import arrow.recursion.pattern.ListFPartialOf
   * import arrow.recursion.pattern.fix
   *
   * fun main() {
   *  val sumAlgebra: Algebra<ListFPartialOf<Int>, Int> = { list: Kind<ListFPartialOf<Int>, Int> ->
   *    when (val fa = list.fix()) {
   *      is ListF.NilF -> 0
   *      is ListF.ConsF -> fa.a + fa.tail
   *    }
   *  }
   *
   *  ListK.recursive<Int>().run {
   *    (0..100).toList().k().cata(sumAlgebra).also(::println)
   *  }
   * }
   * ```
   *
   * Note: Not stack-safe. Use [cataM] with a stack-safe monad, like [Eval]
   */
  fun <A> T.cata(alg: Algebra<F, A>): A = hylo(alg, project(), FF())

  /**
   * Fold monadically over any datatype using that datatypes base-functor.
   *
   * Can be used to get a stack-safe version of [cata] when the monad itself is stack-safe.
   *
   * ```kotlin:ank:playground
   * import arrow.Kind
   * import arrow.core.*
   * import arrow.core.extensions.eval.monad.monad
   * import arrow.data.ListK
   * import arrow.data.k
   * import arrow.recursion.AlgebraM
   * import arrow.recursion.extensions.listk.recursive.recursive
   * import arrow.recursion.pattern.ListF
   * import arrow.recursion.pattern.ListFPartialOf
   * import arrow.recursion.pattern.fix
   * import arrow.recursion.extensions.listf.traverse.traverse
   *
   * fun main() {
   *  val sumAlgebra: AlgebraM<ListFPartialOf<Int>, ForEval, Int> = { list: Kind<ListFPartialOf<Int>, Int> ->
   *    when (val fa = list.fix()) {
   *      is ListF.NilF -> Eval.now(0)
   *      is ListF.ConsF -> Eval.now(fa.a + fa.tail)
   *    }
   *  }
   *
   *  ListK.recursive<Int>().run {
   *    (0..100).toList().k().cataM(ListF.traverse(), Eval.monad(), sumAlgebra).value().also(::println)
   *  }
   * }
   * ```
   */
  fun <M, A> T.cataM(TF: Traverse<F>, MM: Monad<M>, alg: (Kind<F, A>) -> Kind<M, A>): Kind<M, A> =
    hyloM(alg, project() andThen MM::just, TF, MM)

  /**
   * Fold over any datatype using that datatypes base-functor.
   * Also gives access to the current element and not only the fold.
   *
   * ```kotlin:ank:playground
   * import arrow.core.ForOption
   * import arrow.core.fix
   * import arrow.recursion.RAlgebra
   * import arrow.recursion.extensions.recursive
   *
   * fun main() {
   *  val printAsListAlg: RAlgebra<ForOption, Int, String> = { opt ->
   *    opt.fix().fold(ifEmpty = {
   *      "[]"
   *    }, ifSome = { (curr, fold) ->
   *      "$curr : $fold"
   *    })
   *  }
   *
   *  Int.recursive().run {
   *    10.para(printAsListAlg).also(::println)
   *  }
   * }
   * ```
   *
   * Note: Not stack-safe. Use [paraM] with a stack-safe monad, like [Eval]
   */
  fun <A> T.para(alg: RAlgebra<F, T, A>): A =
    hyloC({
      FF().run { it.map { it.fix() } }.let(alg)
    }, project() andThen { FF().run { it.map { it toT it } } },
      FF(), Tuple2.functor()
    )

  /**
   * Fold monadically over any datatype using that datatypes base-functor.
   * Also gives access to the current element and not only the fold.
   *
   * Can be used to get a stack-safe version of [para] when the monad itself is stack-safe.
   *
   * ```kotlin:ank:playground
   * import arrow.core.*
   * import arrow.core.extensions.eval.monad.monad
   * import arrow.core.extensions.monoid
   * import arrow.core.extensions.option.traverse.traverse
   * import arrow.recursion.RAlgebraM
   * import arrow.recursion.extensions.recursive
   *
   * fun main() {
   *  val printAsListAlg: RAlgebraM<ForOption, ForEval, Int, String> = { opt ->
   *    opt.fix().fold(ifEmpty = {
   *      Eval.now("[]")
   *    }, ifSome = { (curr, fold) ->
   *      Eval.now("$curr : $fold")
   *    })
   *  }
   *
   *  Int.recursive().run {
   *    10.paraM(Option.traverse(), Eval.monad(), Int.monoid(), printAsListAlg).value().also(::println)
   *  }
   * }
   * ```
   */
  fun <M, A> T.paraM(
    TF: Traverse<F>,
    MM: Monad<M>,
    MT: Monoid<T>,
    alg: RAlgebraM<F, M, T, A>
  ): Kind<M, A> =
    hyloMC({
      alg(FF().run { it.map { it.fix() } })
    }, project() andThen { FF().run { it.map { it toT it } } } andThen MM::just, TF, Tuple2.traverse(), Tuple2.applicative(MT), MM)

  /**
   * Fold over any datatype using that datatypes base functor.
   * Also gives access to all previous folds inside Cofree.
   *
   * ```kotlin:ank:playground
   * import arrow.core.ForOption
   * import arrow.core.fix
   * import arrow.recursion.CVAlgebra
   * import arrow.recursion.extensions.recursive
   *
   * fun main() {
   *  // fib 0 = 0
   *  // fib 1 = 1
   *  // fib n = fib (n - 1) + fib (n - 2)
   *  val fibAlg: CVAlgebra<ForOption, Int> = { opt ->
   *    opt.fix().fold(ifEmpty = { 0 }, ifSome = { nMinus1 ->
   *      nMinus1.tail.value().fix().fold(ifEmpty = { 1 }, ifSome = { nMinus2 ->
   *        println("(${nMinus1.head} + ${nMinus2.head})")
   *        nMinus1.head + nMinus2.head
   *      })
   *    })
   *  }
   *
   *  Int.recursive().run {
   *    10.histo(fibAlg).also(::println)
   *  }
   * }
   * ```
   *
   * Note: Not stack-safe. Use [histoM] with a stack-safe monad, like [Eval]
   */
  fun <A> T.histo(alg: CVAlgebra<F, A>): A =
    hylo<F, T, Cofree<F, A>>({
      Cofree(FF(), alg(it), Eval.now(it))
    }, project(),
      FF()
    ).head

  /**
   * Fold monadically over any datatype using that datatypes base functor.
   * Also gives access to all previous folds inside Cofree.
   *
   * Can be used to get a stack-safe version of [histo] when the monad itself is stack-safe.
   *
   * The following example is shows stack-safety but since the 5000's fibonacci-number is higher than
   *  Long.MAX_VALUE the result is bullshit.
   * ```kotlin:ank:playground
   * import arrow.recursion.CVAlgebraM
   * import arrow.recursion.extensions.recursive
   * import arrow.core.*
   * import arrow.core.extensions.eval.monad.monad
   * import arrow.core.extensions.option.traverse.traverse
   *
   * fun main() {
   *  // fib 0 = 0
   *  // fib 1 = 1
   *  // fib n = fib (n - 1) + fib (n - 2)
   *  val fibAlg: CVAlgebraM<ForOption, ForEval, Long> = { opt ->
   *    opt.fix().fold(ifEmpty = { Eval.now(0L) }, ifSome = { nMinus1 ->
   *      nMinus1.tail.flatMap { nMinus2Opt ->
   *        nMinus2Opt.fix().fold(ifEmpty = { Eval.now(1L) }, ifSome = { nMinus2 ->
   *          println("(${nMinus1.head} + ${nMinus2.head})")
   *          Eval.now(nMinus1.head + nMinus2.head)
   *        })
   *      }
   *    })
   *  }
   *
   *  Long.recursive().run {
   *    5000L.histoM(Option.traverse(), Eval.monad(), fibAlg).value().also(::println)
   *  }
   * }
   * ```
   */
  fun <M, A> T.histoM(TF: Traverse<F>, MM: Monad<M>, alg: CVAlgebraM<F, M, A>): Kind<M, A> =
    MM.run {
      hyloM<F, M, T, Cofree<F, A>>({
        alg(it).map { a -> Cofree(TF, a, Eval.now(it)) }
      }, project() andThen MM::just,
        TF, MM
      ).map { it.head }
    }

  fun <A> T.prepro(trans: FunctionK<F, F>, alg: Algebra<F, A>): A =
    hylo(alg compose trans::invoke, project(), FF())

  fun <A, B> B.elgot(alg: Algebra<F, A>, f: (B) -> Either<A, Kind<F, B>>): A {
    fun h(b: B): A =
      f(b).fold(::identity) { FF().run { alg(it.map(::h)) } }

    return h(this)
  }

  fun <M, A, B> B.elgotM(TF: Traverse<F>, MM: Monad<M>, alg: AlgebraM<F, M, A>, f: (B) -> Either<A, Kind<F, B>>): Kind<M, A> {
    fun h(b: B): Kind<M, A> =
      f(b).fold(MM::just) { MM.run { TF.run { it.traverse(MM, ::h).flatMap(alg) } } }

    return h(this)
  }
}
