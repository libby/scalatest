/*
 * Copyright 2001-2014 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalactic.algebra

import org.scalacheck.Arbitrary
import org.scalactic.Or.B
import org.scalactic.{Good, Or, UnitSpec}
import org.scalatest.laws.MonadLaws

import scala.language.implicitConversions

class MonadSpec extends UnitSpec {

  "A Monad" should "offer a flatten method" in {
    class ListMonad extends Monad[List] {
      override def flatMap[A, B](ca: List[A])(f: (A) => List[B]): List[B] = ca.flatMap(f)
      override def insert[A](a: A): List[A] = List(a)
    }

    implicit val listMonad = new ListMonad

    Monad[List].flatten(List(List(1, 2), List(3, 4), List(5, 6))) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  it should "provide an instance for List" in {
    new MonadLaws[List].assert()
  }
  
  it should "provide an instance for Option" in {
    new MonadLaws[Option].assert()
  }
  
  it should "provide an instance for Vector" in {
    new MonadLaws[Vector].assert()
  }
  
  it should "provide an instance for Every for One" in {
    import org.scalactic.Every
    import Arbitrary.arbitrary
    implicit def arbEveryOne[A](implicit arbA: Arbitrary[A]): Arbitrary[Every[A]] = Arbitrary(for (a <- arbitrary[A]) yield Every(a))
    new MonadLaws[Every].assert()
  }
  
  it should "provide an instance for Every for Many" in {
    import org.scalactic.Every
    import Arbitrary.arbitrary
    implicit def arbEveryMany[A](implicit arbA: Arbitrary[A]): Arbitrary[Every[A]] = Arbitrary(for (a <- arbitrary[A]; as <- arbitrary[List[A]]) yield Every(a, as: _*))
    new MonadLaws[Every].assert()
  }
  
  it should "provide an instance for Try over the Success side" in {
    import scala.util.Try
    implicit def arbTry[A](implicit arbA: Arbitrary[A]): Arbitrary[Try[A]] = Arbitrary(for (a <- Arbitrary.arbitrary[A]) yield Try(a))
    new MonadLaws[Try].assert()
  }
  
  it should "provide an instance for Stream " in {
    new MonadLaws[Stream].assert()
  }
  
  it should "provide an instance for Chain " in {
    import org.scalactic.Chain
    import Arbitrary.arbitrary
    implicit def arbChain[A](implicit arbA: Arbitrary[A]): Arbitrary[Chain[A]] = Arbitrary(for (a <- arbitrary[A]; as <- arbitrary[List[A]]) yield Chain(a, as: _*))
    new MonadLaws[Chain].assert()
  }
  
  it should "provide an instance for Or, which abstracts over the Good side" in {
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))
    new MonadLaws[Or.B[Int]#G].assert()
  }

  "A Monad Adapter" should "offer a flatten method" in {
    class ListMonad extends Monad[List] {
      override def flatMap[A, B](ca: List[A])(f: (A) => List[B]): List[B] = ca.flatMap(f)
      override def insert[A](a: A): List[A] = List(a)
    }

    implicit val listMonad = new ListMonad

    val adapted = new Monad.Adapter[List, List[Int]]((List(List(1, 2), List(3, 4), List(5, 6))))
    adapted.flatten shouldEqual List(1, 2, 3, 4, 5, 6)
  }
  
}

