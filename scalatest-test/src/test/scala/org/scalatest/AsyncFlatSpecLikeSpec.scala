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
package org.scalatest

import SharedHelpers.EventRecordingReporter
import scala.concurrent.{Future, ExecutionContext}

class AsyncFlatSpecLikeSpec extends FunSpec {

  describe("AsyncFlatSpecLike") {

    it("can be used for tests that return Future") {

      class ExampleSpec extends AsyncFlatSpecLike {

        implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

        val a = 1

        it should "test 1" in {
          Future {
            assert(a == 1)
          }
        }

        it should "test 2" in {
          Future {
            assert(a == 2)
          }
        }

        it should "test 3" in {
          Future {
            pending
          }
        }

        it should "test 4" in {
          Future {
            cancel
          }
        }

        it should "test 5" ignore {
          Future {
            cancel
          }
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(rep.testStartingEventsReceived.length == 4)
      assert(rep.testSucceededEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived(0).testName == "should test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "should test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "should test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "should test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "should test 5")
    }

    it("can be used for tests that did not return Future") {

      class ExampleSpec extends AsyncFlatSpecLike {

        implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

        val a = 1

        it should "test 1" in {
          assert(a == 1)
        }

        it should "test 2" in {
          assert(a == 2)
        }

        it should "test 3" in {
          pending
        }

        it should "test 4" in {
          cancel
        }

        it should "test 5" ignore {
          cancel
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(rep.testStartingEventsReceived.length == 4)
      assert(rep.testSucceededEventsReceived.length == 1)
      assert(rep.testSucceededEventsReceived(0).testName == "should test 1")
      assert(rep.testFailedEventsReceived.length == 1)
      assert(rep.testFailedEventsReceived(0).testName == "should test 2")
      assert(rep.testPendingEventsReceived.length == 1)
      assert(rep.testPendingEventsReceived(0).testName == "should test 3")
      assert(rep.testCanceledEventsReceived.length == 1)
      assert(rep.testCanceledEventsReceived(0).testName == "should test 4")
      assert(rep.testIgnoredEventsReceived.length == 1)
      assert(rep.testIgnoredEventsReceived(0).testName == "should test 5")
    }

  }

}