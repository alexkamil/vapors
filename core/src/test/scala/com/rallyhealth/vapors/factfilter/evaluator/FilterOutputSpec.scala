package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example.{FactTypes, Tags}
import com.rallyhealth.vapors.factfilter.data.{Evidence, FactTable}
import org.scalatest.wordspec.AnyWordSpec
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import org.scalatest.matchers.should.Matchers._

class FilterOutputSpec extends AnyWordSpec {

  "Expr.FilterOutput" when {

    "using with an 'OutputWithinSet' op" when {
      val tagFacts = FactTable(Tags.smoker, Tags.asthma, Tags.normalBmi)

      "comparing facts" should {

        "return all matching facts from a given subset" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.filter(_ in Set(Tags.asthma))
          }
          val res = eval(tagFacts)(q)
          res.output.value should contain theSameElementsAs Seq(Tags.asthma)
          assertResult(Evidence(Tags.asthma))(res.output.evidence)
        }

        "return all matching facts from a given superset" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.filter(_ in Set(Tags.asthma, Tags.obeseBmi))
          }
          val res = eval(tagFacts)(q)
          res.output.value should contain theSameElementsAs Seq(Tags.asthma)
          assertResult(Evidence(Tags.asthma))(res.output.evidence)
        }

        "return an empty list of facts when given a set that contains no common elements" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.filter(_ in Set(Tags.obeseBmi))
          }
          val res = eval(tagFacts)(q)
          assert(res.output.value.isEmpty)
          assert(res.output.evidence.isEmpty)
        }
      }

      "comparing values" should {

        "return all matching values from a given subset" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).filter(_ in Set(Tags.asthma).map(_.value))
          }
          val res = eval(tagFacts)(q)
          res.output.value should contain theSameElementsAs Seq(Tags.asthma).map(_.value)
        }

        "return the correct evidence for the matching values from a given subset" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).filter(_ in Set(Tags.asthma).map(_.value))
          }
          val res = eval(tagFacts)(q)
          pendingUntilFixed {
            // TODO: Merge this assertion the above unit test when it passes
            assertResult(Evidence(Tags.asthma))(res.output.evidence)
          }
        }

        "return the matching values from a given superset" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).filter(_ in Set(Tags.asthma, Tags.obeseBmi).map(_.value))
          }
          val res = eval(tagFacts)(q)
          res.output.value should contain theSameElementsAs Seq(Tags.asthma).map(_.value)
        }

        "return the correct evidence for the matching values from a given superset" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).filter(_ in Set(Tags.asthma, Tags.obeseBmi).map(_.value))
          }
          val res = eval(tagFacts)(q)
          pendingUntilFixed {
            // TODO: Merge this assertion the above unit test when it passes
            assertResult(Evidence(Tags.asthma))(res.output.evidence)
          }
        }

        "return an empty list of values when given a set that contains no common elements" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).filter(_ in Set(Tags.obeseBmi).map(_.value))
          }
          val res = eval(tagFacts)(q)
          assert(res.output.value.isEmpty)
          assert(res.output.evidence.isEmpty)
        }
      }

      "using the 'containsAny' op" should {

        "return 'true' when the fact table contains a superset of the given set" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).containsAny(Set(Tags.asthma).map(_.value))
          }
          val res = eval(tagFacts)(q)
          assert(res.output.value)
        }

        "return the correct evidence for the facts that contain a superset of the given set" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).containsAny(Set(Tags.asthma).map(_.value))
          }
          val res = eval(tagFacts)(q)
          pendingUntilFixed {
            // TODO: Merge this assertion the above unit test when it passes
            assertResult(Evidence(Tags.asthma))(res.output.evidence)
          }
        }

        "return 'true' when the fact table contains a subset of the given set" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).containsAny(Set(Tags.asthma, Tags.obeseBmi).map(_.value))
          }
          val res = eval(tagFacts)(q)
          assert(res.output.value)
        }

        "return the correct evidence for the facts that contain a subset of the given set" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).containsAny(Set(Tags.asthma, Tags.obeseBmi).map(_.value))
          }
          val res = eval(tagFacts)(q)
          pendingUntilFixed {
            // TODO: Merge this assertion the above unit test when it passes
            assertResult(Evidence(Tags.asthma))(res.output.evidence)
          }
        }

        "return 'false' with no Evidence when the facts do not contain anything in the given set" in {
          val q = withFactsOfType(FactTypes.Tag).where { facts =>
            facts.map(_.value).containsAny(Set(Tags.obeseBmi).map(_.value))
          }
          val res = eval(tagFacts)(q)
          assert(!res.output.value)
          assert(res.output.evidence.isEmpty)
        }
      }
    }

    "using with an 'OutputWithinRange' operator" should {
      val low = FactTypes.Age(10)
      val middle = FactTypes.Age(18)
      val high = FactTypes.Age(85)
      val numericFacts = FactTable(low, middle, high)

      "return all values that match the condition" in {
        val q = withFactsOfType(FactTypes.Age).where { facts =>
          facts.map(_.value).filter(_ >= middle.value)
        }
        val res = eval(numericFacts)(q)
        res.output.value should contain theSameElementsAs Seq(middle, high).map(_.value)
      }

      "return the correct evidence for the matching values from a given subset" in {
        val q = withFactsOfType(FactTypes.Age).where { facts =>
          facts.map(_.value).filter(_ >= middle.value)
        }
        val res = eval(numericFacts)(q)
        pendingUntilFixed {
          // TODO: Merge this assertion the above unit test when it passes
          assertResult(Evidence(middle, high))(res.output.evidence)
        }
      }

      "return all facts that match the condition" in {
        val q = withFactsOfType(FactTypes.Age).where { facts =>
          facts.filter(_.value >= middle.value)
        }
        val res = eval(numericFacts)(q)
        res.output.value should contain theSameElementsAs Seq(middle, high)
        assertResult(Evidence(middle, high))(res.output.evidence)
      }

      "return an empty list of values when none of the elements meet the condition" in {
        val q = withFactsOfType(FactTypes.Age).where { facts =>
          facts.map(_.value).filter(_ > high.value)
        }
        val res = eval(numericFacts)(q)
        assert(res.output.value.isEmpty)
        assert(res.output.evidence.isEmpty)
      }

      "return an empty list of facts when none meet the condition" in {
        val q = withFactsOfType(FactTypes.Age).where { facts =>
          facts.filter(_.value > high.value)
        }
        val res = eval(numericFacts)(q)
        assert(res.output.value.isEmpty)
        assert(res.output.evidence.isEmpty)
      }
    }
  }
}
