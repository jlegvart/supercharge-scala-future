package exercises.action.imperative

import java.time.{Instant, LocalDate}

import exercises.action.imperative.UserCreationExercises._
import exercises.action.DateGenerator._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.UserCreationExercisesTest
class UserCreationExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("readSubscribeToMailingList example") {
    forAll { yesNo: Boolean =>
      val inputs  = ListBuffer(formatYesNo(yesNo))
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val result  = readSubscribeToMailingList(console)

      assert(result == yesNo)
      assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
    }
  }

  test("readSubscribeToMailingList example failure") {
    val console = Console.mock(ListBuffer("Never"), ListBuffer())
    val result  = Try(readSubscribeToMailingList(console))

    assert(result.isFailure)
  }

  test("readDateOfBirth example success") {
    forAll { dateOfBirth: LocalDate =>
      val console = Console.mock(ListBuffer(dateOfBirthFormatter.format(dateOfBirth)), ListBuffer())
      val result  = readDateOfBirth(console)

      assert(result == dateOfBirth)
    }
  }

  test("readDateOfBirth example failure") {
    val console = Console.mock(ListBuffer("21/07/1986"), ListBuffer())
    val result  = Try(readDateOfBirth(console))

    assert(result.isFailure)
  }

  test("readUser example") {
    val inputs  = ListBuffer("Eda", "Y", "N", "18-03-2001", "Never", "/", "Y")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val time    = Instant.ofEpochSecond(234234234)
    val clock   = Clock.constant(time)
    val result  = readUser(console, clock)

    val expected = User(
      name = "Eda",
      dateOfBirth = LocalDate.of(2001, 3, 18),
      subscribedToMailingList = true,
      createdAt = time
    )

    assert(result == expected)
  }

  // ////////////////////////////////////////////
  // PART 2: Error handling
  // ////////////////////////////////////////////

  test("readSubscribeToMailingListRetry negative maxAttempt") {
    forAll(negativeValue) { attempt =>
      val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
      val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = attempt))

      assert(result.isFailure)
    }
  }

  test("readSubscribeToMailingListRetry example success") {
    forAll(randomMaxAttempt, randomStrings, randomBool) {
      (maxAttempts: Int, invalidInputs: List[String], yesNo: Boolean) =>
        val yN      = if (yesNo) "Y" else "N"
        val outputs = ListBuffer.empty[String]
        val inputs  = invalidInputs :+ yN
        val console = Console.mock(ListBuffer.from(inputs), outputs)

        val result = Try(readSubscribeToMailingListRetry(console, maxAttempts))

        val q   = "Would you like to subscribe to our mailing list? [Y/N]"
        val err = """Incorrect format, enter "Y" for Yes or "N" for "No""""

        val n        = (invalidInputs.size + 1).min(maxAttempts)
        val qErrPair = List.fill(n)(List(q, err)).flatten

        if (invalidInputs.size < maxAttempts) {
          assert(result.isSuccess)
          assert(result.get == yesNo)
          assert(outputs.toList == qErrPair.dropRight(1))
        } else {
          assert(result.isFailure)
          assert(outputs.toList == qErrPair)
        }
    }
  }

  /*
  test("readSubscribeToMailingListRetry example invalid input") {
    forAll(randomString) { str =>
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(ListBuffer(str), outputs)
      val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = 1))

      assert(result.isFailure)
      assert(
        outputs.toList == List(
          "Would you like to subscribe to our mailing list? [Y/N]",
          """Incorrect format, enter "Y" for Yes or "N" for "No""""
        )
      )

      // check that the error message is the same as `readSubscribeToMailingList`
      val console2 = Console.mock(ListBuffer(str), ListBuffer.empty[String])
      val result2  = Try(readSubscribeToMailingList(console2))
      assert(result.failed.get.getMessage == result2.failed.get.getMessage)
    }
  }
   */

  test("readDateOfBirthRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readDateOfBirthRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("July 21st 1986", "21-07-1986"), outputs)
    val result  = readDateOfBirthRetry(console, maxAttempt = 2)

    assert(result == LocalDate.of(1986, 7, 21))
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001""",
        """What's your date of birth? [dd-mm-yyyy]"""
      )
    )
  }

  test("readDateOfBirthRetry example failure") {
    val outputs        = ListBuffer.empty[String]
    val invalidAttempt = "July 21st 1986"
    val console        = Console.mock(ListBuffer(invalidAttempt), outputs)
    val result         = Try(readDateOfBirthRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001"""
      )
    )
  }

}
