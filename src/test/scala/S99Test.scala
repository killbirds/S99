import org.scalatest._
import org.scalatest.matchers._

class S1to10Test extends FunSpec with ShouldMatchers {

	def last[T](list: List[T]): T = {
		list.last
	}

	def penultimate[T](list: List[T]): T = {
		list.init.last
	}

	def nth[T](n: Int, list: List[T]): T = {
		list(n)
	}

	def length[T](list: List[T]): Int = {
		list.size
	}

	def reverse[T](list: List[T]): List[T] = {
		list.reverse
	}

	def isPalindrome[T](list: List[T]): Boolean = {
		list == list.reverse
	}

	describe("1 to 10 problems") {

		it("Find the last element of a list.") {
			last(List(1, 1, 2, 3, 5, 8)) should be (8)
		}

		it("Find the last but one element of a list.") {
			penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
		}

		it("Find the Kth element of a list.") {
			info("By convention, the first element in the list is element 0.")
			nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
		}

		it("Find the number of elements of a list.") {
			length(List(1, 1, 2, 3, 5, 8)) should be (6)
		}

		it("Reverse a list.") {
			reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
		}

		it("Find out whether a list is a palindrome.") {
			isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
		}
	}
}