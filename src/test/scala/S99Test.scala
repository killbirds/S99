import org.scalatest._
import org.scalatest.matchers._

import scala.annotation._

class S99Test extends FunSpec with ShouldMatchers {

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

	def flatten(lists: List[_]): List[_] = {
		lists.foldLeft(List[Any]()) { case (result, entry) =>
			entry match {
				case entries: List[_] =>
					result ++ flatten(entries)
				case entry =>
					result :+ entry
			}
		}
	}

	def compress[T](list: List[T]): List[T] = 
		list.foldLeft(list.head :: Nil) { case (result, entry) =>
			if (result.head == entry) {
				result
			}else {
				entry :: result
			}
		}.reverse

	def pack[T](list: List[T]): List[List[T]] = {
		list.span { _ == list.head } match {
			case (s, Nil) =>
				s :: Nil
			case (s, d) =>
				s :: pack(d)
		}
	}

	def encode[T](list: List[T]): List[(Int, T)] = {
		pack(list).map { list =>
			(list.size, list.head)
		}
	}

	describe("1 to 10 problems") {

		it("1. Find the last element of a list.") {
			last(List(1, 1, 2, 3, 5, 8)) should be (8)
		}

		it("2. Find the last but one element of a list.") {
			penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
		}

		it("3. Find the Kth element of a list.") {
			info("By convention, the first element in the list is element 0.")
			nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
		}

		it("4. Find the number of elements of a list.") {
			length(List(1, 1, 2, 3, 5, 8)) should be (6)
		}

		it("5. Reverse a list.") {
			reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
		}

		it("6. Find out whether a list is a palindrome.") {
			isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
		}

		it("7. Flatten a nested list structure.") {
			flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
		}

		it("8. Eliminate consecutive duplicates of list elements.") {
			info("If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.")

			compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))


		}

		it("9. Pack consecutive duplicates of list elements into sublists.") {
			info("If a list contains repeated elements they should be placed in separate sublists.")
			pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))

		}

		it("10. Run-length encoding of a list.") {
			info("Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.")

			encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		}
	}

	def encodeModified[T](list: List[T]): List[_] = {
		encode(list).map { case (s, e) =>
			if(s == 1) e else (s,e) 
		}
	}

	def decode[T](list: List[(Int, T)]): List[T] = {
		list.foldLeft(List[T]()) { case (result, (size, entry)) =>
			result ::: (0 until size).map{index => entry}.toList
		}
	}

	describe("11 to 20 problems") {
		it("11. Modified run-length encoding.") {
			info("Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.")
			encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
		}

		it("12. Decode a run-length encoded list.") {
			info("Given a run-length code list generated as specified in problem P10, construct its uncompressed version.")
			decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
		}

	}
} 