import org.scalatest._
import org.scalatest.matchers._

import scala.annotation._

import scala.util.Random

import scala.math

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

	def encodeDirect[T](list: List[T]): List[(Int, T)] = {
		var init = (0, list.head)
		list.foldLeft(List[(Int, T)]()) { case (result, entry) =>
			if(init._2 == entry) {
				init = (init._1 + 1, entry)
				result
			}else {
				val nResult = result :+ init
				init = (1, entry)
				nResult
			}
		} :+ init
	}

	def duplicate[T](list: List[T]): List[T] = {
		list.flatMap { entry =>
			List(entry, entry)
		}
	}

	def duplicateN[T](n: Int, list: List[T]): List[T] = {
		list.flatMap { entry =>
			(0 until n).map{i => entry}
		}
	}

	def drop[T](n: Int, list: List[T]): List[T] = {
		list.zipWithIndex.foldLeft(List[T]()) { case (result, (entry, index)) =>

			if((index + 1) % n == 0) {
				result
			} else {
				result :+ entry
			}
		}
	}

	def split[T](n: Int, list: List[T]): (List[T], List[T]) = {
		list.splitAt(n)
	}

	def slice[T](from: Int, to: Int, list: List[T]): List[T] = {
		list.slice(from, to)
	}

	def rotate[T](n: Int, list: List[T]): List[T] = {
		if(n < 0) {
			rotate(list.size + n, list)
		} else {
			list.splitAt(n) match {
				case (f, s) =>
					s ::: f
			}			
		}
	}

	def removeAt[T](n: Int, list: List[T]): (List[T], T) = {
		list.splitAt(n) match {
			case (f, s) =>
				(f ::: s.tail, s.head)		
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

		it("13. Run-length encoding of a list (direct solution).") {
			info("""Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.""")
			encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		}

		it("14. Duplicate the elements of a list.") {
			duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
		}

		it("15. Duplicate the elements of a list a given number of times.") {
			duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
		}

		it("16. Drop every Nth element from a list.") {
			drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
		}

		it("17. Split a list into two parts.") {
			info("The length of the first part is given. Use a Tuple for your result.")

			split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be ((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
		}

		it("18. Extract a slice from a list.") {
			info("Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.")
			slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g))
		}

		it("19. Rotate a list N places to the left.") {
			rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

			rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
		}

		it("20. Remove the Kth element from a list.") {
			info("Return the list and the removed element in a Tuple. Elements are numbered from 0.")

			removeAt(1, List('a, 'b, 'c, 'd)) should be ((List('a, 'c, 'd),'b))
		}
	}

	def insertAt[T](target: T, n: Int, list: List[T]): List[T] = {
		list.patch(n, target :: Nil, 0)
	}

	def range(from: Int, to: Int): List[Int] = {
		(from to to).toList
	}

	def randomSelect[T](n: Int, list: List[T]): List[T] = {
		(for(i <- 0 until n) yield {
			list(Random.nextInt(list.size))
		}).toList
	}

	def lotto(n: Int, max: Int): List[Int] = {
		(0 until n).map { i =>
			Random.nextInt(max)
		}.toList
	}

	def randomPermute[T](list: List[T]): List[T] = {
		list match {
			case Nil => Nil
			case _ =>
				removeAt(Random.nextInt(list.size), list) match {
					case (list, removed) =>
						removed :: randomPermute(list)
				}
		}
		
	}

	def combinations[T](n: Int, list: List[T]): List[List[T]] = {
		list.combinations(n).toList
	}

	// def group3[T](list: List[T]): List[List[List[T]]] = {
	// 	(for{
	// 		a <- list
	// 		b <- list diff List(a)
	// 		c <- list diff List(a, b)
	// 		d <- list diff List(a, b, c)
	// 		e <- list diff List(a, b, c, d)
	// 	} yield {
	// 		val e2 = List(a,b)
	// 		val e3 = List(c,d,e)
	// 		List(e2.toSet, e3.toSet, (list diff e2 diff e3).toSet)
	// 	}).distinct map { _.map (_.toList) }
	// }	

	def group3[T](list: List[T]): List[List[List[T]]] = {
		list.combinations(2).toList.map { e2 =>
			(list diff e2).combinations(3).toList.map { e3 =>
				List(e2, e3, list diff e2 diff e3)
			}
		}.flatten
	}	

	def group[T](n: List[Int], list: List[T]): List[List[List[T]]] = {
		list.combinations(n(0)).toList.map { e2 =>
			(list diff e2).combinations(n(1)).toList.map { e3 =>
				List(e2, e3, list diff e2 diff e3)
			}
		}.flatten
	}

	def lsort[T](list: List[List[T]]): List[List[T]] = {
		list.sortBy(_.size)
	}

	def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
		list.groupBy(_.size).toList.sortBy(_._2.size).map(_._2).flatten
	}

	describe("21 to 30 problems") {
		it("21. Insert an element at a given position into a list.") {
			insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
		}

		it("22. Create a list containing all integers within a given range.") {
			range(4, 9) should be (List(4, 5, 6, 7, 8, 9))
		}

		it("23. Extract a given number of randomly selected elements from a list.") {
			val result = randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h))
			info("" + result)
			result should have size (3)
		}

		it("24. Lotto: Draw N different random numbers from the set 1..M.") {
			val result = lotto(6, 49)
			info("" + result)
			result should have size (6)
		}

		it("25. Generate a random permutation of the elements of a list.") {
			val expected = List('a, 'b, 'c, 'd, 'e, 'f)
			val result = randomPermute(expected)
			info("" + result)
			result should not be (expected)
			result.diff(expected).isEmpty should be (true)
		}

		it("26. Generate the combinations of K distinct objects chosen from the N elements of a list.") {
			info("In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.")
			val result = combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
			info("" + result)
			info("size : " + result.size)
		}

		it("27. Group the elements of a set into disjoint subsets.") {
			info("In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.")
			val source = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
			var result = group3(source)
			result foreach { entry: List[List[String]] =>
				source.size should be (entry.flatten.size)
				source diff entry.flatten should have size (0)
			}
			info("size : " + result.size)

			info("Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.")
			result = group(List(2, 2, 5), source)
			result foreach { entry: List[List[String]] =>
				source.size should be (entry.flatten.size)
				source diff entry.flatten should have size (0)
			}
			info("size : " + result.size)

		}

		it("28. Sorting a list of lists according to length of sublists.") {
			info("We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.") 

			lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should be (List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))

			info("Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.")

			lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should be (List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
		}
	}

	info("Arithmetic")

	class ArithmeticSupport(num: Int) {
		def isPrime: Boolean = num > 1 && (2 to (math.sqrt(num) toInt)).forall(num % _ != 0)

		def isCoprimeTo(other: Int): Boolean = gcd(num, other) == 1

		def totient: Int = {
			if(num == 1) 1
			else {
				(2 to (math.sqrt(num).toInt)).filter(num % _ == 0).size * 2 + 2
			}
		}

		def primeFactors: List[Int] = {
			def prime(num: Int, p: Int): List[Int] = {
				if(num <= p) List(num)
				else if(num % p == 0) {
					p :: prime(num / p, 2)
				}else {
					prime(num, p + 1)
				}
			}
			prime(num, 2)
		}

		def primeFactorMultiplicity: Map[Int, Int] = {
			primeFactors.groupBy(p => p).mapValues{_.size}
		}
	}

	implicit def toArithmeticSupport(num: Int): ArithmeticSupport = new ArithmeticSupport(num)

	def gcd(a: Int, b: Int): Int = {
		if(a > b) gcd(b, a)
		else if(a == 0) b
		else gcd(b % a, a)

	}
	
	describe("31 ~ 40 problems") {
		it("31. Determine whether a given integer number is prime.") {
			7.isPrime should be (true)
		}

		it("32. Determine the greatest common divisor of two positive integer numbers.") {
			gcd(36, 63) should be (9)
		}

		it("33. Determine whether two positive integer numbers are coprime.") {
			info("Two numbers are coprime if their greatest common divisor equals 1.")

			35.isCoprimeTo(64) should be (true)
		}

		it("34. Calculate Euler's totient function phi(m).") {
			info("Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.")
			10.totient should be (4)
		}

		it("35. Determine the prime factors of a given positive integer.") {
			info("Construct a flat list containing the prime factors in ascending order.")

			315.primeFactors should be (List(3,3,5,7))
		}

		it("36. Determine the prime factors of a given positive integer (2).") {
			info("Construct a list containing the prime factors and their multiplicity.")

			315.primeFactorMultiplicity should be (Map(3 -> 2, 5 -> 1, 7 -> 1))
		}

		it("37. Calculate Euler's totient function phi(m) (improved).") {
			pending
		}

		it("38. Compare the two methods of calculating Euler's totient function.") {
			pending	
		}

		it("39. A list of prime numbers.") {
			pending	
		}

		it("40. Goldbach's conjecture.") {
			pending	
		}

		it("41. A list of Goldbach compositions.") {
			pending	
		}
	}

	def and(a: Boolean, b: Boolean) = a & b

	def xor(a: Boolean, b: Boolean) = a ^ b

	def or(a: Boolean, b: Boolean) = a | b


	def table2(fn: Function2[Boolean, Boolean, Boolean]): String = {
		("A     B     result" :: {
			for{
				a <- List(true, false)
				b <- List(true, false)
			} yield {
				"%-5s %-5s %s".format(a, b, fn(a,b))
			}
		}).mkString("\n")
	}

	class LogicSupport(a: Boolean) {
		def and(b: Boolean): Boolean = a & b

		def xor(b: Boolean): Boolean = a ^ b

		def or(b: Boolean): Boolean = a | b
	}

	implicit def toLogicSupport(a: Boolean) = new LogicSupport(a)

	def gray(n: Int): List[String] = {
		if(n == 1) {
			List("0", "1")
		} else {
			val pre = gray(n - 1)

			pre.map { "0" + _ } ::: pre.reverse.map { "1" + _ }
		}
	}

	def huffman(list: List[(String, Int)]): List[(String, Int)] = {
		null
	}

	info("Logic and Codes")
	describe("46 ~ 50 problems") {
		it("46. Truth tables for logical expressions.") {
			and(true, true) should be (true)

			xor(true, true) should be (false)

			table2((a: Boolean, b: Boolean) => and(a, or(a, b))) should be("""|A     B     result
                                                                              |true  true  true
                                                                              |true  false true
                                                                              |false true  false
                                                                              |false false false""".stripMargin)
		}

		it("47. Truth tables for logical expressions (2).") {
			def not(a: Boolean) = !a

			table2((a: Boolean, b: Boolean) => a and (a or not(b))) should be("""|A     B     result
	                                                              |true  true  true
	                                                              |true  false true
	                                                              |false true  false
	                                                              |false false false""".stripMargin)
		}

		it("48. Truth tables for logical expressions (3).") {
			info("Omitted for now.")
		}

		it("49. Gray code.") {
			gray(1) should be (List("0", "1"))
			gray(2) should be (List("00", "01", "11", "10"))
			gray(3) should be (List("000", "001", "011", "010", "110", "111", "101", "100"))
		}

		it("50. Huffman code.") {
			info("First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!")

			info("""We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples. E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)). Our
objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.""")

			huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) should be (List(("a",0), ("b",101), ("c",100), ("d",111), ("e",1101), ("f",1100)))
		}

	}
} 