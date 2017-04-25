import java.util.regex.Pattern
import scala.annotation.tailrec

object Anagrams extends App {
	/** A word is simply a `String`. */
	type Word = String

	/** A sentence is a `List` of words. */
	type Sentence = List[Word]

	/** A fingerprint is a string which represents a sorted sequence of characters:
	  * Examples:
	  *
	  * "aaccx"
	  * "abyz"
	  * "ppp"
	  * ""
	  */
	type FingerPrint = String


	/** The dictionary is simply a sequence of words.
	  * You can begin your development with this simple example.
	  * A dictionary of English words is given to you as an external file (linuxwords.txt)
	  * that you can load to use with your program
	  */
	val dictionary: List[Word] =
		List("ate", "eat", "tea", "pot", "top", "sonja", "jason", "normal",
			"I", "love", "you", "olive")


	/** Converts a word/sentence into its fingerprint.
	  * The fingerprint has the same characters as the word, with the same
	  * number of occurrences, but the characters appear in sorted order.
	  */
	def fingerPrint(s: Word): FingerPrint = s.toLowerCase.sorted
	def fingerPrint(s: Sentence): FingerPrint = fingerPrint(("" /: s) (_ + _))


	/** `matchingWords` is a `Map` from fingerprints to a sequence of all
	  * the words that have that fingerprint.
	  * This map serves as an easy way to obtain all the anagrams of a word given its fingerprint.
	  *
	  * For example, the word "eat" has the fingerprint "aet".
	  * Incidentally, so do the words "ate" and "tea".
	  *
	  * This means that the `matchingWords` map will contain an entry:
	  *
	  * "aet"-> List("ate", "eat", "tea")
	  */
	val matchingWords: Map[FingerPrint, List[Word]] = dictionary.groupBy(fingerPrint)

	/** Returns all the anagrams of a given word. */
	def wordAnagrams(word: Word): List[Word] = matchingWords(fingerPrint(word))

	// Test code with for example:
	//println(wordAnagrams("eta"))
	//println(wordAnagrams("jbdikb"))


	/** Returns the list of all subsequences of a fingerprint.
	  * This includes the fingerprint itself, i.e.
	  * "ko" is a subsequence of "kkoo". It also always includes
	  * the empty string "".
	  *
	  * Example: the subsequences of the fingerprint "aabb" are
	  *
	  * List("", "a", "aa", "b", "bb", "ab", "aab", "abb", "aabb")
	  *
	  * Note that the order of the subsequences does not matter -- the subsequences
	  * in the example above could have been displayed in some other order.
	  */
	def subseqs(fp: FingerPrint): List[FingerPrint] = {
		def generate(partialFp: FingerPrint): List[FingerPrint] = {
			if (partialFp.isEmpty) {
				List("")
			} else {
				val seqs = generate(partialFp.tail)
				val head = partialFp.head
				seqs ++ seqs.map(head + _)
			}
		}
		generate(fp).distinct
	}


	// Test code with for example:
	//println(subseqs("aabb"))


	/** Subtracts fingerprint `y` from fingerprint `x`.
	  *
	  * The precondition is that the fingerprint `y` is a subsequence of
	  * the fingerprint `x` -- any character appearing in `y` must
	  * appear in `x`.
	  */
	def subtract(x: FingerPrint, y: FingerPrint): FingerPrint = {
		require(subseqs(x).contains(y), s"`$y` must be a subsequence of `$x`")
		@tailrec
		def removeOne(a: FingerPrint, b: FingerPrint): FingerPrint = {
			// Removes one occurrence of the first character from b in a until b is empty
			// The String#replaceFirst is actually taking a regex pattern as first argument,
			// using Pattern.quote is the correct thing to do here even if not strictly
			// necessary for our specific use-case.
			if (b.isEmpty) a
			else removeOne(a.replaceFirst(Pattern.quote(b.take(1)), ""), b.tail)
		}
		removeOne(x, y)
	}

	// Test code with for example:
	//println(subtract("aabbcc", "abc")) //


	/** Returns a list of all anagram sentences of the given sentence.
	  *
	  * An anagram of a sentence is formed by taking the fingerprint of all the characters of
	  * all the words in the sentence, and producing all possible combinations of words with those characters,
	  * such that the words have to be from the dictionary.
	  *
	  * The number of words in the sentence and its anagrams does not have to correspond.
	  * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
	  *
	  * Also, two sentences with the same words but in a different order are considered two different anagrams.
	  * For example, sentences `List("You", "olive")` and `List("olive","you")` are different anagrams of
	  * `List("I", "love", "you")`.
	  *
	  * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
	  * so it has to be returned in this list.
	  *
	  * Note: There is only one anagram of an empty sentence.
	  */
	def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
		def compose(fp: FingerPrint, sentence: List[Word] = Nil): List[Sentence] = fp match {
			case "" =>
				// If the remaining fingerprint is empty, we have successfully composed an anagram!
				// Actually, the sentence is in reverse order, but since words order does not matter
				// in the final result (every possible ordering will be returned anyway), there is
				// no need to reverse the list at this point
				List(sentence)
			case _ =>
				// If there is some remaining char in the fingerprint, we first compute the subseqs
				// of this fingerprint to be able to identify words that could be used to compose
				// the sentence (their fingerprint must be a subsequence of the remaining fp), then:
				// - For each word in the dictionary
				// - We compute its fingerprint
				// - If this fingerprint is a subsequence of the remaining available fingerprint
				// - Then we attempt to use the word to form a sentence, adding the word to the
				//   current sentence and substracting fingerprints
				// - The recursive call will produce a list of every acceptable anagrams using
				//   the current word at the current position, we then simply return the list
				//   of all these anagrams (a sneaky flatMap is hidden somewhere there!)
				val candidates = subseqs(fp)
				for {
					word <- dictionary
					wfp = fingerPrint(word)
					if candidates.contains(wfp)
					anagram <- compose(subtract(fp, wfp), word :: sentence)
				} yield anagram
		}
		compose(fingerPrint(sentence)).distinct
	}

	// Test code with for example:
	println(sentenceAnagrams(List("eat", "tea")))
	println(sentenceAnagrams(List("you", "olive")))
	println(sentenceAnagrams(List("I", "love", "you")))
	println(sentenceAnagrams(List()))
}
