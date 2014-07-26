package forcomp

import common._

object test {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.test.Word] = List(Aarhus, Aaron, Ababa, aback, ab
                                                  //| aft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased, a
                                                  //| basement, abasements, abases, abash, abashed, abashes, abashing, abasing, ab
                                                  //| ate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, abb
                                                  //| ey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abb
                                                  //| reviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal, 
                                                  //| abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe, 
                                                  //| abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, abe
                                                  //| rrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhorred
                                                  //| , abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding, Ab
                                                  //| idjan, Abigail, Abilene, abilities, ability, abject, abjection, abjections, 
                                                  //| abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated, a
                                                  //| blates, ablating, ablati
                                                  //| Output exceeds cutoff limit.

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = ((w.toLowerCase groupBy (c => c)).toList map (i => (i._1, i._2.length))).sorted
                                                  //> wordOccurrences: (w: forcomp.test.Word)forcomp.test.Occurrences

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldLeft("")(_ + _))
                                                  //> sentenceOccurrences: (s: forcomp.test.Sentence)forcomp.test.Occurrences

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = (dictionary groupBy wordOccurrences).withDefaultValue(List())
                                                  //> dictionaryByOccurrences: => Map[forcomp.test.Occurrences,List[forcomp.test.
                                                  //| Word]]

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
                                                  //> wordAnagrams: (word: forcomp.test.Word)List[forcomp.test.Word]

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case x :: xs =>
      val succs = combinations(xs)
      succs ++ (for {
        c <- 1 to x._2;
        succ <- succs
      } yield (x._1, c) :: succ)
    case Nil => List(Nil)
  }                                               //> combinations: (occurrences: forcomp.test.Occurrences)List[forcomp.test.Occu
                                                  //| rrences]

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = y.foldLeft(x.toMap.withDefaultValue(0)) {
    case (m, (chr, occ)) =>
      val res = m(chr) - occ
      if (res == 0) m - chr else m.updated(chr, res)
  }.toList.sorted                                 //> subtract: (x: forcomp.test.Occurrences, y: forcomp.test.Occurrences)forcomp
                                                  //| .test.Occurrences

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
    *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def iter(occs: Occurrences): List[Sentence] = occs match {
      case x :: xs =>
        for {
          this_comb_occs <- combinations(occs);
          this_word <- dictionaryByOccurrences(this_comb_occs);
          succ <- iter(subtract(occs, this_comb_occs))
        } yield this_word :: succ
      case Nil => List(Nil)
    }

    iter(sentenceOccurrences(sentence))
  }                                               //> sentenceAnagrams: (sentence: forcomp.test.Sentence)List[forcomp.test.Senten
                                                  //| ce]
   val occ=sentenceOccurrences(List("Yes","man")) //> occ  : forcomp.test.Occurrences = List((a,1), (e,1), (m,1), (n,1), (s,1), (
                                                  //| y,1))
   val c=combinations(sentenceOccurrences(List("Yes","man"))).tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
                                                  //> c  : forcomp.test.Occurrences = List((e,1), (m,1), (n,1))
   dictionaryByOccurrences(combinations(subtract(occ,c)).tail.tail.tail.tail.tail.tail.tail.head)
                                                  //> res0: List[forcomp.test.Word] = List(say)
   sentenceAnagrams(List("Yes","man"))            //> res1: List[forcomp.test.Sentence] = List(List(my, en, as), List(my, as, en)
                                                  //| , List(my, sane), List(my, Sean), List(yes, man), List(en, my, as), List(en
                                                  //| , as, my), List(men, say), List(as, my, en), List(as, en, my), List(say, me
                                                  //| n), List(man, yes), List(sane, my), List(Sean, my))
}