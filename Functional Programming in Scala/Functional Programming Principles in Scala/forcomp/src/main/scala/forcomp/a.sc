import scala.Option

/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]

/** `Occurrences` is a `List` of pairs of characters and positive integers saying
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


def wordOccurrences(w: String): List[(Char,Int)] = {
  w.toLowerCase.toList.groupBy(f => f).mapValues(x => x.length).toList.sortBy(x=>x._1).sortBy(x=>x._1)
}

wordOccurrences("PratikKabara")

def sentenceOccurrences(s: List[String]):List[(Char, Int)] = {
  s.flatMap(x => wordOccurrences(x)).groupBy(x=>x._1).mapValues( x=> x.map(y=>y._2)).mapValues(x=> x.sum).toList.sortBy(x=>x._1)
}

sentenceOccurrences(List("my","name","pratik","kabara"))

val dictionary = List("ate","pratik","eat","ate","mine")

lazy val dictionaryByOccurrences: Map[List[(Char,Int)], List[String]] = dictionary.groupBy(x=> wordOccurrences(x)) withDefaultValue(Nil)

dictionaryByOccurrences(wordOccurrences("ateo"))

def charOccurrences(s: List[Char]):List[(Char, Int)] = {
  s.groupBy(x=>x).map(x=>(x._1,x._2.length)).toList
}

val occurrences = wordOccurrences("eaat")
val allElements = occurrences.flatMap(x=> List.fill(x._2)(x._1))

val z = for(
  i <- 0 to allElements.length
) yield allElements.combinations(i).toList.map(x=> charOccurrences(x))

z.flatMap(x=>x).toList


var u=wordOccurrences("attteeo")
val y=wordOccurrences("teo")

val uMap = u.toMap
val yMap = y.toMap

def adjust(x:(Char,Int)) : (Char,Int) ={
  val p = uMap.getOrElse(x._1,0)
  (x._1, p - x._2)
}

(uMap ++ (yMap map adjust)).toList.filter(x=> x._2!=0).sortBy(x=>x._1)

