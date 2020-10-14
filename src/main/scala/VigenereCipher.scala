import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

/**
 * Encrypts a message in English to a ciphertext with Vigenere cipher using a supplied key.
 * Decrypts a ciphertext into plaintext by using Friedman test (finding the key length) and letter frequency analysis.
 *
 * Only considers keys of length 2-10.
 *
 * @author Ondřej Holub
 * @note converted from a simple REPL script
 */
object VigenereCipher {
  def main(args: Array[String]): Unit = {

    ////////////////////////////// INPUT
    val rawMessage =
      """I have a dream that one day this nation will rise up and live out the true meaning of its creed:
        | "We hold these truths to be self-evident, that all men are created equal."
        | I have a dream that one day on the red hills of Georgia, the sons of former slaves and the sons of
        | former slave owners will be able to sit down together at the table of brotherhood."""
        .stripMargin
        .replaceAll("\n", " ")
    val key = "SUS"
    val letterFreqDistribution: Vector[(Char, Double)] = Vector(('a', 8.2), ('b', 1.5), ('c', 2.8), ('d', 4.3), ('e', 13.0),
      ('f', 2.2), ('g', 2.0), ('h', 6.1), ('i', 7.0), ('j', 0.15), ('k', 0.77), ('l', 4.0), ('m', 2.4), ('n', 6.7),
      ('o', 7.5), ('p', 1.9), ('q', 0.095), ('r', 6.0), ('s', 6.3), ('t', 9.1), ('u', 2.8), ('v', 0.98), ('w', 2.4),
      ('x', 0.15), ('y', 2.0), ('z', 0.074))
      .map { case (x, y) => (x.toUpper, y / 100) }

    println(s"Plaintext: $rawMessage")
    println(s"Key: $key")
    ////////////////////////////// ENCRYPTING
    // "I love you!" -> ('i', 'l', 'o', 'v', 'e', 'y', 'o', 'u')
    val message: Array[Char] = rawMessage
      .replaceAll("[^A-Za-z]", "")
      .toLowerCase
      .toCharArray

    def zipWithExplodedKey(message: Seq[Char], key: String): Seq[(Char, Char)] = {
      val keyMultiplierWholePart: Int = message.length / key.length
      val keyModuloPart: String = key.slice(0, message.length % key.length)
      val keyExplodedToMessageLength = (key * keyMultiplierWholePart ++ keyModuloPart)
        .toLowerCase
        .toCharArray
      message.zip(keyExplodedToMessageLength)
    }

    def addTwoChars(a: Char, b: Char): Char = {
      val alphabet = 'a'.to('z').toVector
      val twoAlphabets = alphabet ++ alphabet
      val shiftedIndex = twoAlphabets.indexOf(a) + twoAlphabets.indexOf(b)
      twoAlphabets(shiftedIndex)
    }

    // ('i', 'l', 'o', 'v', 'e', 'y', 'o', 'u')
    // ('k', 'e', 'y', 'k', 'e', 'y', 'k', 'e')
    val messageZipKey = zipWithExplodedKey(message, key).toVector

    // ('i', 'l', 'o', 'v', 'e', 'y', 'o', 'u')
    //   +    +    +    +    +    +    +    +
    // ('k', 'e', 'y', 'k', 'e', 'y', 'k', 'e')
    val enc = messageZipKey.map(x => addTwoChars(x._1, x._2).toUpper).mkString("")
    println(s"Ciphertext: $enc")

    ////////////////////////////// DECRYPTING - ESTIMATING KEY LENGTH (based on Friedman test, but estimated from a histogram)
    val coincidencesAbsRel: Seq[(Int, Double)] = enc.indices.drop(1).map { j =>
      val abs = enc.dropRight(j) // [1, n-j]
        .zip(enc.drop(j)) // zip with [j+1, n]
        .count(x => x._1 == x._2) // count coincidences
      (abs, abs.toDouble / (enc.length - j).toDouble) // return (abs, rel = abs/n-j)
    }.dropRight(enc.length / 2)
    println("Aggregate relative coincidence index (key and non key positions): " + coincidencesAbsRel.map(_._2).sum / coincidencesAbsRel.length)

    // consider keys of length between 2 and 10
    val coincidencesNormalizedSums: Seq[(Int, Int)] = 2.to(10).map { i =>
      val indices = (i - 1) until coincidencesAbsRel.length by i // indices of histogram columns that would be elevated if key.length = i (not including the first column where there was no shift and thus all pairs were equal)
      (i, indices
        .map(coincidencesAbsRel(_)._1) // get those columns - coincidence counts
        .sum// sum them
        ./(indices.length)) // normalize the sum by the amount of columns
    }
    println(s"Pairs of (possible key length, normalized coincidences): $coincidencesNormalizedSums)")

    // Naive estimation for picking the first spike in a histogram:
    // "Pick a notably high peak in number of coincidences (compared to the following positions)
    // as early into the list as possible" (since key length of 3 will also "spike" on positions 6, 9, 12...).
    // Threshold 1.2 was picked as a very rough estimate based on a few observations,
    // it looks to be low enough to allow us to distinguish a "peak" column from a "valley" column,
    // and high enough not to consider further spikes after the 1st one
    val keyLength = coincidencesNormalizedSums.reduce((acc, b) => if (b._2 >= 1.2 * acc._2) b else acc)._1
    println(s"Key length: $keyLength")

    val keyBucket = (keyLength - 1)
      .until(coincidencesAbsRel.length - 1)
      .by(keyLength)
      .map(coincidencesAbsRel(_)._2)

    // english coincidence index ~ 0.065
    // czech coincidence index ~ 0.058
    val coincidenceIndex = keyBucket.sum / keyBucket.length
    println(s"Coincidence index of key columns: $coincidenceIndex")

    ////////////////////////////// DECRYPTING - KEY LENGTH FOUND, USE FREQUENCY ANALYSIS
    // break into buckets
    val letterBucketsBuffer: ListBuffer[Vector[Char]] = ListBuffer.empty
    for (i <- 0 until keyLength) {
      val indexesInBucket = List.range(i, enc.length - 1, keyLength)
      val lettersInBucket: Vector[Char] = indexesInBucket.map(enc(_)).toVector
      letterBucketsBuffer += lettersInBucket
    }

    def turnWheel(wheel: Seq[Any], steps: Int): Seq[Any] = {
      val turnedWheel = wheel.takeRight(steps) ++: wheel
      turnedWheel.take(wheel.length)
    }

    def getAbsoluteDelta(a: Double, b: Double): Double = {
      if (a >= b) a - b else b - a
    }

    val letterBuckets = letterBucketsBuffer.toList
    val outputKey: String = letterBuckets.map { bucket =>
      val bucketTotalLetters = bucket.toArray.length
      val bucketCounts = bucket.toArray.foldLeft(Map.empty[Char, Int]) { (acc, char) => acc.addOne((char, acc.getOrElse(char, 0) + 1)) }
      // normalized bucket distribution
      val bucketDistribution = bucketCounts.map { case (x, y) => (x -> y.toDouble / bucketTotalLetters.toDouble) }
      // ensure all letter in the alphabet are in the distribution
      letterFreqDistribution.foreach { case (char: Char, f: Double) =>
        if (!bucketDistribution.contains(char)) bucketDistribution.update(char, 0)
      }
      // for each letter in the alphabet zip sorted global distribution with SORTED, SHIFTED and NORMALIZED bucket distribution, and measure total delta
      val deviationHeuristicPerLetter = letterFreqDistribution.indices.map(i => turnWheel(bucketDistribution.toVector.sortBy(_._1), i).asInstanceOf[Vector[(Char, Double)]].map(_._2).zip(letterFreqDistribution.map(_._2)).map { case (f1, f2) => getAbsoluteDelta(f1, f2) }.sum)
      val lowestDeviationIndex = deviationHeuristicPerLetter.indexOf(deviationHeuristicPerLetter.min)
      turnWheel(letterFreqDistribution.reverse, 1).asInstanceOf[Vector[(Char, Double)]].apply(lowestDeviationIndex)._1
    }.mkString("")
    println(s"Estimated key: $outputKey")

    def subtractTwoChars(a: Char, b: Char): Char = {
      val alphabet = 'A'.to('Z').toVector
      val twoAlphabets = alphabet ++ alphabet
      val shiftedIndex = twoAlphabets.lastIndexOf(a) - twoAlphabets.indexOf(b)
      twoAlphabets(shiftedIndex)
    }

    val outputMessage: String = zipWithExplodedKey(enc, outputKey).map(x => subtractTwoChars(x._1, x._2.toUpper)).mkString("").toLowerCase()
    println(s"Discovered text: $outputMessage")
  }
}