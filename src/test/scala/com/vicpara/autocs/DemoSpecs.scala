package com.vicpara.autocs

import java.io.{ FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream }

import breeze.linalg.SparseVector
import com.vicpara.autocs.learning.Tfidf
import breeze.linalg._
import org.specs2.mutable.Specification

import scala.collection.mutable.ArrayBuffer
import scalax.file.Path

class TfidfSpecs extends Specification {

  "TFIDF logic" should {

    val hardExample = List(
      "one two three three three three six two nine ten",
      "two ten six two one seven",
      "five three two nine",
      "three four six four four four",
      "three four six four four four ten",
      "three six six four two",
      "",
      "eleven",
      "twelve twelve twelve"
    )

    "correctly tokenizes simple sentences" in {
      val test: Map[String, Int] = Map(
        "1,2 3   4 ,5.6!7?8..9" -> 9,
        "1,2" -> 2,
        "1.2" -> 2,
        "1 2" -> 2,
        "1!2" -> 2,
        """1"2""" -> 2,
        "1#2" -> 2,
        "1$2" -> 2,
        "1%2" -> 2,
        "1&2" -> 2,
        "1*2" -> 2,
        "1+2" -> 2,
        "[1]2" -> 2,
        "]1[2" -> 2,
        "{1}2" -> 2,
        "}1{2" -> 2,
        "1(2" -> 2,
        "1)2" -> 2,
        "1:2" -> 2,
        "1;2" -> 2,
        "1<2" -> 2,
        "1>2" -> 2,
        "1?2" -> 2,
        "1=2" -> 2,
        "1\\2" -> 2,
        "1/2" -> 2,
        "1~2" -> 2,
        "1^2" -> 2,
        "  1  2   " -> 2,
        "  1  2   3  " -> 3,
        "   ,   " -> 0,
        "   2   " -> 1,
        "   rick_rick   " -> 1, // _ doesnt splits tokens
        "   rick-rick   " -> 1, // - doesn't split tokens
        "   rick'rick   " -> 1, // ' doesn't split tokens
        "   rick`rick   " -> 1
      ) // ` doesn't split tokens

      val tokens = test.map(d => (d._1, Tfidf.tokenizeDoc(d._1), d._2))
      tokens.foreach(t => {
        if (t._2.size != t._3) {
          println(s"For ${t._1} \t\t\t${t._2.size} must= ${t._3} ... ${t._2.size == t._3}")
          println(t._2)
        }

        t._2.size must_=== t._3
      })
      tokens.count(r => r._2.size == r._3) must_=== test.size
    }

    //    "correctly computes the DocumentFrequency" in {
    //      val corpus = List("1,2,3",
    //        "2,3,4",
    //        "4,5,6",
    //        "1,2,3,4,5,6").zipWithIndex
    //        .map(e => (e._2.toString, e._1.split(",").toIterable))
    //
    //      val utokens = Tfidf.uniqueTokens(sc.parallelize(corpus, numSlices = 1))
    //
    //      utokens.size must_=== 6
    //      utokens.diff(Set("1", "2", "3", "4", "5", "6")).size must_=== 0
    //    }

    def roundAtDecimal(value: Double, decimal: Int) = math.round(value * math.pow(10, decimal)) / math.pow(10, decimal)

    "correctly computes IDF" in {
      val corpusSize = 4
      val idfFun = Tfidf.computeIDF(corpusSize) _

      roundAtDecimal(idfFun(1), 10) must_=== 1.9162907319
      roundAtDecimal(idfFun(2), 10) must_=== 1.5108256238
      roundAtDecimal(idfFun(3), 10) must_=== 1.2231435513
      roundAtDecimal(idfFun(4), 10) must_=== 1
    }

    "correctly computes the TFIDF in the simple case" in {

      val corpus =
        List("one two three three", "two", "five", "three four")
          .zipWithIndex
          .map(e => (e._2.toString, e._1))

      val (tfidf, tfidfTransf) = Tfidf.tfidfRowWise(corpus)
      val tfidfRes = tfidf.toMap

      val tfidf0 = tfidfRes("0")
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 3.02165125
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 1.51082562
      val tfidf1 = tfidfRes("1")
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 1.51082562
      val tfidf2 = tfidfRes("2")
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf3 = tfidfRes("3")
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
    }

    "correctly compute TFIDF on a hard example" in {
      val corpus =
        hardExample
          .zipWithIndex
          .map(e => (e._2.toString, e._1))
      val (tfidf, tfidfTransf) = Tfidf.tfidfRowWise(corpus)
      val tfidfRes = tfidf.toMap

      println(tfidfTransf.uniqTokensIndex.toSeq)

      tfidfTransf.uniqTokensIndex.keys.toSet must_=== Set("one", "two", "three", "four", "five", "six", "seven", "nine", "ten", "eleven",
        "twelve")

      val tfidf0 = tfidfRes("0")
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 6.04330250
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 3.38629436
      val tfidf1 = tfidfRes("1")
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 2.60943791
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 3.38629436
      val tfidf2 = tfidfRes("2")
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 2.60943791
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 1.69314718
      val tfidf3 = tfidfRes("3")
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 7.66516293
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf4 = tfidfRes("4")
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 7.66516293
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf5 = tfidfRes("5")
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 3.02165125
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 1.69314718
      val tfidf6 = tfidfRes("6")
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf7 = tfidfRes("7")
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 2.60943791
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf8 = tfidfRes("8")
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 7.82831374
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
    }

    "correctly compute TFIDF on a hard example" in {
      val corpus =
        hardExample
          .zipWithIndex
          .map(e => (e._2.toString, e._1))
      val (tfidf, tfidfTransf) = Tfidf.tfidfRowWise(corpus)
      val tfidfRes = tfidf.toMap

      println(tfidfTransf.uniqTokensIndex.toSeq)

      tfidfTransf.uniqTokensIndex.keys.toSet must_=== Set("one", "two", "three", "four", "five", "six", "seven", "nine", "ten", "eleven",
        "twelve")

      val tfidf0 = tfidfRes("0")
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 6.04330250
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf0(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 3.38629436
      val tfidf1 = tfidfRes("1")
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 2.60943791
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf1(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 3.38629436
      val tfidf2 = tfidfRes("2")
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 2.60943791
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 2.20397280
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf2(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 1.69314718
      val tfidf3 = tfidfRes("3")
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 7.66516293
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf3(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf4 = tfidfRes("4")
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 7.66516293
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf4(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf5 = tfidfRes("5")
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 1.91629073
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 3.02165125
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 1.51082562
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf5(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 1.69314718
      val tfidf6 = tfidfRes("6")
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf6(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf7 = tfidfRes("7")
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 2.60943791
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 0.0
      roundAtDecimal(tfidf7(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
      val tfidf8 = tfidfRes("8")
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("eleven")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("five")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("four")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("nine")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("one")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("seven")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("six")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("ten")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("three")), 8) must_=== 0.0
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("twelve")), 8) must_=== 7.82831374
      roundAtDecimal(tfidf8(tfidfTransf.uniqTokensIndex("two")), 8) must_=== 0.0
    }

    "stupid stupid serialization test" in {
      val x = Array[SparseVector[Double]](
        SparseVector[Double](1, 23, 3, 3, 0, 123123, 32, 2, 31, 32, 2, 2, 23, 1, 23, 23, 23, 32, 22, 23, 1, 33, 1, 1, 1, 66, 23, 2, 32, 32, 32, 355, 3, 5, 3, 3, 3, 3, 3, 3, 12, 3),
        SparseVector[Double](1.2, 23.1212, 22, 0.0, 0, 0, 0, 1.01, 0.0, 1, 1, 2, 3, 3, 3, 3, 3, 3, 3.012)
      )
      //        SparseVector[Double](1.2, 23.1212, 22, 0.0, 0, 0, 0, 1.01, 0.0, 1, 1, 2, 3, 3, 3, 3, 3, 3, 3.012)

      saveObject(x, "zzz")
      val res = loadObject("zzz")
      res must_=== x
    }

    "stupid serialization test" in {
      val x: ArrayBuffer[(String, SparseVector[Double])] = ArrayBuffer[(String, SparseVector[Double])](
        ("1m2m3", SparseVector[Double](322.0, 32, 33, 11.1, 3, 3, 3, 32.123, 33.01, 32323.11, 333.333)),
        ("1m222m3", SparseVector[Double](322, 32, 33, 11.1, 3, 3, 3, 32.123, 33.01)),
        ("1m222m3", SparseVector[Double](322, 32, 33, 11.1, 3, 3, 3, 32.123, 33.01)),
        ("1m2m3", SparseVector[Double](1, 1, 23, 3, 3, 0, 0.1023123, 32, 2, 31, 32, 2, 2, 23, 1, 23, 23, 23, 32, 23, 1, 33, 1, 1, 1, 66, 23, 2, 32, 32, 32, 355, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 1, 11, 121, 1, 23, 1, 2, 23, 12, 3)),
        ("1m2m3", SparseVector[Double](1, 23, 3, 3, 0, 123123, 32, 2, 31, 32, 2, 2, 23, 1, 23, 23, 23, 32, 22, 23, 1, 33, 1, 1, 1, 66, 23, 2, 32, 32, 32, 355, 3, 5, 3, 3, 3, 3, 3, 3, 12, 3))
      )

      saveObject(x, "aaa")
      val res = loadObject("aaa")
      res must_== x
    }

    def saveObject(payload: AnyRef, f: String) = {
      val file = Path.fromString(s"/var/auto-cs/archive/${f}.xxx")
      if (file.exists) file.delete(true)
      val objstream = new ObjectOutputStream(new FileOutputStream(file.jfile))
      objstream.writeObject(payload)
      objstream.flush()
      objstream.close()
    }

    def loadObject(f: String): AnyRef = {
      val objstream = new ObjectInputStream(new FileInputStream(s"/var/auto-cs/archive/${f}.xxx"))
      val res = objstream.readObject()
      objstream.close()
      res
    }

  }
}
