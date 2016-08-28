package com.abhi

/**
  * Created by abhsrivastava on 8/27/16.
  */
object Regex101 extends App {
   def findDigits() = {
      val input = "My phone number is 415-555-1212"
      val regex = """\d\d\d-\d\d\d-\d\d\d\d""".r
      regex.findAllIn(input).foreach{x => println(s"Phone number found: $x")}
   }

   def groupingWithParanthesis() = {
      val input = "My phone number is 415-555-1212"
      val regex = """(\d\d\d)-(\d\d\d)-(\d\d\d\d)""".r
      for {
         m <- regex.findAllIn(input).matchData
         s <- m.subgroups
      } println(s)
   }

   def escapingParanthesis() = {
      val input = "My phone number is (415) 555-1212"
      val regex = """(\(\d{3}\))\s(\d{3})-(\d{4})""".r
      for {
         m <- regex.findAllIn(input).matchData
         s <- m.subgroups
      } println(s)
   }

   def multipleGroups = {
      val regex = "Batman|Tina Fey".r
      val input = "Batman and Tina Fey"
      regex.findAllIn(input).foreach(println)
   }

   def pipeAndGrouping = {
      val regex = "Bat(man|mobile|copter)"r
      val input = "Batmobile lost a wheel"
      regex.findAllIn(input).foreach(println)
      for {
         m <- regex.findAllIn(input).matchData
         s <- m.subgroups
      } println(s)
   }

   def optionalMatches = {
      val regex = """Bat(wo)?man""".r
      val input1 = "The adventures of Batman"
      val input2 = "The adventures of Batwoman"
      val regex2 = """(\d{3}-)?\d{3}-\d{4}""".r
      val input3 = "My phone number is 415-555-1212"
      val input4 = "My phone number is 555-1212"
      regex.findAllIn(input1).foreach(println)
      regex.findAllIn(input2).foreach(println)
      regex2.findAllIn(input3).foreach(println)
      regex2.findAllIn(input4).foreach(println)
   }

   def specificRepetitions = {
      val regex = """(Ha){3}""".r
      val input = "HaHaHaHa"
      regex.findAllIn(input).foreach(println)
   }

   def greedy = {
      // look from 3 to 5 reptetions. greedy means keep looking till max find
      val regex = """(Ha){3,5}""".r
      val input = "HaHaHaHa"
      regex.findAllIn(input).foreach(println)
   }

   def nonGreedy = {
      // non greedy is ? meaning that stop looking when minimum criteria is met
      val regex = """(Ha){3,5}?""".r
      val input = "HaHaHaHaHa"
      regex.findAllIn(input).foreach(println)
   }

   def characterClasses = {
//      =========================  ===========================================
//         Shorthand character class  Represents
//      =========================  ===========================================
//         \d                         Any numeric digit from 0 to 9.
//      \0                         Any character that is not a numeric digit
//      from 0 to 9
//      \w                         Any letter, numeric digit, or the underscore
//      character.  (Think of this as matching
//      "word" characters.)
//      \W                         Any character that is not a letter,
//      numeric digit, or the underscore character.
//      \s                         Any space, tab, or newline character.  (
//         Think of this as matching white-space
//            characters.)
//      \S                         Any character that is not a space, tab,
//      or newline.
//      =========================  ===========================================

      val input = "12 drummers, 11 pipers, 10 lords, 9 ladies, 8 maids, 7 swans, 6 geese, 5 rings, 4 hummingbirds, 3 hens, 2 turtledoves, 1 partridge"
      val regex = """(\d+)\s(\w+)""".r
      regex.findAllIn(input).foreach(x => println(x))
      for {
         m <- regex.findAllIn(input).matchData
         e <- m.subgroups
      } println(e)
   }

   def greedyVsNonGreedy() = {
      val input = "<<to serve man> for dinner.>"
      val regex = """<.*>""".r // greedy
      regex.findAllIn(input).foreach(println)
      val regex2 = """<.*?>""".r // non greedy. stop at first match
      regex2.findAllIn(input).foreach(println)
   }

   def complex() = {
      val foo =
         """(                       // start group to capture the phone number
           |(                       // start of optional area code choices
           |\d{3}                   // bare three digits
           ||                       // or
           |\(\d{3}\)               // three digits enclosed in parentheses
           |)?                      // end of optional area code choices
           |(                       // start of optional separator
           |\s|-|\.                 // start of optional separator
           |)?                      // separator can be whitespace, dash or period
           |\d{3}                   // exchange number (required)
           |(\s|-|\.)               // same separator but required this time
           |\d{4}                   // final digits (required)
           |(                       // start of optional extension
           |\s*                     // zero or more characters of white space
           |(                       // start of extention indicator
           |ext.|x.|ext.|extn.      // extention can be indicated by "ext", "x", or extn followed by any character
           |)                       // end of extension indicator
           |\s*                     // zero or more characters of white space
           |\d{2,6}                 // two to five digits of extension number
           |)?                      //  end of optional estension
           |)""".stripMargin.replace("\n", "").trim
      println(foo)
      val regex = foo.r
      val input = "(888)-456-7890 extn: 12345"
      regex.findAllIn(input).foreach(println)
   }

   findDigits
   groupingWithParanthesis
   escapingParanthesis
   multipleGroups
   pipeAndGrouping
   optionalMatches
   specificRepetitions
   greedy
   nonGreedy
   characterClasses
   greedyVsNonGreedy
   complex
}
