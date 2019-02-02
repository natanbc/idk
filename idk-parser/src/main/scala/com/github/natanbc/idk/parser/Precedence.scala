package com.github.natanbc.idk.parser

object Precedence {
    final val ASSIGNMENT = 1
    final val DISJUNCTION = 2 // a || b
    final val CONJUNCTION = 3// a && b
    final val CONDITIONAL = 4
    final val SUM = 5
    final val PRODUCT = 6
    final val EXPONENT = 7
    final val PREFIX = 8
    final val POSTFIX = 9
    final val CALL = 10
    final val MEMBER_ACCESS = 11
}
