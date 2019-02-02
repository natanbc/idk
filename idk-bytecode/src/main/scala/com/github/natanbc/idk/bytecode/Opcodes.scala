package com.github.natanbc.idk.bytecode

object Opcodes {
    final val LOAD_LOCAL = Opcode(1, 1) //push local
    final val LOAD_GLOBAL = Opcode(2, 1) //push global
    final val LOAD_STRING_CONSTANT = Opcode(3, 1) //push constant
    final val LOAD_LONG_CONSTANT = Opcode(4, 1) //push constant
    final val LOAD_DOUBLE_CONSTANT = Opcode(5, 1) //push constant
    final val LOAD_TRUE = Opcode(6, 0) //push constant
    final val LOAD_FALSE = Opcode(7, 0) //push constant
    final val LOAD_NIL = Opcode(8, 0) //push constant
    final val CALL = Opcode(9, 1) //pop <arg 1> elements, pop value, call it

    final val ADD = Opcode(10, 0) //pop 2, add, push
    final val SUB = Opcode(11, 0) //pop 2, subtract, push
    final val MUL = Opcode(12, 0) //pop 2, multiply, push
    final val DIV = Opcode(13, 0) //pop 2, divide, push
    final val MOD = Opcode(14, 0) //pop 2, modulo, push
    final val POW = Opcode(15, 0) //pop 2, elevate, push

    final val NEG = Opcode(16, 0) //pop 1, invert signal, push
    final val NEGATE = Opcode(17, 0) //pop 1, logical negate, push
    final val EQ = Opcode(18, 0) //pop 2, compare, push
    final val NEQ = Opcode(19, 0) //pop 2, compare, push

    final val GREATER = Opcode(20, 0) //pop 2, compare, push
    final val GREATER_EQ = Opcode(21, 0) //pop 2, compare, push
    final val SMALLER = Opcode(22, 0) //pop 2, compare, push
    final val SMALLER_EQ = Opcode(23, 0) //pop 2, compare, push

    final val AND = Opcode(24, 0) //pop 2, compare, push
    final val OR = Opcode(25, 0) //pop 2, compare, push

    final val LOAD_FUNCTION = Opcode(26, 1) //push function with id <arg 1>
    final val STORE_LOCAL = Opcode(27, 1) //pop, store local
    final val STORE_GLOBAL = Opcode(28, 1) //pop, store global

    final val GOTO = Opcode(29, 1) //change instruction pointer

    final val IF = Opcode(30, 1) //pop, change instruction pointer if true

    final val DUP = Opcode(31, 0) //pop, push twice

    final val LOAD_UPVALUE = Opcode(32, 2) //load local <arg 2> from function <arg 1> up the call stack, push
    final val STORE_UPVALUE = Opcode(33, 2) //pop, store local <arg 2> in function <arg 1> up the call stack
    final val LOAD_FIELD = Opcode(34, 0) //pop field, pop location, load, push value
    final val STORE_FIELD = Opcode(35, 0) //pop value, pop field, pop destination, store

    final val UNPACK_CALL = Opcode(36, 1) //equal to CALL, but last argument must be an array and is unpacked for varargs call

    final val RETURN = Opcode(37, 0) //pop, return

    final val OBJECT_LITERAL = Opcode(38, 1) //(pop value, pop key) <arg 1> times, push object
    final val ARRAY_LITERAL = Opcode(39, 1) //pop <arg 1> values, push array

    final val POP = Opcode(40, 0) //pop

    final val DECORATOR_CALL = Opcode(41, 1) //pop argument, pop decorator, call, push result (also asserts everything is a function). <arg 1> is the CP index of the decorator name (for error messages)

    //FunctionWriter internal
    final val IF_LABEL = Opcode(253, 1)
    final val GOTO_LABEL = Opcode(254, 1)
    final val LABEL = Opcode(255, 1)

    case class Opcode(id: Int, narg: Int)

    private val map: Map[String, Any] = Map(
        "LOAD_LOCAL"->LOAD_LOCAL,
        "LOAD_GLOBAL"->LOAD_GLOBAL,
        "LOAD_STRING_CONSTANT"->LOAD_STRING_CONSTANT,
        "LOAD_LONG_CONSTANT"->LOAD_LONG_CONSTANT,
        "LOAD_DOUBLE_CONSTANT"->LOAD_DOUBLE_CONSTANT,
        "LOAD_TRUE"->LOAD_TRUE,
        "LOAD_FALSE"->LOAD_FALSE,
        "LOAD_NIL"->LOAD_NIL,
        "CALL"->CALL,
        "ADD"->ADD,
        "SUB"->SUB,
        "MUL"->MUL,
        "DIV"->DIV,
        "MOD"->MOD,
        "POW"->POW,
        "NEG"->NEG,
        "NEGATE"->NEGATE,
        "EQ"->EQ,
        "NEQ"->NEQ,
        "GREATER"->GREATER,
        "GREATER_EQ"->GREATER_EQ,
        "SMALLER"->SMALLER,
        "SMALLER_EQ"->SMALLER_EQ,
        "AND"->AND,
        "OR"->OR,
        "LOAD_FUNCTION"->LOAD_FUNCTION,
        "STORE_LOCAL"->STORE_LOCAL,
        "STORE_GLOBAL"->STORE_GLOBAL,
        "GOTO"->GOTO,
        "IF"->IF,
        "DUP"->DUP,
        "LOAD_UPVALUE"->LOAD_UPVALUE,
        "STORE_UPVALUE"->STORE_UPVALUE,
        "LOAD_FIELD"->LOAD_FIELD,
        "STORE_FIELD"->STORE_FIELD,
        "UNPACK_CALL"->UNPACK_CALL,
        "RETURN"->RETURN,
        "OBJECT_LITERAL"->OBJECT_LITERAL,
        "ARRAY_LITERAL"->ARRAY_LITERAL,
        "POP"->POP,
        "DECORATOR_CALL"->DECORATOR_CALL,
        "IF_LABEL"->IF_LABEL,
        "GOTO_LABEL"->GOTO_LABEL,
        "LABEL"->LABEL
    )

    def fromID(id: Int): Opcode = {
        map.values.find({
            case Opcode(i, _) => i == id
            case _ => false
        }).getOrElse(throw new IllegalArgumentException(s"No opcode with id $id")).asInstanceOf[Opcode]
    }

    def nameOf(id: Int): String = {
        map.find(_._2 match {
            case Opcode(i, _) => id == i
            case _ => false
        }).getOrElse(throw new IllegalArgumentException(s"No opcode with id $id"))._1
    }

    import language.implicitConversions
    implicit def o2i(o: Opcode): Int = o.id

    object Meta {
        final val FUNCTION = 1
        final val CONSTANT_STRING = 2
        final val CONSTANT_LONG = 3
        final val CONSTANT_DOUBLE = 4
    }
}
