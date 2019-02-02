package com.github.natanbc.idk.bytecode

import java.io.{DataInputStream, InputStream}

import scala.annotation.switch

class BytecodeReader(in: InputStream) {
    private val dis = new DataInputStream(in)
    private val constants = new collection.mutable.ArrayBuffer[Constant]()
    private val constantCount = dis.readUnsignedShort()
    constants.sizeHint(constantCount)
    for(_ <- 1 to constantCount) {
        (dis.readUnsignedByte(): @switch) match {
            case Opcodes.Meta.CONSTANT_STRING =>
                constants += StringConstant(dis.readUTF())
            case Opcodes.Meta.CONSTANT_DOUBLE =>
                constants += FloatConstant(dis.readDouble())
            case Opcodes.Meta.CONSTANT_LONG =>
                constants += IntConstant(dis.readLong())
            case o =>
                throw new MalformedBytecodeException(f"Unexpected constant pool entry type $o%02X")
        }
    }
    //ignore the length
    dis.readInt()

    def getStringConstant(idx: Int): String = {
        val c = constants(idx)
        if(c.isString) {
            c.asString
        } else {
            throw new MalformedBytecodeException(s"Constant pool entry at index $idx is not a string")
        }
    }

    def getLongConstant(idx: Int): Long = {
        val c = constants(idx)
        if(c.isInt) {
            c.asLong
        } else {
            throw new MalformedBytecodeException(s"Constant pool entry at index $idx is not a long")
        }
    }

    def getDoubleConstant(idx: Int): Double = {
        val c = constants(idx)
        if(c.isFloat) {
            c.asDouble
        } else {
            throw new MalformedBytecodeException(s"Constant pool entry at index $idx is not a double")
        }
    }

    def readFunction(): CompiledFunction = {
        dis.read() match {
            case -1 => null
            case Opcodes.Meta.FUNCTION => new CompiledFunction(this, dis)
            case o => throw new MalformedBytecodeException(f"Unexpected type $o%02X")
        }
    }
}
