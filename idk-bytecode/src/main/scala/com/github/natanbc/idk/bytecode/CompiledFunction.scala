package com.github.natanbc.idk.bytecode

import java.io.{ByteArrayInputStream, DataInputStream}

import scala.collection.mutable.ListBuffer

class CompiledFunction(br: BytecodeReader, in: DataInputStream) {
    val id: Int = in.readUnsignedShort()
    val name: String = {
        if(in.readBoolean()) {
            br.getStringConstant(in.readUnsignedShort())
        } else {
            null
        }
    }
    val varargs: Boolean = in.readBoolean()
    val annotations: List[String] = {
        val buffer = new ListBuffer[String]()
        for(_ <- 1 to in.readUnsignedShort()) {
            buffer += br.getStringConstant(in.readUnsignedShort())
        }
        buffer.toList
    }
    val argCount: Int = in.readUnsignedShort()
    val argNames: List[String] = {
        val buffer = new ListBuffer[String]()
        for(_ <- 1 to argCount) {
            buffer += br.getStringConstant(in.readUnsignedShort())
        }
        buffer.toList
    }
    val localCount: Int = in.readUnsignedShort()

    val code: List[CompiledFunction.OpcodeAndArgs] = {
        val buffer = new ListBuffer[CompiledFunction.OpcodeAndArgs]()
        val bytes = new Array[Byte](in.readUnsignedShort())
        in.read(bytes)
        val newIn = new DataInputStream(new ByteArrayInputStream(bytes))
        while(newIn.available() > 0) {
            val opcode = Opcodes.fromID(newIn.readUnsignedByte())
            buffer += CompiledFunction.OpcodeAndArgs(opcode, (1 to opcode.narg).map(_=>newIn.readUnsignedShort()):_*)
        }
        buffer.toList
    }
}

object CompiledFunction {
    case class OpcodeAndArgs(opcode: Opcodes.Opcode, args: Int*)
}
