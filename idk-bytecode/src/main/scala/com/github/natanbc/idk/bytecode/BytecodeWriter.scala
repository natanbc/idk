package com.github.natanbc.idk.bytecode

import java.io.{ByteArrayOutputStream, DataOutputStream, OutputStream}

class BytecodeWriter private(out: OutputStream, baos: ByteArrayOutputStream) extends DataOutputStream(baos) {
    def this(out: OutputStream) = this(out, new ByteArrayOutputStream())

    private val constantPool = new collection.mutable.ListBuffer[Any]()

    def newFunction(id: Int, name: String, args: List[String], varargs: Boolean, annotations: List[String], parent: FunctionWriter = null): FunctionWriter = {
        new FunctionWriter(this, id, name, args, varargs, annotations, parent)
    }

    @inline private def checkPool(a: Any): Int = {
        val idx = constantPool.indexOf(a)
        if(idx != -1) return idx
        constantPool.append(a)
        constantPool.length - 1
    }

    def constant(l: Long): Int = checkPool(l)

    def constant(d: Double): Int = checkPool(d)

    def constant(s: String): Int = checkPool(s)

    def write(): Unit = {
        val dos = new DataOutputStream(out)
        dos.writeShort(constantPool.length)
        constantPool.foreach({
            case s: String =>
                dos.writeByte(Opcodes.Meta.CONSTANT_STRING)
                dos.writeUTF(s)
            case l: Long =>
                dos.writeByte(Opcodes.Meta.CONSTANT_LONG)
                dos.writeLong(l)
            case d: Double =>
                dos.writeByte(Opcodes.Meta.CONSTANT_DOUBLE)
                dos.writeDouble(d)
        })
        val bytes = baos.toByteArray
        dos.writeInt(bytes.length)
        dos.write(bytes)
    }
}
