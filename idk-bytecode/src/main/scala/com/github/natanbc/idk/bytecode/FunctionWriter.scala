package com.github.natanbc.idk.bytecode

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.github.natanbc.idk.bytecode.Opcodes.Opcode

class FunctionWriter private(bw: BytecodeWriter, id: Int, name: String, args: List[String], out: ByteArrayOutputStream, varargs: Boolean, annotations: List[String], private val parent: FunctionWriter) extends DataOutputStream(out) {
    private[bytecode] def this(bw: BytecodeWriter, id: Int, name: String, args: List[String], varargs: Boolean, annotations: List[String], parent: FunctionWriter) = this(bw, id, name, args, new ByteArrayOutputStream(), varargs, annotations, parent)

    private val declaredLocals = new collection.mutable.ListBuffer[String]()
    declaredLocals.appendAll(args)
    private val labels = new collection.mutable.ListBuffer[Label]()

    def declareLocal(name: String): Unit = {
        declaredLocals += name
    }

    def loadVariable(name: String): Unit = {
        val idx = declaredLocals.indexOf(name)
        if(idx != -1) {
            writeByte(Opcodes.LOAD_LOCAL)
            writeShort(idx)
            return
        } else if(parent != null) {
            var p = parent
            var depth = 1
            while(p != null) {
                val idx = p.declaredLocals.indexOf(name)
                if(idx != -1) {
                    writeByte(Opcodes.LOAD_UPVALUE)
                    writeShort(depth)
                    writeShort(idx)
                    return
                }
                p = p.parent
                depth += 1
            }
        }
        writeByte(Opcodes.LOAD_GLOBAL)
        writeUTF(name)
    }

    def storeVariable(name: String): Unit = {
        val idx = declaredLocals.indexOf(name)
        if(idx != -1) {
            writeByte(Opcodes.STORE_LOCAL)
            writeShort(idx)
        } else {
            if(parent != null) {
                var p = parent
                var depth = 1
                while(p != null) {
                    val idx = p.declaredLocals.indexOf(name)
                    if(idx != -1) {
                        writeByte(Opcodes.STORE_UPVALUE)
                        writeShort(depth)
                        writeShort(idx)
                        return
                    }
                    p = p.parent
                    depth += 1
                }
            }
            writeByte(Opcodes.STORE_GLOBAL)
            writeUTF(name)
        }
    }

    def storeGlobal(name: String): Unit = {
        writeByte(Opcodes.STORE_GLOBAL)
        writeUTF(name)
    }

    def storeMember(): Unit = {
        writeByte(Opcodes.STORE_FIELD)
    }

    def loadMember(): Unit = {
        writeByte(Opcodes.LOAD_FIELD)
    }

    def pop(): Unit = {
        writeByte(Opcodes.POP)
    }

    def loadConstant(l: Long): Unit = {
        writeByte(Opcodes.LOAD_LONG_CONSTANT)
        writeLong(l)
    }

    def loadConstant(d: Double): Unit = {
        writeByte(Opcodes.LOAD_DOUBLE_CONSTANT)
        writeDouble(d)
    }

    def loadConstant(s: String): Unit = {
        writeByte(Opcodes.LOAD_STRING_CONSTANT)
        writeUTF(s)
    }

    def loadConstant(b: Boolean): Unit = {
        if(b) {
            writeByte(Opcodes.LOAD_TRUE)
        } else {
            writeByte(Opcodes.LOAD_FALSE)
        }
    }

    def loadNil(): Unit = {
        writeByte(Opcodes.LOAD_NIL)
    }

    def call(narg: Int): Unit = {
        writeByte(Opcodes.CALL)
        writeShort(narg)
    }

    def unpackCall(narg: Int): Unit = {
        writeByte(Opcodes.UNPACK_CALL)
        writeShort(narg)
    }

    def decoratorCall(name: String): Unit = {
        writeByte(Opcodes.DECORATOR_CALL)
        writeUTF(name)
    }

    def objectLiteral(size: Int): Unit = {
        writeByte(Opcodes.OBJECT_LITERAL)
        writeShort(size)
    }

    def arrayLiteral(size: Int): Unit = {
        writeByte(Opcodes.ARRAY_LITERAL)
        writeShort(size)
    }

    def writeInstruction(opcode: Opcode, arguments: Int*): Unit = {
        if(opcode.narg != arguments.length) {
            throw new IllegalArgumentException("opcode.narg != arguments.length")
        }
        writeByte(opcode)
        arguments.foreach(writeShort(_))
    }

    def writeLabel(l: Label): Unit = {
        if(l.written) throw new IllegalArgumentException("This label has already been written!")
        val idx = labels.indexOf(l)
        if(idx == -1) throw new IllegalArgumentException("This label belongs to a different FunctionWriter!")
        l.written = true
        writeInstruction(Opcodes.LABEL, idx)
    }

    def jumpTo(l: Label): Unit = {
        val idx = labels.indexOf(l)
        if(idx == -1) throw new IllegalArgumentException("This label belongs to a different FunctionWriter!")
        writeInstruction(Opcodes.GOTO_LABEL, idx)
    }

    def ifFalse(l: Label): Unit = {
        val idx = labels.indexOf(l)
        if(idx == -1) throw new IllegalArgumentException("This label belongs to a different FunctionWriter!")
        writeInstruction(Opcodes.NEGATE)
        writeInstruction(Opcodes.IF_LABEL, idx)
    }

    def ifTrue(l: Label): Unit = {
        val idx = labels.indexOf(l)
        if(idx == -1) throw new IllegalArgumentException("This label belongs to a different FunctionWriter!")
        writeInstruction(Opcodes.IF_LABEL, idx)
    }

    def dup(): Unit = {
        writeInstruction(Opcodes.DUP)
    }

    def ret(): Unit = {
        writeInstruction(Opcodes.RETURN)
    }

    def newLabel(): Label = {
        val l = new Label()
        labels += l
        l
    }

    def write(): Unit = {
        val bytes = processLabels(moveConstantsToPool(out.toByteArray))

        bw.writeByte(Opcodes.Meta.FUNCTION)
        bw.writeShort(id)
        bw.writeBoolean(name != null)
        if(name != null) {
            bw.writeShort(bw.constant(name))
        }
        bw.writeBoolean(varargs)
        bw.writeShort(annotations.length)
        annotations.foreach(a=>{
            bw.writeShort(bw.constant(a))
        })
        bw.writeShort(args.length)
        args.foreach(a=>{
            bw.writeShort(bw.constant(a))
        })
        bw.writeShort(declaredLocals.length)
        bw.writeShort(bytes.length)
        bw.write(bytes)
    }

    private def moveConstantsToPool(bytecode: Array[Byte]): Array[Byte] = {
        val buffer = new ByteArrayOutputStream()

        val newOut = new DataOutputStream(buffer)
        val in = new DataInputStream(new ByteArrayInputStream(bytecode))

        while(in.available() > 0) {
            Opcodes.fromID(in.readUnsignedByte()) match {
                case Opcodes.LOAD_LONG_CONSTANT =>
                    newOut.writeByte(Opcodes.LOAD_LONG_CONSTANT)
                    newOut.writeShort(bw.constant(in.readLong()))
                case Opcodes.LOAD_DOUBLE_CONSTANT =>
                    newOut.writeByte(Opcodes.LOAD_DOUBLE_CONSTANT)
                    newOut.writeShort(bw.constant(in.readDouble()))
                case Opcodes.LOAD_STRING_CONSTANT =>
                    newOut.writeByte(Opcodes.LOAD_STRING_CONSTANT)
                    newOut.writeShort(bw.constant(in.readUTF()))
                case Opcodes.LOAD_GLOBAL =>
                    newOut.writeByte(Opcodes.LOAD_GLOBAL)
                    newOut.writeShort(bw.constant(in.readUTF()))
                case Opcodes.STORE_GLOBAL =>
                    newOut.writeByte(Opcodes.STORE_GLOBAL)
                    newOut.writeShort(bw.constant(in.readUTF()))
                case Opcodes.DECORATOR_CALL =>
                    newOut.writeByte(Opcodes.DECORATOR_CALL)
                    newOut.writeShort(bw.constant(in.readUTF()))
                case op =>
                    newOut.writeByte(op.id)
                    for(_ <- 1 to op.narg) {
                        newOut.writeShort(in.readShort())
                    }
            }
        }

        buffer.toByteArray
    }

    private def processLabels(bytecode: Array[Byte]): Array[Byte] = {
        var in = new DataInputStream(new ByteArrayInputStream(bytecode))

        var index = 0
        while(in.available() > 0) {
            Opcodes.fromID(in.readUnsignedByte()) match {
                case Opcodes.LABEL =>
                    labels(in.readUnsignedShort()).index = index
                case o =>
                    for(_ <- 1 to o.narg) {
                        in.readShort()
                    }
                    index += 1
            }
        }

        val buffer = new ByteArrayOutputStream()
        val newOut = new DataOutputStream(buffer)
        in = new DataInputStream(new ByteArrayInputStream(bytecode))

        while(in.available() > 0) {
            Opcodes.fromID(in.readUnsignedByte()) match {
                case Opcodes.GOTO_LABEL =>
                    newOut.writeByte(Opcodes.GOTO)
                    newOut.writeShort(labels(in.readUnsignedShort()).index)
                case Opcodes.IF_LABEL =>
                    newOut.writeByte(Opcodes.IF)
                    newOut.writeShort(labels(in.readUnsignedShort()).index)
                case Opcodes.LABEL => in.readUnsignedShort()
                case o =>
                    newOut.writeByte(o.id)
                    for(_ <- 1 to o.narg) {
                        newOut.writeShort(in.readUnsignedShort())
                    }
            }
        }

        buffer.toByteArray
    }
}
