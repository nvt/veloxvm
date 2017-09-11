/*
 * Copyright (c) 2012-2017, RISE SICS AB
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

package se.sics.veloxj;

import java.io.*;
import java.util.logging.*;
import java.util.Vector;

public class VeloxByteCode {
    private static final int VELOX_BYTECODE_VERSION =  1;
    private static final int VELOX_FILE_MAGIC1      =  94;
    private static final int VELOX_FILE_MAGIC2      = 181;

    private static final int VELOX_TYPE_BOOLEAN   = 0;
    private static final int VELOX_TYPE_INTEGER   = 1;
    private static final int VELOX_TYPE_RATIONAL  = 2;
    private static final int VELOX_TYPE_REAL      = 3;
    private static final int VELOX_TYPE_STRING    = 4;
    private static final int VELOX_TYPE_SYMBOL    = 5;
    private static final int VELOX_TYPE_CHARACTER = 6;

    private static final int VELOX_TOKEN_ATOM     = 0;
    private static final int VELOX_TOKEN_FORM     = 1;

    private static final int VELOX_FORM_INLINE    = 0;
    private static final int VELOX_FORM_LAMBDA    = 1;
    private static final int VELOX_FORM_REF       = 2;

    private static final int VELOX_TOKEN_LENGTH         =  1;
    private static final int VELOX_FORM_TYPE_LENGTH     =  2;
    private static final int VELOX_TYPE_LENGTH          =  3;
    private static final int VELOX_EMBEDDED_LENGTH      =  4;
    private static final int VELOX_EXPR_LENGTH          =  5;
    private static final int VELOX_SHORT_EXPR_ID_LENGTH =  4;
    private static final int VELOX_EXPR_ID_LENGTH       = 12;

    private VeloxApp app;
    private VeloxBitStream bitStream;

    public VeloxByteCode(VeloxApp app, VeloxBitStream bitStream) {
        this.app = app;
	this.bitStream = bitStream;
    }

    public VeloxByteCode(VeloxApp app) {
        this(app, new VeloxBitStream());
    }

    public byte[] generate() throws VeloxException {
        storeHeader();

        storeTable(app.getStringTable());
        storeTable(app.getSymbolTable());
	storeTable(app.getExpressionTable());

	return bitStream.toByteArray();
    }

    private void storeHeader() {
	bitStream.writeBits(VELOX_FILE_MAGIC1, 8);
	bitStream.writeBits(VELOX_FILE_MAGIC2, 8);
	bitStream.writeBits(VELOX_BYTECODE_VERSION, 8);
    }

    private void storeAtom(int type) {
	storeAtom(type, 0);
    }

    private void storeAtom(int type, int infoField) {
	bitStream.writeBits(VELOX_TOKEN_ATOM, VELOX_TOKEN_LENGTH);
	bitStream.writeBits(infoField, VELOX_EMBEDDED_LENGTH);
	bitStream.writeBits(type, VELOX_TYPE_LENGTH);
    }

    private void storeForm(int formType, int formInfo) throws VeloxException {
	bitStream.writeBits(VELOX_TOKEN_FORM, VELOX_TOKEN_LENGTH);
	bitStream.writeBits(formType, VELOX_FORM_TYPE_LENGTH);
	switch (formType) {
	case VELOX_FORM_INLINE:
	    bitStream.writeBits(formInfo, VELOX_EXPR_LENGTH);
	    break;
	case VELOX_FORM_LAMBDA:
	case VELOX_FORM_REF:
	    if (formInfo < 16) {
		bitStream.writeBits(1);
		bitStream.writeBits(formInfo, VELOX_SHORT_EXPR_ID_LENGTH);
	    } else {
		bitStream.writeBits(0);
		bitStream.writeBits(formInfo, VELOX_EXPR_ID_LENGTH);
	    }
	    break;
	default:
	    throw new VeloxException("Unhandled form type");
	}
    }

    private void storeItem(byte[] bytes, boolean withLength) throws VeloxException {
	if (bytes.length > 255) {
	    throw new VeloxException("Cannot write a table item that is larger than 255 bytes");
	}

	try {
	    VeloxCompiler.logger.finer("Writing an item of " + bytes.length + " bytes");
	    if (withLength) {
		bitStream.writeBits(bytes.length, 8);
	    }
	    bitStream.writeBytes(bytes);
	} catch (IOException e) {
	    throw new VeloxException("Unable to store a table: " +
				     e.getMessage());
	}
    }

    private void storeTable(VeloxTable table) throws VeloxException {
	Vector items = table.vectorize();

	/* The first byte of the table specifies how many items it contains. */
	bitStream.writeBits(items.size(), 8);

	int i = 0;
	for (Object item : items) {
	    byte[] bytes;

	    if (item instanceof VeloxString ||
		item instanceof VeloxSymbol) {
		VeloxObject object = (VeloxObject)item;
		VeloxCompiler.logger.fine("Store string or symbol \"" +
			    object.getObject() + "\"");
		try {
		    bytes = ((String)(object.getObject())).getBytes("ISO-8859-1");
		} catch (UnsupportedEncodingException e) {
		    throw new VeloxException("Cannot store item in ISO-8859-1 format");
		}
		storeItem(bytes, true);
	    } else if (item instanceof VeloxExpression) {
		VeloxBitStream savedBitStream = bitStream;
		ByteArrayOutputStream tempOutputStream = new ByteArrayOutputStream();

		bitStream = new VeloxBitStream(tempOutputStream);

		if (i == 0) {
		    for (VeloxObject arg : ((VeloxExpression)item).getArguments()) {
			storeDispatch(arg);
		    }
		} else {
		    store((VeloxExpression)item);
		}

		bitStream = savedBitStream;

		bytes = tempOutputStream.toByteArray();
		storeItem(bytes, true);
	    } else {
		throw new VeloxException("Storing a table with an unhandled item type");
	    }

	    i++;
	}
    }

    private void storeDispatch(VeloxObject obj) throws VeloxException {
	if (obj instanceof VeloxBoolean) {
	    store((VeloxBoolean)obj);
	} else if (obj instanceof VeloxInteger) {
	    store((VeloxInteger)obj);
	} else if (obj instanceof VeloxRational) {
	    store((VeloxRational)obj);
	} else if (obj instanceof VeloxCharacter) {
	    store((VeloxCharacter)obj);
	} else if (obj instanceof VeloxExpressionRef) {
	    store((VeloxExpressionRef)obj);
	} else if (obj instanceof VeloxStringRef) {
	    store((VeloxStringRef)obj);
	} else if (obj instanceof VeloxSymbolRef) {
	    store((VeloxSymbolRef)obj);
	} else if (obj instanceof VeloxLambdaExpression) {
	    store((VeloxLambdaExpression)obj);
	} else if (obj instanceof VeloxExpression) {
	    store((VeloxExpression)obj);
	} else {
	    throw new VeloxException("Unable to store object of type " +
				     obj.getClass().getName());
	}
    }

    public void store(VeloxExpression obj) throws VeloxException {
        Vector<VeloxObject> argv = obj.getArguments();
	int argc = argv.size();

	VeloxCompiler.logger.finer("Store expression with " +
				   argc + " arguments");

	storeForm(VELOX_FORM_INLINE, argc);
	for (VeloxObject arg : argv) {
	    VeloxCompiler.logger.finer("Dispatch object of type " +
				       arg.getClass().getName());
	    storeDispatch(arg);
	}
    }

    public void store(VeloxLambdaExpression obj) throws VeloxException {
        Vector <VeloxObject> arguments = obj.getArguments();
        int argc = arguments.size();

        storeForm(VELOX_FORM_LAMBDA, argc);
	VeloxExpression expr = new VeloxExpression();
	expr.addArgument(new VeloxSymbol("bind"));
	for (VeloxObject arg : arguments) {
	    expr.addArgument(arg);
	}
	/* TODO: Add a reference to the body's expr ID as the final argument. */
	storeDispatch(expr);
    }

    public void store(VeloxBoolean obj) {
	storeAtom(VELOX_TYPE_BOOLEAN,
		  ((Boolean)obj.getObject()).booleanValue() ? 1 : 0);
    }

    public void store(VeloxInteger obj) {
	int formInfo;
	Integer i = (Integer)obj.getObject();
	int bytesRequired;

	if (i < 0) {
	    i = Math.abs(i);
	    formInfo = 1 << 3;
	} else {
	    formInfo = 0;
	}

	if ((i >> 24) != 0) {
	    bytesRequired = 4;
	} else if ((i >> 16) != 0) {
            bytesRequired = 3;
	} else if ((i >> 8) != 0) {
	    bytesRequired = 2;
	} else {
	    bytesRequired = 1;
	}
	VeloxCompiler.logger.finer("Write integer " + i + " of " +
				   bytesRequired + " bytes");
	formInfo |= bytesRequired;

	storeAtom(VELOX_TYPE_INTEGER, formInfo);
	for (int j = 0; j < bytesRequired; j++) {
	    bitStream.writeBits((i >> (j * 8)) & 0xff, 8);
	}
    }

    public void store(VeloxRational obj) {
	storeAtom(VELOX_TYPE_RATIONAL);
	store(obj.numerator());
	store(obj.denominator());
    }

    public void store(VeloxCharacter obj) {
	storeAtom(VELOX_TYPE_CHARACTER);
	bitStream.writeBits(((Character)obj.getObject()).charValue(), 8);
    }

    public void store(VeloxExpressionRef obj) throws VeloxException {
	VeloxCompiler.logger.finer("Store expression ref " + obj.getExprID());
	storeForm(VELOX_FORM_REF, obj.getExprID());
    }

    public void store(VeloxStringRef obj) throws VeloxException {
	storeAtom(VELOX_TYPE_STRING);
	bitStream.writeBits(obj.getStringID(), 8);
    }

    public void store(VeloxSymbolRef obj) throws VeloxException {
	storeAtom(VELOX_TYPE_SYMBOL);

	bitStream.writeBits(obj.isLocal() ? 1 : 0);
	int symbolID = obj.getSymbolID();
	if(symbolID > 63) {
	    bitStream.writeBits(1);
	    bitStream.writeBits(symbolID, 14);
	} else {
	    bitStream.writeBits(0);
	    bitStream.writeBits(symbolID, 6);
	}
    }
}
