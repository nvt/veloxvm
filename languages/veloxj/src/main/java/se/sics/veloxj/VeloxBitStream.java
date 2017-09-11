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

import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class VeloxBitStream {
    private ByteArrayOutputStream outputStream;
    private int byteValue;
    private int bitsFree;

    private int highestSetBit(int value) {
	for (int i = 31; i >= 0; i--) {
	    if (((value >> i) & 1) == 1) {
		return i + 1;
	    }
	}
	return 0;
    }

    public static boolean test() {
	ByteArrayOutputStream x = new ByteArrayOutputStream();
	VeloxBitStream bitStream = new VeloxBitStream(x);

	if(false) {
	    bitStream.writeBits(2);
	    bitStream.writeBits(2);
	    bitStream.writeBits(0, 4);
	} else {
	    bitStream.writeBits(1);
	    bitStream.writeBits(0);
	    bitStream.writeBits(64);
	}

	byte[] buf = x.toByteArray();

	System.out.println("bitstream result size: " + buf.length);
	for(int i = 0; i < buf.length; i++) {
	    System.out.println("Byte " + i + ": " + buf[i]);
	}

	return true;
    }

    public VeloxBitStream(ByteArrayOutputStream outputStream) {
	this.outputStream = outputStream;
	byteValue = 0;
	bitsFree = 8;
    }

    public VeloxBitStream() {
        this(new ByteArrayOutputStream());
    }

    public byte[] toByteArray() {
        return outputStream.toByteArray();
    }

    public int bitOffset() {
	return outputStream.toByteArray().length + 8 - bitsFree;
    }

    /*
     * Write a value to the bit stream. The length is decided by the position
     * of the highest set bit. If the value is zero, we treat it as a 1-bit
     * value.
     */
    public void writeBits(int value) {
	writeBits(value, value == 0 ? 1 : highestSetBit(value));
    }

    /* Write a value to the bit stream. The length is explicitly set by
       the caller, which is useful if the value contains leading zeroes. */
    public void writeBits(int value, int length) {
	while(length >= bitsFree) {
	    byteValue = (byteValue << bitsFree);
	    byteValue |= ((value >> (length - bitsFree)) & ((1 << bitsFree) - 1));

	    outputStream.write((byte)byteValue);

	    length -= bitsFree;
	    value = (value << bitsFree) & ((1 << length) - 1);
	    bitsFree = 8;
	    byteValue = 0;
	}

	if(length > 0) {
	    byteValue = (byteValue << length) | (value & ((1 << length) - 1));
	    bitsFree -= length;
	}
    }

    public void writeBytes(byte[] bytes) throws IOException, VeloxException {
	if (bitsFree != 8) {
	    throw new VeloxException("Cannot write a byte array when the offset is not at the beginning of a byte");
	}
	outputStream.write(bytes);
    }
}
