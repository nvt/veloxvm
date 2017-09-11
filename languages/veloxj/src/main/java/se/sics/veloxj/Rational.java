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

public class Rational {
    private int numerator;
    private int denominator;

    private int gcd(int a, int b) {
	int shiftResult, oldA;
	boolean aIsEven, bIsEven;

	a = Math.abs(a);
	b = Math.abs(b);

	if(a == 0 || b == 0) {
	     /* GCD is zero if a equals b, otherwise it is 1. */
	    return a == b ? 0 : 1;
	}

	/* The Binary GCD algorithm. */

	shiftResult = 0;
	while(a != b) {
	    if(a == 1 || b == 1) {
		return 1 << shiftResult;
	    }

	    aIsEven = (~a & 1) == 1;
	    bIsEven = (~b & 1) == 1;

	    if (aIsEven) {
		a /= 2;
		if (bIsEven) {
		    b /= 2;
		    shiftResult++;
		}
	    } else {
		if (bIsEven) {
		    b /= 2;
		} else {
		    if(a > b) {
			a = (a - b) / 2;
		    } else {
			oldA = a;
			a = (b - a) / 2;
			b = oldA;
		    }
		}
	    }
	}

	return a << shiftResult;
    }

    private void normalize() {
	if (denominator < 0) {
	    denominator = -denominator;
	    numerator = -numerator;
	}

	if(denominator != 1) {
	    int divisor = gcd(numerator, denominator);
	    numerator /= divisor;
	    denominator /= divisor;
	}
    }

    public Rational(int value) {
	this(value, 1);
    }

    public Rational(int numerator, int denominator) {
	this.numerator = numerator;
	this.denominator = denominator;
	normalize();
    }

    public Integer numerator() {
	return Integer.valueOf(numerator);
    }

    public Integer denominator() {
	return Integer.valueOf(denominator);
    }
}
