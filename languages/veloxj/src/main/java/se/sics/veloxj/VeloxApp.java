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

import java.util.Vector;
import java.util.logging.*;

public class VeloxApp {
    private String name;
    private VeloxTable<VeloxString> strings;
    private VeloxTable<VeloxSymbol> symbols;
    private VeloxTable<VeloxSymbol> internalSymbols;
    private VeloxTable<VeloxExpression> expressions;

    public VeloxApp(String name) throws VeloxException {
	this.name = name;
	strings = new VeloxTable<VeloxString>();
	symbols = new VeloxTable<VeloxSymbol>();
	internalSymbols = VeloxCompiler.getInternalSymbols();
	expressions = new VeloxTable<VeloxExpression>();

	/* Insert an empty top-level expression. */
	expressions.insert(new VeloxExpression());
    }

    public String getName() {
	return name;
    }

    public VeloxTable<VeloxString> getStringTable() {
	return strings;
    }

    public VeloxTable<VeloxSymbol> getSymbolTable() {
	return symbols;
    }

    public VeloxTable<VeloxExpression> getExpressionTable() {
	return expressions;
    }

    public void addExpression(VeloxExpression expression) throws VeloxException {
	VeloxExpression topExpression = expressions.get(0);
	topExpression.addArgument(expression);
	processExpression(expression);
    }

    private void processExpression(VeloxExpression expression) throws VeloxException {
	Vector<VeloxObject> arguments = expression.getArguments();
	for(int i = 0; i < arguments.size(); i++) {
	    VeloxObject obj = arguments.elementAt(i);

	    if (obj instanceof VeloxExpression) {
		/* Replace the actual expression with a reference to the
		   placement of the expression in the expression table. */
		int exprID = expressions.find((VeloxExpression)obj);
		if (exprID < 0) {
		    exprID = expressions.insert((VeloxExpression)obj);
		}
		arguments.setElementAt((VeloxObject)new VeloxExpressionRef(exprID), i);
		VeloxCompiler.logger.finer("Replace an expression at argument " +
					   i + " to a reference to ID " +
					   exprID);
		processExpression((VeloxExpression)obj);
	    } else if (obj instanceof VeloxString) {
		int stringID = strings.find((VeloxString)obj);
		if (stringID < 0) {
		    stringID = strings.insert((VeloxString)obj);
		}
		arguments.set(i, new VeloxStringRef(stringID));
	    } else if (obj instanceof VeloxSymbol) {
		int symbolID = internalSymbols.find((VeloxSymbol)obj);
		if (symbolID >= 0) {
		    /* The symbol is defined by Velox. */
		    VeloxCompiler.logger.finest("Found Velox symbol "+
						obj.getObject());
		    arguments.set(i, new VeloxSymbolRef(symbolID, false));
		} else {
		    /* The symbol is defined by the application. */
		    symbolID = symbols.find((VeloxSymbol)obj);
		    if (symbolID < 0) {
			symbolID = symbols.insert((VeloxSymbol)obj);
		    }
		    arguments.set(i, new VeloxSymbolRef(symbolID, true));
		}
	    }
	}
    }

    public String getDefaultRepresentation() {
        StringBuilder buf = new StringBuilder();

	for (VeloxExpression expression : expressions.vectorize()) {
	    buf.append(expression.getDefaultRepresentation());
	    buf.append('\n');
	}

	if (buf.charAt(buf.length() - 1) == '\n') {
	    return buf.substring(0, buf.length() - 1).toString();
        } else {
	    return buf.toString();
        }
    }
}
