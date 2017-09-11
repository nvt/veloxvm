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
import java.nio.charset.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.*;
import java.util.Vector;

public class VeloxCompiler {
    private static final String VELOX_PATH = System.getenv("VELOX_PATH");
    private static final Level DEFAULT_LOG_LEVEL = Level.INFO;
    private static final String VELOX_DEFAULT_FILE = "app.vm";
    private static VeloxTable<VeloxSymbol> internalSymbols;
    private VeloxApp app;
    static Logger logger = Logger.getLogger("se.sics.veloxj");

    public VeloxCompiler(String appName) throws VeloxException {
	app = new VeloxApp(appName);
    }

    public VeloxCompiler(VeloxApp app) {
	this.app = app;
    }

    public VeloxApp getApp() {
	return app;
    }

    public void execute(String filename) throws VeloxException {
        if (VELOX_PATH == null) {
	    throw new VeloxException("Unable to execute Velox without VELOX_PATH set");
        }

	logger.info("Executing " + filename + "...");

	Runtime runtime = Runtime.getRuntime();
        InputStream inputStream = null;
	try {
	    Path veloxPath = Paths.get(VELOX_PATH);
	    veloxPath = veloxPath.resolve("vm");
	    Process process = runtime.exec(veloxPath.toString() +
					   " " + filename);
	    inputStream = process.getInputStream();
	    BufferedReader in = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.ISO_8859_1));

	    /* Process all Velox output pertaining to the execution of this
	       application's expressions. */
	    String line;
	    while((line = in.readLine()) != null) {
		if (line.contains("EVAL")) {
		    line = line.substring(line.indexOf("EVAL") + 5);
		    logger.info(line);
		}
	    }

	    process.waitFor();
	    logger.info("Done! Exit code: " + process.exitValue());
	} catch (IOException e) {
	  logger.severe("Got an IO exception when executing Velox: " +
		      e.getMessage());
	} catch (InterruptedException e) {
	    logger.severe("Velox execution got interrupted: " + e.getMessage());
	} catch (IllegalThreadStateException e) {
	    logger.severe("Unable to get Velox's exit value:" + e.getMessage());
	} finally {
	    if (inputStream != null) {
	        try {
	            inputStream.close();
                } catch (IOException e) {
		    logger.fine("Unable to close an input stream");
		}
	    }
	}
    }

    public byte[] compile() throws VeloxException {
        logger.finer("Compiling the application " +
		     app.getDefaultRepresentation());
	VeloxByteCode byteCode = new VeloxByteCode(app);
	return byteCode.generate();
    }

    public static void saveByteCode(String filename, byte[] bytecode) {
	File file = new File(filename);
	FileOutputStream outputFile = null;

	try {
	    outputFile = new FileOutputStream(file);
	    outputFile.write(bytecode);
	    outputFile.close();
	} catch(FileNotFoundException e) {
	    logger.severe("Unable to save the bytecode in the file " +
			       filename);
	    logger.severe("Reason: " + e.getMessage());
	} catch (IOException e) {
	    logger.severe("Unable to save the bytecode in the file " +
			       filename);
	    logger.severe("Reason: " + e.getMessage());
	    if (file.delete() == false) {
	      logger.severe("Unable to delete a possibly incomplete bytecode file");
	    }
	}

	logger.info("Wrote the bytecode to " + filename);
    }

    private void setupTestApp() throws VeloxException {
	VeloxExpression expression, subExpression;

	expression = new VeloxExpression();
	expression.addArgument(new VeloxSymbol("+"));
	expression.addArgument(new VeloxInteger(1));
	expression.addArgument(new VeloxInteger(2));
	app.addExpression(expression);

	expression = new VeloxExpression();
	expression.addArgument(new VeloxSymbol("define"));
	expression.addArgument(new VeloxSymbol("var2"));
	expression.addArgument(new VeloxInteger(100));
	app.addExpression(expression);

	subExpression = new VeloxExpression();
	subExpression.addArgument(new VeloxSymbol("*"));
	subExpression.addArgument(new VeloxSymbol("var2"));
	subExpression.addArgument(new VeloxSymbol("var2"));

	expression = new VeloxExpression();
	expression.addArgument(new VeloxSymbol("+"));
	expression.addArgument(new VeloxInteger(10));
	expression.addArgument(subExpression);
	expression.addArgument(new VeloxInteger(20));
	expression.addArgument(new VeloxRational(99, -66));
	app.addExpression(expression);

	expression = new VeloxExpression();
	expression.addArgument(new VeloxSymbol("print"));
	expression.addArgument(new VeloxString("Hello, world!"));
	app.addExpression(expression);
    }

    public static void main(String[] args) {
	logger.setLevel(DEFAULT_LOG_LEVEL);
	logger.info("Velox Compiler started");

	if (VELOX_PATH == null) {
	    logger.info("VELOX_PATH is not set; will not execute the compiled app");
	}

	String appName = "TestApp";
	try {
	    VeloxCompiler compiler = new VeloxCompiler(appName);
	    compiler.setupTestApp();
	    byte[] bytecode = compiler.compile();
	    String filename = args.length > 0 ? args[0] : VELOX_DEFAULT_FILE;
	    compiler.saveByteCode(filename, bytecode);
	    if (VELOX_PATH != null) {
	        compiler.execute(filename);
            }
	} catch (VeloxException e) {
	    logger.severe("An error occurred while compiling " +
			  appName + ": ");
	    StringWriter stackTrace = new StringWriter();
	    e.printStackTrace(new PrintWriter(stackTrace));
	    logger.severe(stackTrace.toString());
	}
	logger.info("Exiting");
    }

    private static void generateInternalSymbols() throws VeloxException {
	String[] symbolNames = {
	    /* Mathematical functions. */
	    "+", "-", "*", "/", "gcd",
	    "lcm", "numerator", "denominator", "quotient",
	    "remainder", "modulo", "=", "/=", "<",
	    "<=", ">", ">=", "zero?",

	    /* Primitive functions. */
	    "bind", "return", "begin", "if", "define",
	    "set!", "and", "or", "apply", "quote",
	    "number?", "integer?", "rational?", "real?",
	    "complex?", "exact?", "inexact?", "procedure?",
	    "boolean?", "port?", "not", "eq?", "eqv?",
	    "equal?",

	    /* System functions. */
	    "system-info", "load-program", "import",
	    "get-devices", "print", "random", "time",
	    "get-programs", "program-info", "exit",

	    /* List functions. */
	    "list", "cons", "push", "pop", "car", "cdr",
	    "list-ref", "list-tail", "append", "remove",
	    "reverse", "length", "null?", "list?", "pair?",
	    "set-car!", "set-cdr!", "memq", "memv", "member",
	    "assq", "assv", "assoc",

	    /* Higher-order list functions. */
	    "map", "filter", "for-each", "reduce", "count",

	    /* Character functions. */
	    "char?", "char-compare", "char-class", "char->integer",
	    "integer->char", "char-upcase", "char-downcase",

	    /* String functions. */
	    "make-string", "string", "string?", "string-length",
	    "string-ref", "string-set!", "string->list",
	    "list->string", "vector->string", "string-fill!",
	    "string-compare", "substring", "string-append",
	    "string-copy", "string-split", "number->string",
	    "string->number",

	    /* Exception and condition functions. */
	    "guard", "raise",

	    /* Thread functions. */
	    "thread-create!", "thread-fork!", "thread-id",
	    "thread-join!", "thread-sleep!",
	    "thread-specific", "thread-specific-set!",
	    "thread-terminate!", "thread-yield!", "thread-stats",

	    /* Mutex functions. */
	    "mutex?", "make-mutex", "mutex-name", "mutex-specific",
	    "mutex-specific-set!", "mutex-state", "mutex-lock!",
	    "mutex-unlock!",

	    /* Vector functions. */
	    "make-vector", "vector", "vector?", "buffer?",
	    "vector-length", "vector-ref", "vector-set!",
	    "vector->list", "list->vector", "vector-fill!",

	    /* I/O functions. */
	    "input-port?", "output-port?", "current-input-port",
	    "current-output-port", "open-input-file", "open-output-file",
	    "close-input-port", "close-output-port", "read-char",
	    "read", "peek-char", "eof-object?", "char-ready?",
	    "write-char", "write", "display", "with-input-from-file",
	    "with-output-to-file",

	    /* Internet socket functions. */
	    "make-client", "make-server", "peer-name",
	    "accept-client", "incoming-client?", "addr->string",

	    /* Mathematical functions using floats. */
	    "floor", "ceiling", "round", "truncate", "exp",
	    "log", "sin", "cos", "tan", "asin", "acos",
	    "atan", "sqrt", "expt", "exact-to-inexact",
	    "inexact-to-exact",

	    /* Evaluation control functions. */
	    "call-with-current-continuation", "values", "call-with-values",
	    "dynamic-wind", "eval",

	    /* Bit manipulation functions. */
	    "bit-and", "bit-or", "bit-invert", "bit-not",
	    "bit-xor", "bit-shift",

	    /* Packet management functions. */
	    "construct-packet", "deconstruct-packet"
	};

	internalSymbols = new VeloxTable<VeloxSymbol>();
	for (int i = 0; i < symbolNames.length; i++) {
	    internalSymbols.insert(new VeloxSymbol(symbolNames[i]));
	}
    }

    public static VeloxTable<VeloxSymbol> getInternalSymbols() throws VeloxException {
	if (internalSymbols == null) {
	    generateInternalSymbols();
	}

	return internalSymbols;
    }
}
