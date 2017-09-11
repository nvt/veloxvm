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

public class VeloxTable<T> {
    private static final int VELOX_TABLE_MAX_SIZE = 256;

    private Vector<T> items;

    public VeloxTable() {
      items = new Vector<T>(VELOX_TABLE_MAX_SIZE);
    }

    public Vector<T> vectorize() {
      return items;
    }

    public T get(int index) throws VeloxException {
	if(index >= items.size()) {
	    throw new VeloxException("Out of bounds read from a table");
	}
	return items.elementAt(index);
    }

    public int find(T item) {
	int i = 0;
	for (T tableItem : items) {
	    if (item.equals(tableItem)) {
		return i;
	    }
            i++;
        }
	return -1;
    }

    public int insert(T item) throws VeloxException {
	if (items.size() == VELOX_TABLE_MAX_SIZE) {
	    throw new VeloxException("Insert in a full table");
	}
	items.add(item);
	return items.size() - 1;
    }
}
