# Copyright (c) 2026, RISE Research Institutes of Sweden AB
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived
#    from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.

"""
PyVelox compiler error type that carries source location.

When the translator hits an unsupported construct or other compile-time
problem, it raises PyveloxCompileError with the offending AST node's
lineno/col_offset attached. The CLI formats the result as
`path:line:col: <message>` and, when source lines are available, prints
the offending line with a caret under the column.
"""


class PyveloxCompileError(Exception):
    """A compilation error with optional source location."""

    def __init__(self, message, lineno=None, col_offset=None,
                 source_lines=None):
        self.raw_message = message
        self.lineno = lineno
        self.col_offset = col_offset
        self.source_lines = source_lines
        super().__init__(self.format())

    def format(self, source_path=None):
        """Render the error as `[path:]line:col: message` plus a snippet
        of the offending line when source_lines is available."""
        prefix_parts = []
        if source_path is not None:
            prefix_parts.append(str(source_path))
        if self.lineno is not None:
            prefix_parts.append(str(self.lineno))
            if self.col_offset is not None:
                prefix_parts.append(str(self.col_offset + 1))
        prefix = ":".join(prefix_parts) + ": " if prefix_parts else ""

        out = f"{prefix}{self.raw_message}"

        if (self.source_lines and self.lineno is not None
                and 1 <= self.lineno <= len(self.source_lines)):
            line_text = self.source_lines[self.lineno - 1].rstrip("\n")
            out += f"\n    {line_text}"
            if self.col_offset is not None:
                out += "\n    " + " " * self.col_offset + "^"
        return out
