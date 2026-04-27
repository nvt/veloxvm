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
