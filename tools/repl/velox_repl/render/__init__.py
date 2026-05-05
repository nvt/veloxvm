from velox_repl.render import decode, python, scheme
from velox_repl.render.decode import VObj, decode as decode_obj


def render_for(language: str, obj_encoding: bytes) -> str:
    obj = decode_obj(obj_encoding)
    if language == "scheme":
        return scheme.render(obj)
    if language == "python":
        return python.render(obj)
    raise ValueError(f"unknown language: {language}")


__all__ = ["VObj", "decode_obj", "render_for", "scheme", "python", "decode"]
