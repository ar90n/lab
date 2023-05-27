# %%
from pathlib import Path
from widlparser import Construct, Parser

# %%
WEBIDL_FILE_DIR = Path(__file__).parent / "gecko-dev" / "dom" / "webidl"

# %%
class UI:
    def warn(self, str):
        print(str)

    def note(self, str):
        print(str)

parser = Parser(ui=UI())
for p in WEBIDL_FILE_DIR.glob("*.webidl"):
    parser.parse(p.read_text())

# %%
html_element = parser["HTMLElement"]
html_element.name
ss = [f"class {html_element.name}(Protocol):"]
for m in html_element.members:
    if m.idl_type != 'method':
        continue

    args = [""]
    for ma in m.arguments:
        #ma.type.type.type.type.type
        #ma.optional
        args.append(f"{ma.name}: {ma.type.type.type.type.type}")
    s = f"""    def {m.name}(self{",".join(args)}):
        ...
"""
    ss.append(s)
print("\n".join(ss))
# %%
