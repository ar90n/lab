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
    s = f"""    def {m.name}(self):
        ...
"""
    print(m.name)
#		for member in reversed(self.members):
#			if (('method' == member.idl_type) and (name == member.name)
#			    	and ((argument_names is None) or member.matches_argument_names(argument_names))):
#				return member
#

# %%
# %%
import widlparser
# %%
widlparser.constructs.Interface
		output = '[Interface: ' + Construct.__repr__(self)
		output += '[partial] ' if (self.partial) else ''
		output += '[name: ' + repr(self._name) + '] '
		output += repr(self.inheritance) if (self.inheritance) else ''
		output += '[members: \n'
		for member in self.members:
			output += '  ' + repr(member) + '\n'
		return output + ']]'
# %%
parser["InputEvent"]['constructor'].arguments.values()[0].type.method_name
# %%
print(repr(parser))
# %%
str(parser)
# %%
import html
class Marker(object):
    def markup_construct(self, text, construct):
        return ('<c ' + construct.idl_type + '>', '</c>')

    def markup_type(self, text, construct):
        return ('<t>', '</t>')

    def markup_primitive_type(self, text, construct):
        return ('<p>', '</p>')

    def markup_buffer_type(self, text, construct):
        return ('<b>', '</b>')

    def markup_string_type(self, text, construct):
        return ('<s>', '</s>')

    def markup_object_type(self, text, construct):
        return ('<o>', '</o>')

    def markup_type_name(self, text, construct):
        return ('<tn>', '</tn>')

    def markup_name(self, text, construct):
        return ('<n>', '</n>')

    def markup_keyword(self, text, construct):
        print(text)
        return ('<k>', '</k>')

    def markup_enum_value(self, text, construct):
        return ('<ev>', '</ev>')

    def encode(self, text):
        return html.escape(text, quote=False)

# %%
print(parser.markup(Marker()))

# %%
for construct in parser.constructs:
    if construct.normal_name != "Element":
        continue
    print(str(construct.idl_type) + ': ' + str(construct.normal_name))
    print(construct.__dict__)
    #for member in construct:
    #    print('    ' + member.idl_type + ': ' + str(member.normal_name) + ' (' + str(member.name) + ')')
    #    print(f"        {list(member.arguments or [])}")
    #    print(f"        {member.items()}")
# %%
dir(parser)
# %%
set(parser.keys())
# %%
parser['Element'].
# %%
dir(parser.constructs)
# %%
from typing import Protocol, Callable, Type, Any

# Define a method
def my_method(self):
    pass

# Create a protocol dynamically
MyProtocol = type('MyProtocol', (Protocol,), {'my_method': my_method})

# %%
dir(MyProtocol)
# %%
def class_to_code(cls):
    name = cls.__name__
    bases = ", ".join(base.__name__ for base in cls.__bases__)
    attrs = ", ".join(f"{key}={value}" for key, value in cls.__dict__.items() if not key.startswith("__"))

    return f"class {name}({bases}):\n    {attrs}"

# %%
print(class_to_code(MyProtocol))
# %%
