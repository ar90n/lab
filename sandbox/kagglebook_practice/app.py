import sys
from pathlib import Path

import streamlit as st

import inspect
import textwrap
import numpy as np
import pandas as pd
from collections import OrderedDict

try:
    import_path = str(Path(__file__).parent)
    sys.path.insert(0, import_path)
    from kagglebook_practice import  ch01, ch02
finally:
    sys.path.remove(import_path)


CHAPTERS = OrderedDict([
    ("ch01", ch01.SECTIONS),
    ("ch02", ch02.SECTIONS)
])

def main():
    chapter_name = st.sidebar.selectbox("Choose a chapter", list(CHAPTERS.keys()), 0)

    sections = CHAPTERS[chapter_name]
    section_name = st.sidebar.selectbox("Choose a sections", list(sections.keys()), 0)

    show_code = st.sidebar.checkbox("Show code", True)
    st.markdown(f"# {chapter_name}")
    st.markdown(f"## {section_name}")

    app = sections[section_name]
    app()

    if show_code:
        st.markdown("## Code")
        sourcelines, _ = inspect.getsourcelines(app)
        st.code(textwrap.dedent("".join(sourcelines[1:])))

if __name__ == '__main__':
    main()
