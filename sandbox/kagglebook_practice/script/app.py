import sys
from pathlib import Path

import streamlit as st

import inspect
import textwrap
import numpy as np
import pandas as pd
from collections import OrderedDict

try:
    sys.path.append(str(Path(__file__).parent))
    import ch01
finally:
    sys.path.pop()


CHAPTERS = OrderedDict([
    ("ch01", ch01.app)
])

def main():
    chapter_name = st.sidebar.selectbox("Choose a chapter", list(CHAPTERS.keys()), 0)

    show_code = st.sidebar.checkbox("Show code", True)
    st.markdown(f"# {chapter_name}")

    app = CHAPTERS[chapter_name]
    app()

    if show_code:
        st.markdown("## Code")
        sourcelines, _ = inspect.getsourcelines(app)
        st.code(textwrap.dedent("".join(sourcelines[1:])))

if __name__ == '__main__':
    main()
