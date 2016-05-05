#!/usr/local/bin/python3
import glob
import os
import sys

snippet_prefix = "____"

snippet_template = """snippet %s%s "%s"
%s
endsnippet"""

suffix_map = {
    "haskell" : ".hs",
    "cpp" : ".cpp"
}

def convert_module( lang, module_name ):
    module_dir = os.path.join('.', module_name)
    func_pattern = '*' + suffix_map[lang]
    funcs = [func for func in glob.glob(os.path.join(module_dir,func_pattern))]

    res = []
    for func_path in funcs:
        func_name = os.path.basename(func_path).split('.')[0]
        content = ''.join( open(func_path).readlines() )[:-1]
        snippet_element = snippet_template % ( snippet_prefix, func_name, "", content )
        res.append( snippet_element )

    return '\n\n'.join( res )

def main():
    langs = [lang for lang in glob.glob('*') if os.path.isdir( lang ) ]

    root_dir = os.path.dirname( os.path.abspath(__file__) )
    for lang in langs:
        if not lang in suffix_map:
            continue

        lang_dir = os.path.join( root_dir, lang )
        os.chdir( lang_dir )

        modules = [ convert_module( lang, module ) for module in glob.glob('*') if os.path.isdir( module ) and module != 'test']
        snippets_str = '\n\n'.join( modules )

        os.chdir( root_dir )
        if 0 < len( snippets_str ):
            output_file_name = lang + ".snippets"
            with open( output_file_name, 'w' ) as f:
                f.write(snippets_str)
    pass

if __name__ == '__main__':
    main()
