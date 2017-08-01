#!/usr/local/bin/python3
import glob
import os
import sys
import functools

snippet_prefix = "____"

ultsnippet_template = """snippet %s%s "%s"
%s
endsnippet"""

neosnippets_template = """snippet %s%s
%s"""

suffix_map = {
    "haskell" : ".hs",
    "cpp" : ".cpp",
    "fsharp" : ".fsx"
}

comment_prefix_map = {
    "haskell" : "//",
    "cpp" : "//",
    "fsharp" : "//"
}

def render_snippet( prefix, name, option, content):
    return ultsnippet_template % ( prefix, name, "", content )
#    content = '    ' + content.replace('\n', '\n    ')
#    return neosnippets_template % ( prefix, name, content )

def remove_comment( str_array, lang ):
    return filter( lambda l: not l.startswith(comment_prefix_map[lang]), str_array)

def get_deps( str_array, lang ):
    return map( lambda l : (l[2:-1]).rstrip().lstrip(), filter( lambda l: l.startswith(comment_prefix_map[lang]), str_array))

def toposort( dep ):
    visited = dict([ (k,0) for k in dep.keys() ])
    def doit( root ):
        res = []
        if visited[root] == 0:
            visited[root] = 1
            for k2 in dep[root]:
                res = res + doit( k2 )
            res.append(root)
            visited[root] = 2
        elif visited[root] == 1:
            raise "has loop"

        return res

    res = []
    for k in dep.keys():
        if visited[k] == 0:
            res = res + doit( k )

    return res

def convert_module( lang, module_name ):
    module_dir = os.path.join('.', module_name)
    func_pattern = '*' + suffix_map[lang]
    funcs = [func for func in glob.glob(os.path.join(module_dir,func_pattern))]

    res = {}
    dep = {}
    for func_path in funcs:
        func_name = os.path.basename(func_path).split('.')[0]
        lines = open(func_path).readlines()
        content = ''.join( remove_comment( lines, lang ) )[:-1]
        snippet_element = render_snippet( snippet_prefix, func_name, "", content )
        snippet_key = os.path.join(module_name,os.path.basename(func_path))
        res[snippet_key] = snippet_element
        dep[snippet_key] = get_deps(lines, lang)

    return (res,dep)

def main():
    langs = [lang for lang in glob.glob('*') if os.path.isdir( lang ) ]

    root_dir = os.path.dirname( os.path.abspath(__file__) )
    for lang in langs:
        if not lang in suffix_map:
            continue

        dep_map = {}
        lang_dir = os.path.join( root_dir, lang )
        os.chdir( lang_dir )

        res,dep = functools.reduce( lambda a,b: ({**(a[0]),**(b[0])},{**(a[1]),**(b[1])}), [ convert_module( lang, module ) for module in glob.glob('*') if os.path.isdir( module ) and module != 'test'],({},{}))
        order = toposort( dep )
        snippets_strs = []
        for k in order:
            snippets_strs.append( res[k] )
        snippets_str = '\n\n'.join( snippets_strs )

        os.chdir( root_dir )
        if 0 < len( snippets_str ):
            output_file_name = lang + ".snippets"
            with open( output_file_name, 'w' ) as f:
                f.write(snippets_str)
    pass

if __name__ == '__main__':
    main()
