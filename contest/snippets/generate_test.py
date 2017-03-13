#!/usr/local/bin/python3
import glob
import os
import sys
import functools

snippet_prefix = "____"

snippet_template = """snippet %s%s "%s"
%s
endsnippet"""

replace_str = "{- %%MODULE%% -}"

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

def remove_comment( str_array, lang ):
    return filter( lambda l: not l.startswith(comment_prefix_map[lang]), str_array)

def get_deps( str_array, lang ):
    return map( lambda l : (l[2:-1]).rstrip().lstrip(), filter( lambda l: l.startswith(comment_prefix_map[lang]), str_array))

def toposort( dep , roots = None ):
    if roots == None:
        roots = dep.keys()
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
    for k in roots:
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
        snippet_key = os.path.join(module_name,os.path.basename(func_path))
        res[snippet_key] = content
        dep[snippet_key] = get_deps(lines, lang)

    return (res,dep)

def convert_module2( lang, module_name, res, dep ):
    module_dir = os.path.join('.', module_name)
    test_template_path = os.path.join( module_dir, 'test.template' )
    if not os.path.exists( test_template_path ):
        return None

    func_pattern = '*' + suffix_map[lang]
    funcs = [func[2:] for func in glob.glob(os.path.join(module_dir,func_pattern))]

    primary_func = os.path.join( module_dir, module_name + suffix_map[lang] )
    try:
        i = funcs.index(primary_func)
        funcs[i], funcs[0] = funcs[0], funcs[i]
    except:
        pass

    order = toposort( dep, funcs )
    snippets_strs = []
    for k in order:
        snippets_strs.append( res[k] )
    module_str = '\n\n'.join( snippets_strs )

    test_template = ''.join(open(test_template_path).readlines())[:-1]
    return test_template.replace( replace_str, module_str )

def main():
    langs = [lang for lang in glob.glob('*') if os.path.isdir( lang ) ]

    root_dir = os.path.dirname( os.path.abspath(__file__) )
    for lang in langs:
        if not lang in suffix_map:
            continue

        lang_dir = os.path.join( root_dir, lang )
        os.chdir( lang_dir )

        res,dep = functools.reduce( lambda a,b: ({**(a[0]),**(b[0])},{**(a[1]),**(b[1])}), [ convert_module( lang, module ) for module in glob.glob('*') if os.path.isdir( module ) and module != 'test'],({},{}))
        for module in glob.glob('*'):
            if not os.path.isdir( module ):
                continue

            test_str = convert_module2( lang, module, res, dep )
            if test_str is None:
                continue

            output_file_name = 'test_' + module + suffix_map[lang]
            output_file_path = os.path.join( 'test', output_file_name )
            with open( output_file_path, 'w' ) as f:
                f.write(test_str)

        os.chdir( root_dir )
    pass

if __name__ == '__main__':
    main()
