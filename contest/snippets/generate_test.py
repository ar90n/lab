#!/usr/local/bin/python3
import glob
import os
import sys

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

def convert_module( lang, module_name ):
    module_dir = os.path.join('.', module_name)
    test_template_path = os.path.join( module_dir, 'test.template' )
    if not os.path.exists( test_template_path ):
        return None

    func_pattern = '*' + suffix_map[lang]
    funcs = [func for func in glob.glob(os.path.join(module_dir,func_pattern))]

    primary_func = os.path.join( module_dir, module_name + suffix_map[lang] )
    try:
        i = funcs.index(primary_func)
        funcs[i], funcs[0] = funcs[0], funcs[i]
    except:
        pass

    res = []
    for func_path in funcs:
        func_name = os.path.basename(func_path).split('.')[0]
        content = ''.join( open(func_path).readlines() )[:-1]
        res.append( content )
    module_str = '\n\n'.join( res )

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

        for module in glob.glob('*'):
            if not os.path.isdir( module ):
                continue

            test_str = convert_module( lang, module )
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
