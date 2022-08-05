import os


all_files    = os.environ['R_ESSENTIALS_LIST_FILES2_ALL_FILES'   ] == 'TRUE'
full_names   = os.environ['R_ESSENTIALS_LIST_FILES2_FULL_NAMES'  ] == 'TRUE'
recursive    = os.environ['R_ESSENTIALS_LIST_FILES2_RECURSIVE'   ] == 'TRUE'
include_dirs = os.environ['R_ESSENTIALS_LIST_FILES2_INCLUDE_DIRS'] == 'TRUE'
nodotdot     = os.environ['R_ESSENTIALS_LIST_FILES2_NODOTDOT'    ] == 'TRUE'


if not recursive:
    include_dirs = True


if recursive or not all_files:
    nodotdot = True


specials = ['A:','B:','C:','D:','E:','F:','G:','H:','I:','J:','K:','L:','M:',
            'N:','O:','P:','Q:','R:','S:','T:','U:','V:','W:','X:','Y:','Z:',
            'a:','b:','c:','d:','e:','f:','g:','h:','i:','j:','k:','l:','m:',
            'n:','o:','p:','q:','r:','s:','t:','u:','v:','w:','x:','y:','z:']


with open(os.environ['R_ESSENTIALS_LIST_FILES2_OUTFILE'], mode = 'w', encoding = 'UTF-8') as con:
    
    
##    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##        n = len(path)
##        first = True
##        for root, dirs, files in os.walk(path):
##            if first:
##                first = False
##                if full_names:
##                    if root.endswith( ('/', '\\') ) or (root in specials):
##                        pass
##                    else:
##                        root += '/'
##                else:
##                    root = ''
##                if not nodotdot:
##                    con.writelines(root + '.\n')
##                    con.writelines(root + '..\n')
##                if include_dirs:
##                    con.writelines(root + dir  + '\n' for dir  in dirs  if all_files or not dir .startswith('.'))
##                con    .writelines(root + file + '\n' for file in files if all_files or not file.startswith('.'))
##                if not recursive:
##                    break
##            else:
##                root = root[n:]
##                if all_files or not (root.startswith('.') or ('\\.' in root) or ('/.' in root)):
##                    root = root.replace('\\', '/') + '/'
##                    if full_names:
##                        root = path + root
##                    elif root.startswith('/'):
##                        root = root[1:]
##                    if include_dirs:
##                        con.writelines(root + dir  + '\n' for dir  in dirs  if all_files or not dir .startswith('.'))
##                    con    .writelines(root + file + '\n' for file in files if all_files or not file.startswith('.'))
    
    
    
    
    
##    if recursive:
##        if include_dirs:
##            if all_files:
##                if full_names:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        n = len(path) + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                root += '/'
##                            else:
##                                root = path + '/' + root[n:].replace('\\', '/') + '/'
##                            con.writelines(root + dir  + '\n' for dir  in dirs )
##                            con.writelines(root + file + '\n' for file in files)
##                else:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        n = len(path) + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                con.writelines(dir  + '\n' for dir  in dirs )
##                                con.writelines(file + '\n' for file in files)
##                            else:
##                                root = root[n:].replace('\\', '/') + '/'
##                                con.writelines(root + dir  + '\n' for dir  in dirs )
##                                con.writelines(root + file + '\n' for file in files)
##            else:
##                if full_names:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        m = len(path)
##                        n = m + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                root += '/'
##                            elif '\\.' in root[m:]:
##                                continue
##                            else:
##                                root = path + '/' + root[n:].replace('\\', '/') + '/'
##                            con.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
##                            con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
##                else:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        m = len(path)
##                        n = m + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                con.writelines(dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
##                                con.writelines(file + '\n' for file in files if not file.startswith('.'))
##                            elif '\\.' in root[m:]:
##                                continue
##                            else:
##                                root = root[n:].replace('\\', '/') + '/'
##                                con.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
##                                con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
##        else:
##            if all_files:
##                if full_names:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        n = len(path) + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                root += '/'
##                            else:
##                                root = path + '/' + root[n:].replace('\\', '/') + '/'
##                            con.writelines(root + file + '\n' for file in files)
##                else:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        n = len(path) + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                con.writelines(file + '\n' for file in files)
##                            else:
##                                root = root[n:].replace('\\', '/') + '/'
##                                con.writelines(root + file + '\n' for file in files)
##            else:
##                if full_names:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        m = len(path)
##                        n = m + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                root += '/'
##                            elif '\\.' in root[m:]:
##                                continue
##                            else:
##                                root = path + '/' +root[n:].replace('\\', '/') + '/'
##                            con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
##                else:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        m = len(path)
##                        n = m + 1
##                        first = True
##                        for root, dirs, files in os.walk(path):
##                            if first:
##                                first = False
##                                con.writelines(file + '\n' for file in files if not file.startswith('.'))
##                            elif '\\.' in root[m:]:
##                                continue
##                            else:
##                                root = root[n:].replace('\\', '/') + '/'
##                                con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
##    else:
##        if nodotdot:
##            if all_files:
##                if full_names:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        for root, dirs, files in os.walk(path):
##                            root += '/'
##                            con.writelines(root + dir  + '\n' for dir  in dirs )
##                            con.writelines(root + file + '\n' for file in files)
##                            break
##                else:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        for root, dirs, files in os.walk(path):
##                            con.writelines(dir  + '\n' for dir  in dirs )
##                            con.writelines(file + '\n' for file in files)
##                            break
##            else:
##                if full_names:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        for root, dirs, files in os.walk(path):
##                            root += '/'
##                            con.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
##                            con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
##                            break
##                else:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        for root, dirs, files in os.walk(path):
##                            con.writelines(dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
##                            con.writelines(file + '\n' for file in files if not file.startswith('.'))
##                            break
##        else:
##            if all_files:
##                if full_names:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        for root, dirs, files in os.walk(path):
##                            root += '/'
##                            con.write(root + '.\n')
##                            con.write(root + '..\n')
##                            con.writelines(root + dir  + '\n' for dir  in dirs )
##                            con.writelines(root + file + '\n' for file in files)
##                            break
##                else:
##                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##                        for root, dirs, files in os.walk(path):
##                            con.write('.\n')
##                            con.write('..\n')
##                            con.writelines(dir  + '\n' for dir  in dirs )
##                            con.writelines(file + '\n' for file in files)
##                            break
##            else:
##                raise ValueError("invalid 'all.files' and 'no..'; should never happen, please report!")
    
    
    
    
    
    if recursive:
        if include_dirs:
            if all_files:
                if full_names:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        first = True
                        for root, dirs, files in os.walk(path):
                            print('path =', path)
                            print('root =', root)
                            if first:
                                first = False
                                if root.endswith( ('/', '\\') ) or (root in specials):
                                    pass
                                else:
                                    root += '/'
                            else:
                                root = path + root[n:].replace('\\', '/') + '/'
                            con.writelines(root + dir  + '\n' for dir  in dirs )
                            con.writelines(root + file + '\n' for file in files)
                else:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path) + 1 - (path.endswith( ('/', '\\') ) or (path in specials))
                        first = True
                        for root, dirs, files in os.walk(path):
                            if first:
                                first = False
                                con.writelines(dir  + '\n' for dir  in dirs )
                                con.writelines(file + '\n' for file in files)
                            else:
                                root = root[n:].replace('\\', '/') + '/'
                                con.writelines(root + dir  + '\n' for dir  in dirs )
                                con.writelines(root + file + '\n' for file in files)
            else:
                if full_names:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        first = True
                        for root, dirs, files in os.walk(path):
                            if first:
                                first = False
                                if root.endswith( ('/', '\\') ) or (root in specials):
                                    pass
                                else:
                                    root += '/'
                            else:
                                root = root[n:]
                                if root.startswith('.') or ('\\.' in root) or ('/.' in root):
                                    continue
                                root = path + root.replace('\\', '/') + '/'
                            con.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                            con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
                else:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path) + 1 - (path.endswith( ('/', '\\') ) or (path in specials))
                        first = True
                        for root, dirs, files in os.walk(path):
                            if first:
                                first = False
                                con.writelines(dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                                con.writelines(file + '\n' for file in files if not file.startswith('.'))
                            else:
                                root = root[n:]
                                if not (root.startswith('.') or ('\\.' in root) or ('/.' in root)):
                                    root = root.replace('\\', '/') + '/'
                                    con.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                                    con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
        else:
            if all_files:
                if full_names:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        first = True
                        for root, dirs, files in os.walk(path):
                            if first:
                                first = False
                                if root.endswith( ('/', '\\') ) or (root in specials):
                                    pass
                                else:
                                    root += '/'
                            else:
                                root = path + root[n:].replace('\\', '/') + '/'
                            con.writelines(root + file + '\n' for file in files)
                else:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path) + 1 - (path.endswith( ('/', '\\') ) or (path in specials))
                        first = True
                        for root, dirs, files in os.walk(path):
                            if first:
                                first = False
                                con.writelines(file + '\n' for file in files)
                            else:
                                root = root[n:].replace('\\', '/') + '/'
                                con.writelines(root + file + '\n' for file in files)
            else:
                if full_names:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        first = True
                        for root, dirs, files in os.walk(path):
                            if first:
                                first = False
                                if root.endswith( ('/', '\\') ) or (root in specials):
                                    pass
                                else:
                                    root += '/'
                            else:
                                root = root[n:]
                                if root.startswith('.') or ('\\.' in root) or ('/.' in root):
                                    continue
                                root = path + root.replace('\\', '/') + '/'
                            con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
                else:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path) + 1 - (path.endswith( ('/', '\\') ) or (path in specials))
                        first = True
                        for root, dirs, files in os.walk(path):
                            if first:
                                first = False
                                con.writelines(file + '\n' for file in files if not file.startswith('.'))
                            else:
                                root = root[n:]
                                if not (root.startswith('.') or ('\\.' in root) or ('/.' in root)):
                                    root = root.replace('\\', '/') + '/'
                                    con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
    else:
        if nodotdot:
            if all_files:
                if full_names:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        for root, dirs, files in os.walk(path):
                            if root.endswith( ('/', '\\') ) or (root in specials):
                                pass
                            else:
                                root += '/'
                            con.writelines(root + dir  + '\n' for dir  in dirs )
                            con.writelines(root + file + '\n' for file in files)
                            break
                else:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        for root, dirs, files in os.walk(path):
                            con.writelines(dir  + '\n' for dir  in dirs )
                            con.writelines(file + '\n' for file in files)
                            break
            else:
                if full_names:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        for root, dirs, files in os.walk(path):
                            if root.endswith( ('/', '\\') ) or (root in specials):
                                pass
                            else:
                                root += '/'
                            con.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                            con.writelines(root + file + '\n' for file in files if not file.startswith('.'))
                            break
                else:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        for root, dirs, files in os.walk(path):
                            con.writelines(dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                            con.writelines(file + '\n' for file in files if not file.startswith('.'))
                            break
        else:
            if all_files:
                if full_names:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        for root, dirs, files in os.walk(path):
                            if root.endswith( ('/', '\\') ) or (root in specials):
                                pass
                            else:
                                root += '/'
                            con.writelines(root + '.\n')
                            con.writelines(root + '..\n')
                            con.writelines(root + dir  + '\n' for dir  in dirs )
                            con.writelines(root + file + '\n' for file in files)
                            break
                else:
                    for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                        n = len(path)
                        for root, dirs, files in os.walk(path):
                            con.writelines('.\n')
                            con.writelines('..\n')
                            con.writelines(dir  + '\n' for dir  in dirs )
                            con.writelines(file + '\n' for file in files)
                            break
            else:
                raise ValueError("invalid 'all.files' and 'no..'; should never happen, please report!")

