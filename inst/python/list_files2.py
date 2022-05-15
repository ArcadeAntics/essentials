import os


all_files    = os.environ['R_ESSENTIALS_LIST_FILES2_ALL_FILES'   ] == 'TRUE'
full_names   = os.environ['R_ESSENTIALS_LIST_FILES2_FULL_NAMES'  ] == 'TRUE'
recursive    = os.environ['R_ESSENTIALS_LIST_FILES2_RECURSIVE'   ] == 'TRUE'
include_dirs = os.environ['R_ESSENTIALS_LIST_FILES2_INCLUDE_DIRS'] == 'TRUE'
nodotdot     = os.environ['R_ESSENTIALS_LIST_FILES2_NODOTDOT'    ] == 'TRUE'


outfile      = os.environ['R_ESSENTIALS_LIST_FILES2_OUTFILE']
outfile      = open(outfile, mode = 'w', encoding = 'UTF-8')


if not recursive:
    include_dirs = True


if recursive or not all_files:
    nodotdot = True


##for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
##    m = len(path)
##    n = m + 1
##    first = True
##    for root, dirs, files in os.walk(path):
##        if first:
##            first = False
##            root = ''
##            if full_names:
##                root = path + '/' + root
##            if not nodotdot:
##                outfile.writelines(root + '.\n')
##                outfile.writelines(root + '..\n')
##            if include_dirs:
##                outfile.writelines(root + dir  + '\n' for dir  in dirs  if all_files or not dir .startswith('.'))
##            outfile    .writelines(root + file + '\n' for file in files if all_files or not file.startswith('.'))
##            if not recursive:
##                break
##        else:
##            if all_files or '\\.' not in root[m:]:
##                root = root[n:].replace('\\', '/') + '/'
##                if full_names:
##                    root = path + '/' + root
##                if include_dirs:
##                    outfile.writelines(root + dir  + '\n' for dir  in dirs  if all_files or not dir .startswith('.'))
##                outfile    .writelines(root + file + '\n' for file in files if all_files or not file.startswith('.'))


if recursive:
    if include_dirs:
        if all_files:
            if full_names:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    n = len(path) + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            root += '/'
                        else:
                            root = path + '/' + root[n:].replace('\\', '/') + '/'
                        outfile.writelines(root + dir  + '\n' for dir  in dirs )
                        outfile.writelines(root + file + '\n' for file in files)
            else:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    n = len(path) + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            outfile.writelines(dir  + '\n' for dir  in dirs )
                            outfile.writelines(file + '\n' for file in files)
                        else:
                            root = root[n:].replace('\\', '/') + '/'
                            outfile.writelines(root + dir  + '\n' for dir  in dirs )
                            outfile.writelines(root + file + '\n' for file in files)
        else:
            if full_names:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    m = len(path)
                    n = m + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            root += '/'
                        elif '\\.' in root[m:]:
                            continue
                        else:
                            root = path + '/' + root[n:].replace('\\', '/') + '/'
                        outfile.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                        outfile.writelines(root + file + '\n' for file in files if not file.startswith('.'))
            else:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    m = len(path)
                    n = m + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            outfile.writelines(dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                            outfile.writelines(file + '\n' for file in files if not file.startswith('.'))
                        elif '\\.' in root[m:]:
                            continue
                        else:
                            root = root[n:].replace('\\', '/') + '/'
                            outfile.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                            outfile.writelines(root + file + '\n' for file in files if not file.startswith('.'))
    else:
        if all_files:
            if full_names:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    n = len(path) + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            root += '/'
                        else:
                            root = path + '/' + root[n:].replace('\\', '/') + '/'
                        outfile.writelines(root + file + '\n' for file in files)
            else:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    n = len(path) + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            outfile.writelines(file + '\n' for file in files)
                        else:
                            root = root[n:].replace('\\', '/') + '/'
                            outfile.writelines(root + file + '\n' for file in files)
        else:
            if full_names:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    m = len(path)
                    n = m + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            root += '/'
                        elif '\\.' in root[m:]:
                            continue
                        else:
                            root = path + '/' +root[n:].replace('\\', '/') + '/'
                        outfile.writelines(root + file + '\n' for file in files if not file.startswith('.'))
            else:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    m = len(path)
                    n = m + 1
                    first = True
                    for root, dirs, files in os.walk(path):
                        if first:
                            first = False
                            outfile.writelines(file + '\n' for file in files if not file.startswith('.'))
                        elif '\\.' in root[m:]:
                            continue
                        else:
                            root = root[n:].replace('\\', '/') + '/'
                            outfile.writelines(root + file + '\n' for file in files if not file.startswith('.'))
else:
    if nodotdot:
        if all_files:
            if full_names:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    for root, dirs, files in os.walk(path):
                        root += '/'
                        outfile.writelines(root + dir  + '\n' for dir  in dirs )
                        outfile.writelines(root + file + '\n' for file in files)
                        break
            else:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    for root, dirs, files in os.walk(path):
                        outfile.writelines(dir  + '\n' for dir  in dirs )
                        outfile.writelines(file + '\n' for file in files)
                        break
        else:
            if full_names:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    for root, dirs, files in os.walk(path):
                        root += '/'
                        outfile.writelines(root + dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                        outfile.writelines(root + file + '\n' for file in files if not file.startswith('.'))
                        break
            else:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    for root, dirs, files in os.walk(path):
                        outfile.writelines(dir  + '\n' for dir  in dirs  if not dir .startswith('.'))
                        outfile.writelines(file + '\n' for file in files if not file.startswith('.'))
                        break
    else:
        if all_files:
            if full_names:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    for root, dirs, files in os.walk(path):
                        root += '/'
                        outfile.write(root + '.\n')
                        outfile.write(root + '..\n')
                        outfile.writelines(root + dir  + '\n' for dir  in dirs )
                        outfile.writelines(root + file + '\n' for file in files)
                        break
            else:
                for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\t'):
                    for root, dirs, files in os.walk(path):
                        outfile.write('.\n')
                        outfile.write('..\n')
                        outfile.writelines(dir  + '\n' for dir  in dirs )
                        outfile.writelines(file + '\n' for file in files)
                        break
        else:
            raise ValueError("invalid 'all.files' and 'no..'; should never happen, please report!")
outfile.close()
