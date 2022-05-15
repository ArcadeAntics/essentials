import os


full_names = os.environ['R_ESSENTIALS_LIST_DIRS2_FULL_NAMES'] == 'TRUE'
recursive  = os.environ['R_ESSENTIALS_LIST_DIRS2_RECURSIVE' ] == 'TRUE'


outfile      = os.environ['R_ESSENTIALS_LIST_DIRS2_OUTFILE']
outfile      = open(outfile, mode = 'w', encoding = 'UTF-8')


##for path in os.environ['R_ESSENTIALS_LIST_DIRS2_PATH'].split('\t'):
##    n = len(path) + 1
##    first = True
##    for root, dirs, files in os.walk(path):
##        if first:
##            first = False
##            if recursive:
##                if full_names:
##                    outfile.write(path + '\n')
##                else:
##                    outfile.write('.\n')
##            root = ''
##            if full_names:
##                root = path + '/' + root
##            outfile.writelines(root + dir + '\n' for dir in dirs)
##            if not recursive:
##                break
##        else:
##            root = root[n:].replace('\\', '/') + '/'
##            if full_names:
##                root = path + '/' + root
##            outfile.writelines(root + dir + '\n' for dir in dirs)


if recursive:
    if full_names:
        for path in os.environ['R_ESSENTIALS_LIST_DIRS2_PATH'].split('\t'):
            n = len(path) + 1
            first = True
            for root, dirs, files in os.walk(path):
                if first:
                    first = False
                    outfile.write(path + '\n')
                    root = path + '/'
                else:
                    root = path + '/' + root[n:].replace('\\', '/') + '/'
                outfile.writelines(root + dir + '\n' for dir in dirs)
    else:
        for path in os.environ['R_ESSENTIALS_LIST_DIRS2_PATH'].split('\t'):
            n = len(path) + 1
            first = True
            for root, dirs, files in os.walk(path):
                if first:
                    first = False
                    outfile.write('.\n')
                    outfile.writelines(dir + '\n' for dir in dirs)
                else:
                    root = root[n:].replace('\\', '/') + '/'
                    outfile.writelines(root + dir + '\n' for dir in dirs)
else:
    if full_names:
        for path in os.environ['R_ESSENTIALS_LIST_DIRS2_PATH'].split('\t'):
            for root, dirs, files in os.walk(path):
                root = path + '/'
                outfile.writelines(root + dir + '\n' for dir in dirs)
                break
    else:
        for path in os.environ['R_ESSENTIALS_LIST_DIRS2_PATH'].split('\t'):
            for root, dirs, files in os.walk(path):
                outfile.writelines(dir + '\n' for dir in dirs)
                break
outfile.close()
