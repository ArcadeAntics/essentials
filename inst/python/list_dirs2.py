import json
import os
import sys


with open(sys.argv[1], 'rb') as conn:
    paths, full_names, recursive = json.load(conn)


with open(sys.argv[1], mode = 'w', encoding = 'UTF-8', newline = '\n') as conn:
    
    
    def list_dirs(path, offset, recursive):
        try:
            scandir_iterator = os.scandir(path)
        except OSError:
            pass
        else:
            for dir_entry in scandir_iterator:
                if dir_entry.is_dir():
                    x = path + dir_entry.name
                    conn.write(x[offset:] + '\n')
                    if recursive:
                        x += '/'
                        list_dirs(x, offset, recursive)
    
    
    for path in paths:
        if not os.path.isdir(path):
            continue
        if recursive:
            if full_names:
                conn.write(path + '\n')
            else:
                conn.write('\n')
        n = len(path)
        if (n == 2 and path[1] == ':') or \
           (n >= 1 and (path[n - 1] == '/' or path[n - 1] == '\\')):
            pass
        else:
            path += '/'
            n += 1
        list_dirs(path, 0 if full_names else n, recursive)
