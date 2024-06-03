import json
import os
import sys


with open(sys.argv[1], 'rb') as conn:
    paths, all_files, full_names, recursive, include_dirs, no_dots = json.load(conn)


with open(sys.argv[1], mode = 'w', encoding = 'UTF-8', newline = '\n') as conn:
    
    
    def list_files(path, offset, all_files, recursive, include_dirs, allow_dots):
        try:
            scandir_iterator = os.scandir(path)
        except OSError:
            pass
        else:
                # if all_files or not dir_entry.name.startswith('.'):
                #     x = path + dir_entry.name
                #     not_dot = dir_entry.name != '.' and dir_entry.name != '..'
                #     if recursive:
                #         if dir_entry.is_dir():
                #             if not_dot:
                #                 if include_dirs:
                #                     conn.write(x[offset:] + '\n')
                #                 x += '/'
                #                 list_files(x, offset, all_files, recursive, include_dirs, allow_dots)
                #             continue
                #     if not_dot or allow_dots:
                #         conn.write(x[offset:] + '\n')
            if all_files and not recursive and allow_dots:
                if offset:
                    conn.write('.\n..\n')
                else:
                    conn.write(path + '.\n' + path + '..\n')
            for dir_entry in scandir_iterator:
                if all_files or not dir_entry.name.startswith('.'):
                    x = path + dir_entry.name
                    if recursive:
                        if dir_entry.is_dir():
                            if include_dirs:
                                conn.write(x[offset:] + '\n')
                            x += '/'
                            list_files(x, offset, all_files, recursive, include_dirs, allow_dots)
                            continue
                    conn.write(x[offset:] + '\n')
    
    
    for path in paths:
        if not os.path.isdir(path):
            continue
        n = len(path)
        if (n == 2 and path[1] == ':') or \
           (n >= 1 and (path[n - 1] == '/' or path[n - 1] == '\\')):
            pass
        else:
            path += '/'
            n += 1
        list_files(path, 0 if full_names else n, all_files, recursive, include_dirs, not no_dots)
