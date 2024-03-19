import dbfread
import os
import pandas
import sys


def convert_dbf_to_csv(input_dir = None, output_dir = None, names = None):
    if input_dir  is None: input_dir  = '.'
    if output_dir is None: output_dir = '.'
    if names      is None: names = []
    if isinstance(names, str): names = [names]
    for name in names:
        input_path = os.path.join(input_dir, name)
        output_path = os.path.join(output_dir, name[:-3] + 'csv')
        db = dbfread.DBF(input_path)
        df = pandas.DataFrame(db)
        df.to_csv(output_path, index=False)
    return


if __name__ == '__main__':
    input_dir = '.'
    output_dir = '.'
    names = []
    
    
    input_slash = '--input/'
    output_slash = '--output/'
    if os.name == 'nt':
        input_slash = (input_slash, '--input\\')
        output_slash = (output_slash, '--output\\')
    
    
    for argv in sys.argv[1:]:
        if argv.startswith('--input='):
            input_dir = argv[8:]
            if input_dir == '--output':
                input_dir = output_dir
            elif input_dir.startswith(output_slash):
                input_dir = output_dir + input_dir[8:]
        elif argv.startswith('--output='):
            output_dir = argv[9:]
            if output_dir == '--input':
                output_dir = input_dir
            elif output_dir.startswith(input_slash):
                output_dir = input_dir + output_dir[7:]
        else:
            names += argv.split(os.pathsep)
            if names and names[-1] == '':
                del names[-1]
    convert_dbf_to_csv(input_dir, output_dir, names)
