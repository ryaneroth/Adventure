import os

CWD = os.getcwd()
LOCATION = os.path.join(CWD, 'source/location.txt')
MESSAGES = os.path.join(CWD, 'source/messages.txt')
SHORT_DESCRIPTION = os.path.join(CWD, 'source/short_description.txt')
TRAVEL_TABLE = os.path.join(CWD, 'source/travel_table.txt')
VOCABULARY = os.path.join(CWD, 'source/vocabulary.txt')
DATA_DIR = os.path.join(CWD, 'data')
MESSAGES_DIR = os.path.join(DATA_DIR, 'messages')


def main():
    if not os.path.exists(DATA_DIR):
        os.mkdir(DATA_DIR)
    with open(LOCATION, 'r') as f:
        line = f.readline()
        while line:
            n, txt = line.split(maxsplit=1)
            sub_dir = os.path.join(DATA_DIR, n)
            if not os.path.exists(sub_dir):
                os.mkdir(sub_dir)
            with open(f'{sub_dir}/LOCATION.TXT', 'a') as nf:
                nf.write(f'{txt.strip()}\r\n')
            line = f.readline()
    with open(SHORT_DESCRIPTION, 'r') as f:
        line = f.readline()
        while line:
            n, txt = line.split(maxsplit=1)
            sub_dir = os.path.join(DATA_DIR, n)
            if not os.path.exists(sub_dir):
                os.mkdir(sub_dir)
            with open(f'{sub_dir}/SHORTDES.TXT', 'a') as nf:
                nf.write(f'{txt.strip()}\r\n')
            line = f.readline()
    with open(TRAVEL_TABLE, 'r') as f:
        line = f.readline()
        while line:
            cur_n, txt = line.split(maxsplit=1)
            sub_dir = os.path.join(DATA_DIR, cur_n)
            if not os.path.exists(sub_dir):
                os.mkdir(sub_dir)
            with open(f'{sub_dir}/TVLTABLE.TXT', 'a') as nf:
                dest_n, t_txt = txt.split(maxsplit=1)
                for t in t_txt.split():
                    with open(VOCABULARY, 'r') as tt:
                        v_line = tt.readline()
                        while v_line:
                            v_n, v_txt = v_line.split(maxsplit=1)
                            if t == v_n:
                                nf.write(f"{v_txt.strip()} {dest_n}\r\n")
                            v_line = tt.readline()
            line = f.readline()
    if not os.path.exists(MESSAGES_DIR):
        os.mkdir(MESSAGES_DIR)
    with open(MESSAGES, 'r') as f:
        line = f.readline()
        while line:
            n, txt = line.split(maxsplit=1)
            with open(f'{MESSAGES_DIR}/{n}.TXT', 'a') as nf:
                nf.write(f'{txt.strip()}\r\n')
                line = f.readline()




if __name__ == '__main__':
    main()