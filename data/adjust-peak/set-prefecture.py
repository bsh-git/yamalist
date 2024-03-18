#!/usr/bin/env python3
# encoding: utf-8

# 山頂の所在都道府県を調べる

import argparse
import fileinput
import io
import re
import os
import sys

#for f in sys.argv[1:]:
#    pass

#from optparse import OptionParser

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--delta', action='store', help='delta in seconds')
    parser.add_argument('-f', '--force', action='store_true', help='always overwrite')
    parser.add_argument('inputfiles', help='positional arguments', nargs="*")
    args = parser.parse_args()
    #
    #
    with fileinput.FileInput(files = args.inputfiles) as input:
        process(args, input)

LON_FLD=2
LAT_FLD=3
PREFECTURE_FLD=5

def process(args, input):
    buf = []
    err = 0

    for line in input:
        line = line.removesuffix("\n")
        fields = line.split(sep='\t')

        if len(fields) < 1 or fields[1] != "D":
            buf.append((False, line))
        else:
            old_pref = fields[PREFECTURE_FLD]
            if not args.force and old_pref != "" and old_pref != "-":
                buf.append((False, line))
            else:
                lon = convert_angle_to_float(fields[LON_FLD])
                lat = convert_angle_to_float(fields[LAT_FLD])

                buf.append((True, lon, lat, fields))

        if len(buf) >= 300:
            err += flush(buf, args.delta)
            buf = []

    if len(buf) > 0:
        err += flush(buf, args.delta)

    if err > 0:
        process.exit(1)

def flush(buf, delta):
    opt_d = "" if delta == None  else ("-d " + delta)
    cmdbuf = io.StringIO()
    cmdbuf.write("revgeocoder {} ".format(opt_d))
    count = 0

    for d in buf:
        if d[0]:
            (lon, lat, fields) = d[1:]
            cmdbuf.write("{} {} ".format(lon, lat))
            count += 1

    if count > 0:
        cmd = cmdbuf.getvalue()
        #print("cmd="+cmd, file=sys.stderr)
        pipe = os.popen(cmd, mode='r')
    else:
        # this is just to keep Python happy
        pipe = open('/dev/null', 'r')

    count2 = 0
    for d in buf:
        if not d[0]:
            print(d[1])
            continue

        (lon, lat, fields) = d[1:]

        new_loc = pipe.readline()

        prefs = map(pref_to_number, new_loc.removesuffix("\n").split(sep=','))
        new_pref = ",".join(map(str, sorted(prefs)))

        old_pref = fields[PREFECTURE_FLD]
        if old_pref != "" and old_pref != "-" and old_pref != new_pref:
            print("Overwriting {} to {} for #{}".format(old_pref, new_pref, fields[0]),
                  file=sys.stderr)

        fields[PREFECTURE_FLD] = new_pref

        print("\t".join(fields))

        count2 += 1

    if count != count2:
        print("Something went wrong: {} != {}".format(count, count2))
        return 1

    return 0

PREF_CODE = {
    "北海道": 1, "青森県": 2, "岩手県": 3, "宮城県": 4,
    "秋田県": 5, "山形県": 6, "福島県": 7, "茨城県": 8,
    "栃木県": 9, "群馬県": 10, "埼玉県": 11, "千葉県": 12,
    "東京都": 13, "神奈川県": 14, "新潟県": 15, "富山県": 16,
    "石川県": 17, "福井県": 18, "山梨県": 19, "長野県": 20,
    "岐阜県": 21, "静岡県": 22, "愛知県": 23, "三重県": 24,
    "滋賀県": 25, "京都府": 26, "大阪府": 27, "兵庫県": 28,
    "奈良県": 29, "和歌山県": 30, "鳥取県": 31, "島根県": 32,
    "岡山県": 33, "広島県": 34, "山口県": 35, "徳島県": 36,
    "香川県": 37, "愛媛県": 38, "高知県": 39, "福岡県": 40,
    "佐賀県": 41, "長崎県": 42, "熊本県": 43, "大分県": 44,
    "宮崎県": 45, "鹿児島県": 46, "沖縄県": 47,
}

def pref_to_number(s):
    return PREF_CODE[s] or 99

ANGLE_RE=re.compile(r"(\d+)[°度l]\s*(\d+)['分]\s*([0-9.]+)" r'["秒]')

def convert_angle_to_float(s):
    m = ANGLE_RE.match(s)
    if m:
        return float(m[1]) +  float(m[2]) / 60.0  + float(m[3]) / 3600.0
    else:
        return float(s)

if __name__ == "__main__":
    main()
