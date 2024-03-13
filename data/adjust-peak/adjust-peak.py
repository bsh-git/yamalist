#!/usr/bin/env python3
# encoding: utf-8

import argparse
import fileinput
import re
import os
#import sys

#for f in sys.argv[1:]:
#    pass

class Sankaku:
    def __init__(self, code, name, lon, lat):
        self.code = code
        self.name = name
        self.lat = lat
        self.lon = lon

sankaku = []

#from optparse import OptionParser

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-T', '--find-peak-ocr', action='store_true', help='keep output from OCR')
    parser.add_argument('-K', '--keep-ocr', action='store_true', help='keep output from OCR')
    parser.add_argument('-f', '--force', action='store_true', help='always try to adjust peaks')
    parser.add_argument('inputfiles', help='positional arguments', nargs="*")
    args = parser.parse_args()
    #
    #
    with fileinput.FileInput(files = args.inputfiles) as input:
        process(args, input)

#GSI_TILE = 'gsi-tile'
GSI_TILE = '/home/bsh/Proj/yamalist/data/gsi-tile/dist-newstyle/build/x86_64-linux/ghc-9.0.2/gsi-tile-0.1.0.0/x/gsi-tile/build/gsi-tile/gsi-tile'

def process(args, input):
    for line in input:
        line = line.removesuffix("\n")
        fields = line.split(sep='\t')

        # 三角点データ
        if len(fields) == 4:
            sankaku.append(Sankaku(fields[3], fields[0], float(fields[1]), float(fields[2])))
            continue

        if fields[1] != "D":
            print(line)
        else:
            id=fields[0]
            lon = fields[2]
            lat = fields[3]

            snkk = lookup_sankaku(lon, lat)
            if snkk:
                fields[2:4] = ["{}".format(snkk.lon), "{}".format(snkk.lat)]
                fields[9] = snkk.code
                print("\t".join(fields))
                continue

            if not (fields[9] == "" or fields[9] == "-") and not args.force:
                print(line)
                continue

            if not args.find_peak_ocr:
                print(line)
                continue

            keep = "-K" if args.keep_ocr else ""

            cmd = "{} --mark -O {}.png -T {} '{}' '{}'".format(
                GSI_TILE,
                id,
                keep,
                remove_singlequote_from_angle(lon),
                remove_singlequote_from_angle(lat))

            #print(cmd, file=sys.stderr)
            peak = None
            for gstoutput in os.popen(cmd, mode='r'):
                if gstoutput.startswith("Peak="):
                    line = gstoutput[5:].removesuffix("\n")
                    peak = line.split(",")
            if peak:
                fields[2:4] = peak
                fields[9] = "o"
                print("\t".join(fields))
            else:
                print(line)

TRANSPOSE_DICT = str.maketrans("'", ":")

def remove_singlequote_from_angle(s):
    return s.translate(TRANSPOSE_DICT)

CLOSE_ENOUGH = 1.0/3600

def lookup_sankaku(lon, lat):
    if isinstance(lon, str):
        lon = convert_angle_to_float(lon)
    if isinstance(lat, str):
        lat = convert_angle_to_float(lat)

    # TODO: faster search
    for s in sankaku:
        if abs(s.lon - lon) < CLOSE_ENOUGH and abs(s.lat - lat) < CLOSE_ENOUGH:
            return s
    return None

ANGLE_RE=re.compile(r"(\d+)[°度l]\s*(\d+)['分]\s*([0-9.]+)" r'["秒]')

def convert_angle_to_float(s):
    m = ANGLE_RE.match(s)
    if m:
        return float(m[1]) +  float(m[2]) / 60.0  + float(m[3]) / 3600.0
    else:
        return float(s)

if __name__ == "__main__":
    main()
