#!/usr/bin/env node
/** -*- coding: utf-8 -*-
 * @fileoverview application on nodejs
 *
 */
'use strict'

const Geocoder = require('@geolonia/open-reverse-geocoder')
const GetOpt = require('node-getopt');

const usage =
      "Usage: node main.js [OPTION] LONGITUDE LATITUDE longitude latitude...\n"

const args = GetOpt.create(
    [
        ['d', 'delta=SECOND', '東西南北にずらす角度(秒単位)'],
        ['m', '', '東西南北にずらした9点を全て表示する'],
        ['p', '', '市町村まで表示する。デフォルトは都道府県だけ'],
        ['s', 'single', '指定された一点のみ処理し、周辺の8点は処理しない']
    ]).setHelp(
        usage + "\n[[OPTIONS]]\n"
    ).bindHelp().parseSystem()

var shifts = [["NW", 0, 0],
              ["N", 0, 0],
              ["NE", 0, 0],
              ["W", 0, 0],
              ["0", 0, 0],
              ["E", 0, 0],
              ["SW", 0, 0],
              ["S", 0, 0],
              ["SE", 1, 2]]

function set_delta(delta) {
    for (var y = -1; y <= 1; ++y) {
        for (var x = -1; x <= 1; ++x) {
            shifts[(y + 1) * 3 + (x + 1)][1] = x * delta
            shifts[(y + 1) * 3 + (x + 1)][2] = y * delta
        }
    }
}

function range(n) { return [...Array(n).keys()] }

async function main(args) {
    var proc, lon, lat

    var bad = ""
    if (args.argv.length % 2 != 0) {
        console.error(`bad number of arguments: ${args.argv.length}\n`)
        console.error(usage)
	process.exit(1)
    }

    for (var i = 0; i < args.argv.length; i += 2) {

        lon = parseFloat(args.argv[i])
        lat = parseFloat(args.argv[i+1])

        if (args.options['single']) {
            let result = await single_shift_for_loc(undefined, lon, lat)
            make_report(args.options, [result])
        }
        else {
            let d = args.options['delta'] ?? 3
            set_delta(d)

            let result = await Promise.all(
                range(9).map(async (idx) => { return await single_shift_for_loc(idx, lon, lat) } ) )
            make_report(args.options, result)
        }

    }
}

function make_report(opts, results) {
    if (opts['m']) {
        for (var i in range(results.length)) {
            let a = get_addr(opts['p'], results[i])

            console.log(shifts[i][0] + ":\t" + a)
        }
    }
    else {
        var m = new Map()
        for (var r of results) {
            if (r.code === '')
                continue
            m.set(get_addr(opts['p'], r), 1)
        }

        console.log([...m.keys()].join(","))
    }
}

function get_addr(flag_all, result) {
    if (flag_all)
        return result.prefecture + " " + result.city

    return result.prefecture
}

async function single_shift_for_loc(idx, lon, lat) {
    var loc
    if (idx === undefined)
        loc = [lon, lat]
    else {
        loc = [lon + 1/3600.0 * shifts[idx][1], lat + 1/3600.0 * shifts[idx][2]]
    }

    return await Geocoder.openReverseGeocoder(loc)
}

/*
 */
main(args)

