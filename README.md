# yamalist

* 「日本百名山」のような山のリストを地図上に表示する
* 山頂の未踏/既踏状態をブラウザのローカルストレージに保存する
---

[hannou100-openlayers](https://github.com/bsh-git/hannou100-openlayers/) を複数のリストが扱えるように拡張したもの

地図表示には [Openlayers](https://openlayers.org/) を使用


# Subfolders

## data

山頂などのデータとリストの定義

### data/hannou100-to-data
hannou100-openlayers で使っていた geojsonを yamalist用のデータファイルに変換する
