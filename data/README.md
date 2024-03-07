# yamalist
List of mountains

![](firstDiagram.svg)
<details>
  <summary>plantUML</summary>

```
@startuml firstDiagram

class List {
  id: ID
  name: String
}
note Left: 山のリスト (ex. 日本百名山)

class ListElement {
  id: ID
}
note left: リストの要素

class Point extends ListElement {
  names: [Name]
  type: TypeOfPoint
  latitude: float
  longitude: float
  altitude: float
  yamap: IDonYamap or null
  yamareco: Integer or null
}
note right: 山頂、峠、登山口、など

note right of Point::yamap
  YamapでのID
end note

note right of Point::yamareco
  YamarecoでのID
end note

class Prefecture {
  id: ID
  name: String
}
note bottom: 山頂などがある都道府県

class SummitGroup extends ListElement {
   name: Name
   summits: [ID]
}

note bottom of SummitGroup
複数の山頂からなる山 (ex. 燧岳=柴安嵓+俎嵓)
end note

note Left of SummitGroup::summits
  山に含まれる山頂。最初の山頂を代表とする。
end note

class IDonYamap {
  type: TypeOfPoint
  id: Integer
}

class Name {
  name: String
  yomi: [String]
}

note right of Name::yomi
  名前の読み仮名。一つの表記に複数の読み方がありえるので配列。
end note

note right of Point::names
  山頂などの名前。別名がある場合のために配列
end note

List "1" o-- "1..*" ListElement

Point "1" *-- "0..1" IDonYamap

SummitGroup "1" o- "1..*" Point

Point "1" o-- "1..*" Prefecture

Point "1" *-- "1..*" Name

@enduml
```

</details>
