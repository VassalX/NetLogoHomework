extensions [table] ; розширення хеш - таблиці

breed [figures figure]

undirected-link-breed [edges edge]

figures-own [
  domain
  possible-steps
  step-performed?
  message-queue  ;; список вхідних повідомлень у форматі [тип-повідомлення текст-повідомлення]
  lower-naybors  ;; список сусідів з меншим пріоритетом
  naybors        ;; розширений список сусідів
  local-view     ;; відображення виду (номер-агента - колір)
  no-goods       ;; список пар (агент колір), які є обмеженнями
]

edges-own [weight]

globals [all-positions x-positions y-positions]

to setup
  clear-all
  reset-ticks
  ask patches[
    set pcolor grey
  ]
  set-positions
  draw-board
  create-figures queens [
    setxy 0 0
    set color white
    set domain all-positions
    set shape "chess queen"
    set step-performed? false
    set possible-steps []
  ]
  create-figures kings [
    setxy 1 0
    set color white
    set domain all-positions
    set shape "chess king"
    set step-performed? false
    set possible-steps []
  ]
  create-figures rooks [
    setxy 2 0
    set color white
    set domain all-positions
    set shape "chess king"
    set step-performed? false
    set possible-steps []
  ]

  create-figures knights [
    setxy 3 0
    set color white
    set domain all-positions
    set shape "chess knight"
    set step-performed? false
    set possible-steps []
  ]
   create-figures w-bishops [
    setxy 4 0
    set color white
    set domain w-bishops-positions
    set shape "chess bishop"
    set step-performed? false
    set possible-steps []
  ]
   create-figures b-bishops [
    setxy 5 0
    set color white
    set domain b-bishops-positions
    set shape "chess bishop"
    set step-performed? false
    set possible-steps []
  ]


  ask figures [
    create-edges-with other figures
  ]
  ask edges [
    hide-link
    set weight 1
  ]
  ask figures [
    set label who
  ]

end

to-report w-bishops-positions
  report filter [a -> ((first a) mod 2) != ((last a) mod 2)] all-positions
end

to-report b-bishops-positions
  report filter [a -> ((first a) mod 2) = ((last a) mod 2)] all-positions
end

to set-positions
  set all-positions []
  set x-positions n-values max-x [[i] -> (i + 1)]
  set y-positions n-values max-y [[i] -> (i + 1)]

  foreach x-positions [
    [i] ->
    foreach y-positions [
      [j] ->
      set all-positions fput list (i) (j) all-positions
    ]
  ]
end

to draw-board
  foreach all-positions [
    [i] ->
    let x item 0 i
    let y item 1 i
    ask patch x y [
      ifelse (((x mod 2) = 0) xor ((y mod 2) = 0)) [
        set pcolor 8
      ][
      set pcolor brown
      ]
    ]
  ]

end

to assign-figures
  ask figures [
    let assignment one-of domain
    while [count turtles-on patch (item 0 assignment) (item 1 assignment) >= 1]
    [
       set assignment one-of domain
    ]
    move-to-cell assignment
  ]
end

to move-to-cell [a]
  setxy (item 0 a) (item 1 a)

end

; ABT

; визначення необхідних для алгоритму АБТ структур і змінних
to setup-abt
  ; призначаємо випадково кольори, визначаємо множини сусідів
  assign-figures
  ask figures [
    set message-queue []
    set lower-naybors []
    set local-view table:make
    set naybors sort link-neighbors
    let i who
    ; вибірка сусідів з нижчим пріоритетом
    foreach naybors [
      [a] ->
      let w ([who] of a)
      if (w > i) [
        set lower-naybors fput a lower-naybors
      ]
    ]
    ; встановлення початкових обмежень
    set no-goods []
    foreach naybors [
      [a] ->
      let w ([who] of a)
      set no-goods sentence no-goods map [
        [b] ->
        normalize-nogood list (list who (get-possible-steps b shape)) (list w (get-possible-steps b ([shape] of a)))
      ] domain
    ]
    update-possible-steps
   ]
  ; розсилання повідомлень ок? зі своїм кольором сусідам з нижчим пріоритетом
    ask figures [send-out-new-value]
end

to go-abt
  tick
  let important-turtles turtles with [not empty? message-queue]
  ifelse (count important-turtles > 0) [
    ask important-turtles [handle-message]
  ][
    ifelse (not constraint-violations?)[
      show "SOLUTION FOUND"
    ][
      show "NO MORE MESSAGES. NO SOLUTION"
    ]
    stop
  ]
end

to-report violated-links
  report links with [
    member? (list ([xcor] of end2) ([ycor] of end2)) ([possible-steps] of end1) or
    member? (list ([xcor] of end1) ([ycor] of end1)) ([possible-steps] of end2)
  ]
end

to-report constraint-violations?
  report any? violated-links
end

to-report normalize-nogood [nogood]
  ;; Сортування обмежень для виключення дублікатів
  report sort-by [ [a b] -> (first a) < (first b) ] nogood
end

to send-out-new-value
  ;; Надсилання ок? сусідам з нижчим пріоритетом
  let my-message list "ok" (list who possible-steps)
  let i who
  foreach lower-naybors [ [a] ->
    ask a [set message-queue (lput my-message message-queue)]
  ]
end

to handle-message
 ; обробка повідомлень агентами

  if not empty? message-queue [
    let message first message-queue
    set message-queue but-first message-queue
    let message-type first message
    let message-value last message

    ifelse message-type = "ok" [
      let someone first message-value
      let val last message-value
      handle-ok someone val
    ][
      ifelse message-type = "nogood" [
        handle-nogood message-value
      ][
        handle-add-neighbor message-value
      ]
    ]
  ]
end

; обробка повідомлень типу ок?
to handle-ok [someone val]
  show "handle ok"
  table:put local-view someone val
  check-local-view
end

; обробка повідомлень типу nogood
to handle-nogood [nogood]
 if(not member? nogood no-goods) [
    set no-goods fput nogood no-goods
    ;; for each new neighbor
    foreach (filter [
      [a] ->
      not member? (figure first a) naybors ] nogood) [
      [b] ->
      let new-naybor turtle (first b)
      set naybors fput new-naybor naybors
      table:put local-view (first b) (last b)
      let message (list "new-neighbor" who)
      ask new-naybor [
        set message-queue lput message message-queue
      ]
    ]
    foreach naybors [
      [a] ->
      let w ([who] of a)
      if (w > who) and (not member? a lower-naybors) [

        set lower-naybors fput a lower-naybors
      ]
      ]

    check-local-view
  ]

end

; обробка повідомлень додати нового сусіда
to handle-add-neighbor [someone]
 if (not (member? (turtle someone) naybors)) [
    set naybors fput turtle someone naybors
    foreach naybors [
      [a] ->
      let w ([who] of a)
      if (w > who) [
         if (w > who) and (not member? a lower-naybors) [
        set lower-naybors fput a lower-naybors
         ]
      ]
    ]
   let message (list "ok" (list who (list xcor ycor)))
    ask figure someone [
      set message-queue lput message message-queue
    ]
  ]

end

; перевірка сумісності свого кольору множині поточних nogoods
to check-local-view
  if not can-i-be? (list xcor ycor) [

     let try-these filter [ [a] ->
      not (a = (list xcor ycor)) ] domain
    let can-be-something-else false
    while [not empty? try-these] [
      show "try-these"
      show try-these
      let try-this first try-these
      set try-these but-first try-these
      if can-i-be? try-this [
        set try-these [] ;; break loop
        move-to-cell try-this
        update-possible-steps
        set can-be-something-else true
        send-out-new-value

      ]
    ]
    if not can-be-something-else [
      backtrack

    ]
  ]

end

to-report can-i-be? [val]
  table:put local-view who (get-possible-steps val shape)
  foreach no-goods [
    [a] ->
    if (violates? local-view a) [
      table:remove local-view who
      report false
    ]
  ]
  table:remove local-view who
  report true
end

;перевірка порушень
to-report violates? [assignments constraint]
  foreach constraint [
    [a] ->
    ;if not (table:has-key? assignments (first a) and (table:get assignments first a) = (last a)) [report false]
    if not table:has-key? assignments (first a) [report false]
    let steps-1 (table:get assignments (first a))
    let steps-2 (last a)
    if (member? (last steps-1) steps-2) or (member? (last steps-2) steps-1) [report false]
  ]
  report true
end

; процедура відкату
to backtrack
  let no-good normalize-nogood find-new-nogood
  ifelse(not member? no-good no-goods) [
    if ([] = no-good) [
      show "EMPTY NO-GOOD FOUND - NO SOLUTION"
      stop
    ]
    set no-goods fput no-good no-goods

    let index []

    foreach no-good [[a] ->
      set index fput (first a) index]

    ask (figure max index) [
      set message-queue lput (list "nogood" no-good) message-queue
    ]
  ] [ show "NOGOOD"]

end

to-report find-new-nogood
  report table:to-list local-view
end

to update-possible-steps
  set possible-steps get-possible-steps (list xcor ycor) shape
end

to-report get-possible-steps [pos sh]
  let x (first pos)
  let y (last pos)
  let steps []
  set steps fput (list x y) steps
  if sh = "chess bishop" [
    let i 1
    while [(y + i <= max-y) and (x + i <= max-x)] [
      set steps fput (list (x + i) (y + i)) steps
      set i i + 1
    ]
    set i 1
    while [(y - i > 0) and (x - i > 0)] [
      set steps fput (list (x - i) (y - i)) steps
      set i i + 1
    ]
    set i 1
    while [(y + i <= max-y) and (x - i > 0)] [
      set steps fput (list (x - i) (y + i)) steps
      set i i + 1
    ]
    set i 1
    while [(y - i > 0) and (x + i <= max-x)] [
      set steps fput (list (x + i) (y - i)) steps
      set i i + 1
    ]
  ]
  if sh = "chess knight" [
    if y + 2 <= max-y [
      if x < max-x [
        set steps fput (list (x + 1) (y + 2)) steps
      ]
      if x > 1 [
        set steps fput (list (x - 1) (y + 2)) steps
      ]
    ]
    if y - 2 >= 1 [
      if x < max-x [
        set steps fput (list (x + 1) (y - 2)) steps
      ]
      if x > 1 [
        set steps fput (list (x - 1) (y - 2)) steps
      ]
    ]
    if x + 2 <= max-x [
      if y < max-y [
        set steps fput (list (x + 2) (y + 1)) steps
      ]
      if y > 1 [
        set steps fput (list (x + 2) (y - 1)) steps
      ]
    ]
    if x - 2 >= 1 [
      if y < max-y [
        set steps fput (list (x - 2) (y + 1)) steps
      ]
      if y > 1 [
        set steps fput (list (x - 2) (y - 1)) steps
      ]
    ]
  ]
  report steps
end

to set-possible-steps [pos sh]
  set possible-steps get-possible-steps pos sh
end
@#$#@#$#@
GRAPHICS-WINDOW
232
10
600
379
-1
-1
30.0
1
10
1
1
1
0
1
1
1
0
11
0
11
0
0
1
ticks
30.0

BUTTON
78
127
144
160
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
52
270
175
303
NIL
assign-figures
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
36
171
208
204
max-x
max-x
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
39
222
211
255
max-y
max-y
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
716
74
888
107
queens
queens
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
720
136
892
169
kings
kings
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
715
185
887
218
rooks
rooks
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
717
236
889
269
knights
knights
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
720
288
892
321
b-bishops
b-bishops
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
724
340
896
373
w-bishops
w-bishops
0
10
0.0
1
1
NIL
HORIZONTAL

BUTTON
75
385
161
418
NIL
setup-abt
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
81
464
150
497
NIL
go-abt
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
81
521
150
554
NIL
go-abt
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

chess bishop
false
0
Circle -7500403 true true 135 35 30
Circle -16777216 false false 135 35 30
Rectangle -7500403 true true 90 255 210 300
Line -16777216 false 75 255 225 255
Rectangle -16777216 false false 90 255 210 300
Polygon -7500403 true true 105 255 120 165 180 165 195 255
Polygon -16777216 false false 105 255 120 165 180 165 195 255
Rectangle -7500403 true true 105 165 195 150
Rectangle -16777216 false false 105 150 195 165
Line -16777216 false 137 59 162 59
Polygon -7500403 true true 135 60 120 75 120 105 120 120 105 120 105 90 90 105 90 120 90 135 105 150 195 150 210 135 210 120 210 105 195 90 165 60
Polygon -16777216 false false 135 60 120 75 120 120 105 120 105 90 90 105 90 135 105 150 195 150 210 135 210 105 165 60

chess king
false
0
Polygon -7500403 true true 105 255 120 90 180 90 195 255
Polygon -16777216 false false 105 255 120 90 180 90 195 255
Polygon -7500403 true true 120 85 105 40 195 40 180 85
Polygon -16777216 false false 119 85 104 40 194 40 179 85
Rectangle -7500403 true true 105 105 195 75
Rectangle -16777216 false false 105 75 195 105
Rectangle -7500403 true true 90 255 210 300
Line -16777216 false 75 255 225 255
Rectangle -16777216 false false 90 255 210 300
Rectangle -7500403 true true 165 23 134 13
Rectangle -7500403 true true 144 0 154 44
Polygon -16777216 false false 153 0 144 0 144 13 133 13 133 22 144 22 144 41 154 41 154 22 165 22 165 12 153 12

chess knight
false
0
Line -16777216 false 75 255 225 255
Polygon -7500403 true true 90 255 60 255 60 225 75 180 75 165 60 135 45 90 60 75 60 45 90 30 120 30 135 45 240 60 255 75 255 90 255 105 240 120 225 105 180 120 210 150 225 195 225 210 210 255
Polygon -16777216 false false 210 255 60 255 60 225 75 180 75 165 60 135 45 90 60 75 60 45 90 30 120 30 135 45 240 60 255 75 255 90 255 105 240 120 225 105 180 120 210 150 225 195 225 210
Line -16777216 false 255 90 240 90
Circle -16777216 true false 134 63 24
Line -16777216 false 103 34 108 45
Line -16777216 false 80 41 88 49
Line -16777216 false 61 53 70 58
Line -16777216 false 64 75 79 75
Line -16777216 false 53 100 67 98
Line -16777216 false 63 126 69 123
Line -16777216 false 71 148 77 145
Rectangle -7500403 true true 90 255 210 300
Rectangle -16777216 false false 90 255 210 300

chess pawn
false
0
Circle -7500403 true true 105 65 90
Circle -16777216 false false 105 65 90
Rectangle -7500403 true true 90 255 210 300
Line -16777216 false 75 255 225 255
Rectangle -16777216 false false 90 255 210 300
Polygon -7500403 true true 105 255 120 165 180 165 195 255
Polygon -16777216 false false 105 255 120 165 180 165 195 255
Rectangle -7500403 true true 105 165 195 150
Rectangle -16777216 false false 105 150 195 165

chess queen
false
0
Circle -7500403 true true 140 11 20
Circle -16777216 false false 139 11 20
Circle -7500403 true true 120 22 60
Circle -16777216 false false 119 20 60
Rectangle -7500403 true true 90 255 210 300
Line -16777216 false 75 255 225 255
Rectangle -16777216 false false 90 255 210 300
Polygon -7500403 true true 105 255 120 90 180 90 195 255
Polygon -16777216 false false 105 255 120 90 180 90 195 255
Rectangle -7500403 true true 105 105 195 75
Rectangle -16777216 false false 105 75 195 105
Polygon -7500403 true true 120 75 105 45 195 45 180 75
Polygon -16777216 false false 120 75 105 45 195 45 180 75
Circle -7500403 true true 180 35 20
Circle -16777216 false false 180 35 20
Circle -7500403 true true 140 35 20
Circle -16777216 false false 140 35 20
Circle -7500403 true true 100 35 20
Circle -16777216 false false 99 35 20
Line -16777216 false 105 90 195 90

chess rook
false
0
Rectangle -7500403 true true 90 255 210 300
Line -16777216 false 75 255 225 255
Rectangle -16777216 false false 90 255 210 300
Polygon -7500403 true true 90 255 105 105 195 105 210 255
Polygon -16777216 false false 90 255 105 105 195 105 210 255
Rectangle -7500403 true true 75 90 120 60
Rectangle -7500403 true true 75 84 225 105
Rectangle -7500403 true true 135 90 165 60
Rectangle -7500403 true true 180 90 225 60
Polygon -16777216 false false 90 105 75 105 75 60 120 60 120 84 135 84 135 60 165 60 165 84 179 84 180 60 225 60 225 105

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
