__includes ["MCTS.nls"]

; Pieces of the game
breed [pieces piece]

pieces-own [
  id ; will mantain the type of the piece (it identifies the piece)
]
globals[
  Jugador
  Max_interations
  played?
]

; Patches: cells of the board
;     0   1   2   3   4   5   6
;     7   8   9  10  11  12  13
;    14  15  16  17  18  19  20
;    21  22  23  24  25  26  27
;    28  29  30  31  32  33  34
;    35  36  37  38  39  40  41
;    42  43  44  45  46  47  48


; Create  the world with 7x7 patches and draws the board
patches-own[
  value    ;to store the piece (0/1/2) of this place // 0 --> cell empty //1 --> piece player 1// 2 --> piece player 2 (will mantain the id of the piece in it)
]

; state: [content player]
;    The content is a list with the contents of the cells of the board
;    In every cell we have:
;       0 : if there is no piece in it
;      id : if the piece id is in it


; rules: [ pie pos]
;      piece: a piece on the board of the player who is going to play
;      pos  : a cell available (no piece in it). From 0 to 48

;; ------------------------ Monte Carlo ---------------------------------------
; Get the content of the state
to-report MCTS:get-content [s]
  report first s
end

; Get the player that generates the state
to-report MCTS:get-playerJustMoved [s]
  report last s
end

; Create a state from the content and player
to-report MCTS:create-state [c p]
  report (list c p)
end

to-report times [L1 L2]
  if empty? L1 or empty? L2 [report []]
  report  map [y ->  sentence L1 y] L2
end

; Get the rules applicable to the state
to-report MCTS:get-rules [s]
  let lista (list)
  ; ask patches with [value = Jugador][  ; por todos los piezas del jugador que esta jugando
  ;  set lista lput (list (list patch pxcor pycor) (list filter [c -> [value] of patch first c last c = 0] possible-movements patch pxcor pycor )) lista
  ;]
  ask patches with [value = Jugador][
    foreach times (list pxcor pycor) filter [c -> [value] of patch first c last c = 0] possible-movements patch pxcor pycor

    [ elt -> set lista lput elt lista
    ]
  ]
  report lista
end

; Apply the rule r to the state s
to-report MCTS:apply [r s]
  let posx first r
  let posy item 1 r
  let newposx item 2 r
  let newposy item 3 r
  let dist distancia patch posx posy patch newposx newposy

  ifelse dist = 1
  [ ; si la distancia de la nueva posición es mayor que 1 solo mueve la pieza si es 1 la duplica.
    ask patches  with [pxcor = newposx and pycor = newposy ] [
      sprout-pieces 1 [
        set shape "circle"
        set value Jugador
        ifelse Jugador = 1[
          set color blue
          set Jugador 2
        ][
          set color red
          set Jugador 1
        ]
        set size 0.9
      ]
    ]
  ]
  [
    ask patches  with [pxcor = posx and pycor = posy ] [set value 0]
    ask patches  with [pxcor = newposx and pycor = newposy ] [set value Jugador]
    ifelse Jugador = 1[
      set Jugador 2
    ][
      set Jugador 1
    ]
  ]
  let casillas-alrededor possible-movements patch newposx newposy
  foreach casillas-alrededor [c -> if (distancia (patch first c last c) patch newposx newposy = 1)[ ; para todas las casillas a distancia 1 de newpos
    ask turtles with [pxcor = first c and pycor = last c ] [
      ifelse Jugador = 1 [  ; cambiar color y value
        set color red
        set value 2
      ]
      [
        set color blue
        set value 1
      ]
    ]
    ]
  ]
  report board-to-state
end

; Move the result from the last state to the current one
to-report MCTS:get-result [s p]
  if final-state board-to-state = 1[
    report 1
  ]
  if final-state board-to-state = 2[
    report 0
  ]
  if final-state board-to-state = 0[
    report 0.5
  ]
end









;; ------------------------ 2 Humanos ---------------------------------------










;; ------------------------ Interface ---------------------------------------

; Start procedure. Prepares the board
to setup
  ca
  ask patches [
    set pcolor ifelse-value ((pxcor + pycor) mod 2 = 0) [white] [8999]
    set value 0
  ]
  set Jugador 1 ; Jugador blue
                ;Setup pieces
  ask patches  with [pxcor = 0 and pycor = 0] [
    sprout-pieces 1 [
      set shape "circle"
      set color blue
      set size 0.9
    ]
    set value 1
  ]
  ask patches  with [pxcor = 6 and pycor = 6] [
    sprout-pieces 1 [
      set shape "circle"
      set color blue
      set size 0.9
    ]
    set value 1
  ]
  ask patches  with [pxcor = 0 and pycor = 6] [
    sprout-pieces 1 [
      set shape "circle"
      set color red
      set size 0.9
    ]
    set value 2
  ]
  ask patches  with [pxcor = 6 and pycor = 0] [
    sprout-pieces 1 [
      set shape "circle"
      set color red
      set size 0.9
    ]
    set value 2
  ]
  set played? true

end

to play

  let pieza nobody
  let pos nobody
  let newpos nobody
  let lista 0
  let casillas-disponibles 0
  ; In the cycle, the human starts playing
  ; Let's check if you click on a free piece
  if (final-state board-to-state = 0)[
    if mouse-down? [
      if any? pieces-on patch mouse-xcor mouse-ycor and [value] of patch mouse-xcor mouse-ycor = Jugador [ ; si hay una pieza en el patch que pinchamos y pertenece al jugador que le toca mover
        set pieza one-of pieces-on patch mouse-xcor mouse-ycor
        set pos patch mouse-xcor mouse-ycor
        set casillas-disponibles possible-movements  pos
        while[mouse-down?][
          ask pieza [setxy mouse-xcor mouse-ycor]
        ]
        set newpos patch mouse-xcor mouse-ycor
        ask pieza[
          ifelse (not any? other pieces-on patch mouse-xcor mouse-ycor) and (movement-valid? casillas-disponibles newpos) and [value] of patch mouse-xcor mouse-ycor = 0;( comprobar cor de mouse están en la lista de casillas disponibles y si esta basilla y mover la ficha en caso contrario regresarla a su origen)
          [
            move-to patch mouse-xcor mouse-ycor
            set value Jugador
            set newpos patch mouse-xcor mouse-ycor
            ifelse ((distancia  pos newpos) = 1 )[ ; si la distancia de la nueva posición es mayor que 1 solo mueve la pieza si es 1 la duplica.
              ask patches  with [pxcor = [pxcor] of pos and pycor = [pycor] of pos ] [
                sprout-pieces 1 [
                  set shape "circle"
                  set value Jugador
                  ifelse Jugador = 1[
                    set color blue
                    set Jugador 2
                  ][
                    set color red
                    set Jugador 1
                  ]
                  set size 0.9
                ]
              ]
            ][
              ask patches  with [pxcor = [pxcor] of pos and pycor = [pycor] of pos ] [set value 0]
              ask patches  with [pxcor = [pxcor] of newpos and pycor = [pycor] of newpos ] [set value Jugador]
              ifelse Jugador = 1[
                set Jugador 2
              ][
                set Jugador 1
              ]
            ]
            let casillas-alrededor possible-movements newpos
            foreach casillas-alrededor [c -> if (distancia (patch first c last c) newpos = 1)[ ; para todas las casillas a distancia 1 de newpos
              ask turtles with [pxcor = first c and pycor = last c ] [
                ifelse Jugador = 1 [  ; cambiar color y value
                  set color red
                  set value 2
                ]
                [
                  set color blue
                  set value 1
                ]
              ]
              ]

            ]

          ]
          [
            move-to patch [pxcor] of pos  [pycor] of pos
          ]
        ]
      ]
    ]
    stop
  ]
end

to play-con-robot

  let pieza nobody
  let pos nobody
  let newpos nobody
  let lista 0
  let casillas-disponibles 0
  ; In the cycle, the human starts playing
  ; Let's check if you click on a free piece
  if (final-state board-to-state = 0)[
    if mouse-down? [
      if any? pieces-on patch mouse-xcor mouse-ycor and [value] of patch mouse-xcor mouse-ycor = Jugador [ ; si hay una pieza en el patch que pinchamos y pertenece al jugador que le toca mover
        set pieza one-of pieces-on patch mouse-xcor mouse-ycor
        set pos patch mouse-xcor mouse-ycor
        set casillas-disponibles possible-movements  pos
        while[mouse-down?][
          ask pieza [setxy mouse-xcor mouse-ycor]
        ]
        set newpos patch mouse-xcor mouse-ycor
        ask pieza[
          ifelse (not any? other pieces-on patch mouse-xcor mouse-ycor) and (movement-valid? casillas-disponibles newpos) and [value] of patch mouse-xcor mouse-ycor = 0;( comprobar cor de mouse están en la lista de casillas disponibles y si esta basilla y mover la ficha en caso contrario regresarla a su origen)
          [
            move-to patch mouse-xcor mouse-ycor
            set value Jugador
            set newpos patch mouse-xcor mouse-ycor
            ifelse ((distancia  pos newpos) = 1 )[ ; si la distancia de la nueva posición es mayor que 1 solo mueve la pieza si es 1 la duplica.
              ask patches  with [pxcor = [pxcor] of pos and pycor = [pycor] of pos ] [
                sprout-pieces 1 [
                  set shape "circle"
                  set value Jugador
                  ifelse Jugador = 1[
                    set color blue
                    set Jugador 2
                  ][
                    set color red
                    set Jugador 1
                  ]
                  set size 0.9
                ]
              ]
            ][
              ask patches  with [pxcor = [pxcor] of pos and pycor = [pycor] of pos ] [set value 0]
              ask patches  with [pxcor = [pxcor] of newpos and pycor = [pycor] of newpos ] [set value Jugador]
              ifelse Jugador = 1[
                set Jugador 2
              ][
                set Jugador 1
              ]
            ]
            let casillas-alrededor possible-movements newpos
            foreach casillas-alrededor [c -> if (distancia (patch first c last c) newpos = 1)[ ; para todas las casillas a distancia 1 de newpos
              ask turtles with [pxcor = first c and pycor = last c ] [
                ifelse Jugador = 1 [  ; cambiar color y value
                  set color red
                  set value 2
                ]
                [
                  set color blue
                  set value 1
                ]
              ]
              ]

            ]

          ]
          [
            move-to patch [pxcor] of pos  [pycor] of pos
          ]
        ]
      ]
      ; aqui juega la maquina
      wait .1
      let m MCTS:UCT (list (board-to-state) 2) Max_interations
      ;stop
    ]
  ]
  ;stop
end


to-report possible-movements [pos]
  let x [pxcor] of pos
  let y [pycor] of pos
  let casillas-disponibles 0

  set casillas-disponibles (list (list  (x - 1)  y) (list  (x - 2 )  y) (list  (x  + 1)  y) (list  (x  + 2)  y) (list  (x  - 1)  (y + 1) )
    (list  (x  - 1)  (y - 1) ) (list  (x  + 1)  (y - 1) ) (list  (x  + 1) (y + 1))(list  (x  + 2)  (y + 2)) (list  (x  + 2)  (y - 2)) (list  (x  - 2)  (y + 2)) (list  (x  - 2)  (y - 2))
    (list  x  (y - 2)) (list  x  (y - 1)) (list  x  (y + 1)) (list  x  (y + 2)))


  set casillas-disponibles filter[s -> first s >= 0 and first s < 7 and last s >= 0 and last s < 7]casillas-disponibles
  report casillas-disponibles
end

to-report movement-valid? [casillas-disponibles newpos]
  let p (list ([pxcor] of newpos) ([pycor] of newpos))
  foreach casillas-disponibles [c -> if (p = c)[report true]]
  report false

end


to-report distancia [pos newpos]
  let res 0
  set res max(list (abs([pxcor] of newpos - [pxcor] of pos)) (abs([pycor] of newpos - [pycor] of pos)))
  report  res
end

to-report final-state [content]
  let fichas1 0
  let fichas2 0

  ;comprobamos si hay movimientos posibles del jugador que esta jugando  y si hay, seguimos jugando
  ; miramos en cada posicion de la lista del tablero cuyo valor es 0-1-2 , solo en las que pertenecen al jugador que esta jugando y ademas si es vacío el numero de casillas posibles
  ;a las que el jugador se puede mover (posiblle movements) y cuyo valor es vacío(filter(se nos olvidó poner la comprobacion de si es vacía en la funcion y por eso se ha hecho el filter aquí))
  ;foreach posiciones-tablero [i -> if (item i content = Jugador) and ((empty?  filter [s -> [value] of patch (first s) (last s) = 0] possible-movements patch  (i / 7)  (i mod 7)) = false)[ report false]]
  let empty true
  ask patches with [value = Jugador][  ; por todos los piezas del jugador que esta jugando
    if ( empty? filter [s -> [value] of patch first s last s = 0] possible-movements patch pxcor pycor = false ) ; si puede mover unas de sus piezas
    [
      set empty false  ; set empty a false
    ]
  ]
  if empty = false [report 0]  ; si empty es false, es decir que el jugador puede hacer un movimento y que entonces la partida puede seguir

  ;si llegamos aqui es porque no hay movimientos posibles, entonces comprobamos si es porque el tablero esta lleno en cuyo caso procedemos a contar el numero de fichas de cada jugador
  set fichas1 count patches with [value = 1]
  set fichas2 count patches with [value = 2]
  ifelse(fichas1 + fichas2 = 49)[
    ifelse (fichas1 > fichas2)[
      user-message "Jugador-1 ha ganado y Jugador-2 paga un kebab al creador del juego"
      report 2
    ]
    [
      user-message "Jugador-2 ha ganado y Jugador-1 paga 50 cervezas al creador del juego"
      report 1
    ]
  ] ;por ultimo si llegamos aqui es porque no hay movimientos posibles pero el tablero no esta completo, por lo que tenemos que ver qué jugador no puede realizar ningun movimiento ya que será el perdedor.
  [
    user-message   word "Jugador-" word Jugador " ha perdido"
    report Jugador ; que ha perdido
  ]

end

; Auxiliary report to build the representation in list from the patches
to-report board-to-state
  let b map [x -> [value] of x] (sort patches)
  report b
end
to-report coord-to-pos [a b]
   let x 7 * a + b
  report x
end

to-report  pos-to-coord [a]

   let c floor (a / 7)
  let d (a mod 7)

  report sentence d c

end
@#$#@#$#@
GRAPHICS-WINDOW
236
100
544
409
-1
-1
42.857142857142854
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
6
-6
0
0
0
1
ticks
30.0

BUTTON
11
24
101
57
New Game
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
111
24
174
57
Play!
play
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
