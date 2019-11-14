__includes ["MCTS.nls"]

extensions [matrix]

; Piezas del juego
breed [pieces piece]

globals[
  Jugador
  Max_interations
  played?
]

; Patches: celdas del tablero
;     0   1   2   3   4   5   6
;     7   8   9  10  11  12  13
;    14  15  16  17  18  19  20
;    21  22  23  24  25  26  27
;    28  29  30  31  32  33  34
;    35  36  37  38  39  40  41
;    42  43  44  45  46  47  48


; Crear el mundo con  7x7 patches and dibujar el tablero
patches-own[
  value    ;para almacenar (0/1/2)  // 0 --> celda vacía //1 --> pieza jugador 1 (azul) 1// 2 --> pieza jugador 2 (rojo)
]

; state: [content player]
;    El content es una lista de listas que contiene el valor de las celdas del tablero
;    In every cell we have:
;       0 : si la celda esta vacia
;      value : si hay una pieza del jugador1 será 1 y su hay una pieza del jugador2 será un 2


; rules: [ oldposx oldposy newposx newposy]
;      donde (oldposx oldposy) son los coordenadas en la matrix y (newposx newposy) su destino




;; ------------------------ Monte Carlo ---------------------------------------
; Conseguir el contenido del estado
to-report MCTS:get-content [s]
  report first s
end

; Conseguir el jugador que genera el estado
to-report MCTS:get-playerJustMoved [s]
  report last s
end

; Crear un estado a partir de un contenido y un jugador
to-report MCTS:create-state [c p]
  report (list c p)
end


;Conseguir las reglas aplicables al estado
to-report MCTS:get-rules [s]
  let lista (list)
  let matrix first s
  let jug last s
  let jugador-que-va-a-jugar (3 - jug)
  let filas 0
  let columnas 0

  foreach matrix [
    i -> foreach i [
      j ->
      if (j = jugador-que-va-a-jugar) ; por todas las casillas del jugador que va a jugar
      [
        foreach combinaciones-posibles (list filas columnas) possible-movements matrix filas columnas [p -> set lista lput p lista]  ; anadir los movimientos possible donde un movimiento es [px py nx ny]
      ]
      set columnas (columnas + 1)] set columnas 0 set filas (filas + 1)
  ]

  report lista
end

;Aplicar la regla r al estado s
to-report MCTS:apply [r s]
  let matrix first s
  let jug last s

  let content cambiar-estado r matrix (3 - jug)

  report MCTS:create-state content (3 - jug)
end

;Mover el resultado del ultimo estado al estado actual
to-report MCTS:get-result [s p]
  if final-state s = 1[
    report 1
  ]
  if final-state s = 2[
    report 0
  ]
  report false
end

; ------------------------ Interface ---------------------------------------
; Proceso de comienzo. Prepara el tablero.
to setup
  ca
  ; parte grafica
  ask patches [
    set pcolor ifelse-value ((pxcor + pycor) mod 2 = 0) [white] [8999]
    set value 0
  ]
  set Jugador 1 ; Jugador blue (humano)
  change-patch-color 0 0 1
  change-patch-color 6 6 1
  change-patch-color 0 6 2
  change-patch-color 6 0 2
  set played? false
end



 ; ------------------------ Jugador humano vs Jugador humano ---------------------------------------


to play-con-humano
  let pieza nobody
  let oldpos nobody
  let newpos nobody
  let casillas-disponibles 0

  if mouse-down? [  ;si jugador pulsa en un patch
    if any? pieces-on patch mouse-xcor mouse-ycor and [value] of patch mouse-xcor mouse-ycor = Jugador [  ; y si donde ha pinchado hay una pieza y cuyo valor del patch donde esta la pieza es igual al jugador

      set pieza one-of pieces-on patch mouse-xcor mouse-ycor ;asignamos a pieza la pieza que hay en el patch
      set oldpos patch mouse-xcor mouse-ycor  ;asignamos a la variable oldpos las coordenadas donde el jugador a pinchado
      set casillas-disponibles possible-movements board-to-state (6 - [pycor] of oldpos) [pxcor] of oldpos   ;almacenamos en casillas-disponibles los posibles movimientos que puede hacer con esa ficha

      while[mouse-down?][  ;mientras el raton este pulsado arrastra la pieza con el
        ask pieza [setxy mouse-xcor mouse-ycor]
      ]
      ;una vez ha soltado el jugador la pieza almacenamos las coordenadas del patch donde la haya soltado en newpos
      set newpos patch mouse-xcor mouse-ycor
      ; si podemos cambiar los estados de los patches (duplica o mover), es decir si el jugador ha hecho un movimiento posible
      if cambiar-patches pieza [pxcor] of oldpos [pycor] of oldpos [pxcor] of newpos [pycor] of newpos Jugador casillas-disponibles [
        set Jugador (3 - Jugador)  ; cambiamos de jugador
      ]
    ]

    ; comprobamos si hay un ganador
    if final-state (list (board-to-state) (3 - Jugador))  = 1 [
      user-message "Jugador 1 (azul) ha ganado!!!"
      stop
    ]
    if final-state (list (board-to-state) (3 - Jugador))  = 2 [
      user-message "Jugador 2 (rojo) ha ganado!!!"
      stop
    ]
  ]
end


; ------------------------ Jugador automatico con Monte-Carlo vs Jugador humano ---------------------------------------

to play-con-robot
  let pieza nobody
  let oldpos nobody
  let newpos nobody
  let casillas-disponibles 0
  if mouse-down? [   ;si el jugador pulsa en un patch
    if any? pieces-on patch mouse-xcor mouse-ycor and [value] of patch mouse-xcor mouse-ycor = Jugador [  ; y si donde ha pinchado hay una pieza y cuyo valor del patch donde esta la pieza es igual al jugador
      set pieza one-of pieces-on patch mouse-xcor mouse-ycor  ;asignamos a pieza la pieza que hay en el patch
      set oldpos patch mouse-xcor mouse-ycor  ;asignamos a la variable oldpos las coordenadas del patch donde el jugador a pinchado
      set casillas-disponibles possible-movements board-to-state (6 - [pycor] of oldpos) [pxcor] of oldpos  ;almacenamos en casillas-disponibles los posibles movimientos que puede hacer con esa ficha

      while[mouse-down?][   ;mientras el raton este pulsado arrastra la pieza con el
        ask pieza [setxy mouse-xcor mouse-ycor]
      ]
      ;una vez ha soltado el jugador la pieza almacenamos las coordenadas del patch donde la haya soltado en newpos
      set newpos patch mouse-xcor mouse-ycor
      if cambiar-patches pieza [pxcor] of oldpos [pycor] of oldpos [pxcor] of newpos [pycor] of newpos 1 casillas-disponibles [ ; si podemos cambiar los estados de los patches (duplica o mover), es decir si el jugador ha hecho un movimiento posible
        set played? true  ; el humano ha jugado
      ]
    ]
    ;esperamos que el raton suelte la pieza correctamente
    wait .1
    ; comprobamos que el jugador no ha ganado
    if MCTS:get-result (list (board-to-state) 1) 1 = 1 [
      user-message "You win!!!"
      stop
    ]
    if MCTS:get-result (list (board-to-state) 1) 2 = 0 [
      user-message "I win!!!"
      stop
    ]
  ]
  if played? [  ; si el humano ha jugado
    let m MCTS:UCT (list (board-to-state) 1) Max_iterations  ; aplicamos monte-carlo para saber el proximo movimento, la forma de m sera [olpx olpy newx newy]

    ; tenemos que recuperar las coordenadas de las posiciones (anterior y posterior)  y cambiarlas a coordenadas de patches (las coordenadas en el estado no son las mismas que en los patches)
    let oldposy ( 6 - (item 0 m))
    let oldposx item 1 m
    let newposy ( 6 - (item 2 m))
    let newposx item 3 m

    ; cambiamos el tablero grafico
    ifelse ((distancia  oldposx oldposy newposx newposy) = 1 )[ ;creamos la pieza en la antigua posicion porque en la nueva posicion movemos la pieza (arrastrandola)
      change-patch-color newposx newposy 2
    ]
    [ ; si la distancia es 2 cambiamos el valor de los patches, es decir el patch con oldpos le ponemos el valor a 0
      ask patches with [pxcor = oldposx and pycor = oldposy ] [set value 0]
      ask pieces  with [pxcor = oldposx and pycor = oldposy ] [
        move-to patch newposx newposy ;aqui movemos la pieza a la nueva posicion(arrastrandola)
      ];cambiamos el valor del patch de la nueva posicion a 2
      ask patches with [pxcor = newposx and pycor = newposy ] [set value 2]
    ]
    cambiar-colores-alrededor newposx newposy 2  ;cambiamos el color de las piezas que estan a distancia 1 de newpos
    ; comprobamos si la maquina ha ganado
    if MCTS:get-result (list (board-to-state) 2) 2 = 0 [
      user-message "I win!!!"
      stop
    ]
    if MCTS:get-result (list (board-to-state) 2) 1 = 1 [
      user-message "You win!!!"
      stop
    ]
    set played? false
  ]
end



;;  ------------------------ Funciones auxiliares ---------------------------------------

;Crea una lista de listas donde combina la posicion que va a mover con todos sus posibles movientos
;L1[0 0] L2[ [1 1] [2 2] ] -- > [ [0 0 1 1] [0 0 2 2] ]
to-report combinaciones-posibles [L1 L2]
  if empty? L1 or empty? L2 [report []]
  report  map [y ->  sentence L1 y] L2
end

;funcion que cambia los colores de las piezas que han sido convertidas al color del jugador que ha movido
to cambiar-colores-alrededor [px py jug]
  let x px
  let y py
  let casillas-alrededor (list (list  (x - 1)  y) (list  (x - 2 )  y) (list  (x  + 1)  y) (list  (x  + 2)  y) (list  (x  - 1)  (y + 1) )
    (list  (x  - 1)  (y - 1) ) (list  (x  + 1)  (y - 1) ) (list  (x  + 1) (y + 1))(list  (x  + 2)  (y + 2)) (list  (x  + 2)  (y - 2)) (list  (x  - 2)  (y + 2)) (list  (x  - 2)  (y - 2))
    (list  x  (y - 2)) (list  x  (y - 1)) (list  x  (y + 1)) (list  x  (y + 2)))

  set casillas-alrededor filter[s -> first s >= 0 and first s < 7 and last s >= 0 and last s < 7 ]casillas-alrededor

  foreach casillas-alrededor [c -> if (distancia first c last c px py = 1)[ ; por todas las casillas a distancia 1 de newpos
    ask pieces with [pxcor = first c and pycor = last c ] [
      set shape "circle"
      ifelse jug = 1 [  ; cambiar color y value
        set color blue
        set value 1
      ]
      [
        set color red
        set value 2
      ]
    ]
    ]
  ]
end

; crea pieza en pos px py a la color del jugador jug
to change-patch-color [px py jug]
  ask patches  with [pxcor = px and pycor = py] [
    sprout-pieces 1 [
      set shape "circle"
      ifelse jug = 1 [
        set color blue
      ]
      [
        set color red
      ]
      set size 0.9
    ]
    set value jug
  ]
end

; report una matrix con una la linea a la posicion pos por la linea linea en la matrix
to-report sustituir-ficha [linea pos matrix]
  let new-matrix (list)
  let count-linea 0
  foreach matrix [
    l ->
    ifelse (pos != count-linea)
    [set new-matrix lput l new-matrix]
    [set new-matrix lput linea new-matrix]
    set count-linea (count-linea + 1)
  ]
  report new-matrix
end
; report el estado de la matrix una vez aplicada la regla
to-report cambiar-estado [r matrix jug]
  let px item 0 r
  let py item 1 r
  let nx item 2 r
  let ny item 3 r

  let dist distancia px py nx ny
  let new-matrix sustituir-ficha (replace-item ny (item nx matrix) jug) nx matrix ; poner la nueva pieza en su nueva posicion

  if dist = 2
  [  ; si distancia = 2 ,solo movemos la ficha a la nueva casilla y por lo tanto dejamos vacia la casilla anterior
    set new-matrix sustituir-ficha (replace-item py (item px matrix) 0) px new-matrix
  ]
  report new-matrix
end

; esta funcion report true si es posible  mover una pieza de  una posicion a otra y hacer al mismo tiempo los cambios de los patches necesarios si el movimiento es posible
to-report cambiar-patches [pieza oldposx oldposy newposx newposy jug casillas-disponibles]
  let mover true

  ask pieza[
    ifelse not any? other pieces-on patch newposx newposy and movement-valid? casillas-disponibles (6 - newposy) newposx [
      move-to patch newposx newposy   ; movemos la pieza a su nueva posicion
      set value Jugador               ; cambiamos el valor del patch por el del jugador

      ifelse (distancia oldposx oldposy newposx newposy = 1 )[  ; si la distancia de la nueva posición es igual a 2, solo mueve la pieza. Si es igual a 1 la duplica.
        change-patch-color oldposx oldposy Jugador   ;creamos la pieza en la antigua posicion porque a la nueva ya la hemos movido
      ]
      [; si la distancia es 2 cambiamos el valor de los patches, es decir el patch con oldpos le ponemos el valor a 0
        ask patches  with [pxcor = oldposx and pycor = oldposy ] [set value 0]
      ]
      cambiar-colores-alrededor newposx newposy Jugador  ;cambiamos el color de las piezas que estan a distancia 1 de newpos
    ]
    [; si el humano no ha hecho un movimiento posible, movemos la ficha a la posicion inicial
      move-to patch oldposx oldposy  ;movemos la ficha a la posicion inicial
      set mover false  ; no hemos cambiado nado porque el movimiento queriado ne es posible
    ]
  ]
  report mover
end
;report los posibles movimientos en la matrix a partir de la posicion (px py)
to-report possible-movements [matrix px py]
  let x px
  let y py
  let casillas-disponibles (list (list  (x - 1)  y) (list  (x - 2 )  y) (list  (x  + 1)  y) (list  (x  + 2)  y) (list  (x  - 1)  (y + 1) )
    (list  (x  - 1)  (y - 1) ) (list  (x  + 1)  (y - 1) ) (list  (x  + 1) (y + 1))(list  (x  + 2)  (y + 2)) (list  (x  + 2)  (y - 2)) (list  (x  - 2)  (y + 2)) (list  (x  - 2)  (y - 2))
    (list  x  (y - 2)) (list  x  (y - 1)) (list  x  (y + 1)) (list  x  (y + 2)))

  set casillas-disponibles filter[s -> first s >= 0 and first s < 7 and last s >= 0 and last s < 7 ]casillas-disponibles

  let casillas-vacias (list)
  foreach casillas-disponibles [s ->
    if ((item (last s) (item (first s) matrix)) = 0) [
      set casillas-vacias lput s casillas-vacias
    ]
  ]

  report casillas-vacias
end

; report si un movimientos es valido o no
to-report movement-valid? [casillas-disponibles newposx newposy]
  let p (list (newposx) (newposy))
  foreach casillas-disponibles [c -> if (p = c)[report true]]
  report false

end

; report la distancia entre 2 puntos
to-report distancia [posx posy newposx newposy]
  let res max(list (abs(newposx - posx)) (abs(newposy - posy)))
  report  res
end

; report 1 si el jugador 1 ha ganado, 2 si el jugador 1 ha ganado y false si ninguno ha ganado y podemos seguir
to-report final-state [estado]
  let matrix first estado
  let jug last estado
  let jugador-que-va-a-jugar 1
  let fichas1 0
  let fichas2 0

  if jug = 1[set jugador-que-va-a-jugar 2]

  let filas 0
  let columnas 0
  ;Comprobamos si el jugador tiene algun movimiento posible , si es cierto no es un estado final(report false)  pero si no tiene movimiento comprobamos que tipo de estado final es; tablero completo o jugador bloqueado
  foreach matrix [i -> foreach i [j -> if (j = jugador-que-va-a-jugar) [if (not empty? possible-movements matrix filas columnas) [report false]] set columnas (columnas + 1)] set columnas 0 set filas (filas + 1)]

  foreach matrix [i -> foreach i [j -> if j = 1 [set fichas1 ( fichas1 + 1 )] if ( j = 2)[set fichas2 (fichas2 + 1 )]]] ; contamos las fichas de los jugadores
  ifelse (fichas1 + fichas2 = 49)[;si el tablero es completo
    ifelse (fichas1 > fichas2)[
      report 1 ; jugador 1 es el ganador
    ]
    [
      report 2  ; jugador 2 es el ganador
    ]
  ]
  ;si el tablero no esta completo
  [
    report jug ; report el jugador ganador
  ]

end

;Funcion auxiliar que construye la representacion de los patches en una lista de listas
to-report board-to-state
  let l0 map [x -> [value] of x] (sort patches with [pycor = 6])
  let l1 map [x -> [value] of x] (sort patches with [pycor = 5])
  let l2 map [x -> [value] of x] (sort patches with [pycor = 4])
  let l3 map [x -> [value] of x] (sort patches with [pycor = 3])
  let l4 map [x -> [value] of x] (sort patches with [pycor = 2])
  let l5 map [x -> [value] of x] (sort patches with [pycor = 1])
  let l6 map [x -> [value] of x] (sort patches with [pycor = 0])
  report (list l0 l1 l2 l3 l4 l5 l6)
end
@#$#@#$#@
GRAPHICS-WINDOW
202
10
510
319
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
0
6
0
0
1
ticks
30.0

BUTTON
11
24
142
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
8
91
143
124
1 jugador
play-con-robot\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
549
22
721
55
Max_iterations
Max_iterations
0
3000
50.0
50
1
NIL
HORIZONTAL

BUTTON
9
155
141
188
2 jugadores
play-con-humano
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
549
71
721
116
Action..
(word \"Thinking: \" (count MCTSnodes) \" ...\")
17
1
11

@#$#@#$#@
##  ¿QUÉ ES? - WHAT IS IT?

Una demostración del algoritmo Monte Carlo Tree Search aplicado al juego  **Asimilación** en el que un humano juega contra la máquina (**1 Jugador**). Y por otro lado un modo de juego para 2 humanos (**2 Jugadores**). 

La siguiente información ha sido extraida de [Asimilación](https://brainking.com/es/GameRules?tp=89)

## ORIGEN E HISTORIA - ORIGIN AND HISTORY

**Asimilación** (un clon del Ataxx, también conocido como SlimeWars y Frog Cloning) es un juego de tablero que apareció por vez primera en 1990 en versión video juego por la Corporación Leland. Según fuentes de internet, el juego fue ideado por Dave Crummack y Craig Galley en 1988 y se denominó Infección.

## DESCRIPCIÓN - DESCRIPTION

Este es un juego el cual se juega en un tablero cuadrado dividido en 49 cuadrados(7X7) en el que 2 jugadores se enfrentan entre sí. Se utilizan 2 tipos de piezas, un tipo (piezas de color **azul**) pertenecientes al jugador nº 1 y el otro tipo (piezas de color **rojo**) pertenecientes al jugador 2.	 


## OBJETIVO - GOAL
El objetivo del juego para cada jugador es que  el adversario no pueda realizar ningun movimiento legal.Cuando esto ocurre, el jugador que no puede realizar un movimiento es el perdedor. 

## DESARROLLO - DEVELOPMENT
Cada jugador mueve una ficha por turno. Una ficha puede ser movida una o dos casillas en cualquier dirección horizontal, vertical o diagonalmente a una casilla vacía. Hay diferencias en cuanto al movimiento de la ficha a una o dos casillas de distancia :

1. Movimiento de **una** casilla: La ficha se duplica en la nueva casilla.

2. Movimiento de **dos** casillas: La ficha se mueve a la nueva casilla.

Al comienzo del juego cada jugador posee dos fichas. Cada una de ellas en los extremos de una diagonal del tablero, diferente al del oponente.


## FINAL

Si las 49 casillas del tablero han sido completadas con fichas de los jugadores, el ganador será el jugador con mas fichas sobre él.


## CREDITOS Y REFERENCIAS - CREDITS AND REFERENCES

Model created by: **Juliette Philibert, Sergio Pérez López, Enrique Piedra Venegas**

La biblioteca usada para el algoritmo de Monte Carlo: MCTS.nls 

MCTS.nls creada por:[Fernando Sancho Caparrini](https://www.cs.us.es/~fsancho/)
La biblioteca se puede encontrar en: [IA Github Repository](https://github.com/fsancho/IA/tree/master/05.%20Adversarial%20Search)
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
