globals [payoff-matrix strategy-colors strategies colors indices score-table sum-of-strategies proportions high-pay low-pay]
patches-own [mean-total-payoff strategy neighborhood winner winner-strategy strategy-changes]
 ;First we declare our globals as per the assignment and a few extra globals we'll need later.

to setup ;;This one is basically completely copied from the snippets
  clear-output
  clear-all-plots
  reset-ticks
  clear-patches
  set payoff-matrix (list (list CC-payoff-reward     CD-payoff-sucker    )
                        (list DC-payoff-temptation DD-payoff-punishment))

  set high-pay max (list cc-payoff-reward dc-payoff-temptation cd-payoff-sucker dd-payoff-punishment) ;;These high-pay low-pay variables will be used later when we're visualizing payoffs
  set low-pay min (list cc-payoff-reward dc-payoff-temptation cd-payoff-sucker dd-payoff-punishment)

  set strategy-colors [
    ["always-cooperate"    green  ]
    ["always-defect"       red    ]
    ["play-randomly"       gray   ]
    ["unforgiving"         102    ]
    ["tit-for-tat"         violet ]
    ["pavlov"              brown  ]
  ]
  set strategies    map [ [x] -> item 0 x ] strategy-colors ; strip strategies from strategy-colors
  set colors        map [ [x] -> item 1 x ] strategy-colors ; strip colors from strategy-colors

  set indices       n-values length strategies [ [x] -> x ]

  ask patches [ set neighborhood (patch-set self neighbors) ]
  ask patches [ set neighborhood  patches in-radius 1.5  ]
  ask patches [set strategy-changes 0.5]

  initialise-run
  calculate-score-table
  end

to go ;;This is our main loop, the first part has been taken from the snippets
 ask patches [
    set mean-total-payoff mean [item strategy item ([ strategy ] of myself) score-table] of neighborhood
  ]
  ask patches[
  set winner max-one-of neighborhood [mean-total-payoff] ;first we determine the 'winner' of the neighborhood (inluding the patch itself) note the use of max-one-of because there's a chance of multiple neighbors having the highest payoff
  set winner-strategy [strategy] of winner ;;we save winner-strategy seperately, this is for synchronous updates
    ifelse (strategy = winner-strategy) ;;This is used to visualize strategy changes
    [set strategy-changes (1 * strategy-changes)] ;;If the patch doesn't change strategy the value stays the same
    [set strategy-changes (0.95 * strategy-changes)] ;;If the patch changes strategy the value will become lower (resulting in a darker shade of the color)
  ]
  ask patches[
    set strategy winner-strategy ;;first up we update our patches to the winning strategy

    if Visualization = "Current Strategy" ;;The following lines are different ways of using color to represent the currently selected visualization method.
    [set pcolor item strategy colors]

    if Visualization = "Average Payoff"
    [set pcolor scale-color gray (mean-total-payoff) (low-pay) (high-pay)]

    if Visualization = "Strategy Dynamic"
    [set pcolor scale-color item strategy colors strategy-changes 0 1]
  ]
  tick
  clear-output
  if output-rank-list
  [do-plots] ; it is customary to plot /after/ ticks
end

;;The following bits are mainly taken from the snippets
to initialise-run
 normalize-strategy-ratios
 fill-cells
end

; this routine ensures that start-ratio's sum to one
; it requires start- sliders for all strategies, otherwise this routine won't work
to normalize-strategy-ratios
  set sum-of-strategies sum map [ [s] -> run-result (word "start-" s) ] strategies
  if abs(sum-of-strategies - 1) <= 0.001 [ stop ] ; already normalized
  foreach strategies [ [s] ->
    run (word "set start-" s " precision (start-" s " / sum-of-strategies) 2")]
end

to fill-cells
  ; strategy pool is a list of strategies where the multiplicity of every strategy is in accordance
; with its start ratio; this makes for a cheap and well-known way to pick elements proportionally

let strategy-bag map [ [i] -> rijtje i (1000 * run-result (word "start-" item i strategies)) ] indices
; strategy-bag now looks like [[0 0 0 ...] [1 1 ...] [2 2 2 2 2 ...] ...]
; the length of each sub-list corresponds to the start-proportion of that strategy

let strategy-pool reduce [ [x y] -> sentence x y ] strategy-bag
; strategy-pool now looks like [0 0 0 ... 1 1 ... 2 2 2 2 2 ... ...]
; and contains about 1000 indices

ask patches [
 set strategy one-of strategy-pool  ; "strategy" is a natural number; "one-of" is a Netlogo primitive
 set pcolor item strategy colors    ; give patch the color of the strategy it it assigned to
]
end

to-report rijtje [ x n ] ; e.g., rijtje 7 5 yields [7 7 7 7 7]
  report n-values n [ x ]
end

to calculate-score-table
  set score-table map [ [s] -> score-row-for s ] strategies
end

to-report score-row-for [ s1 ]
  report map [ [s2] -> score-entry-for s1 s2 ] strategies
end

to-report score-entry-for [ s1 s2 ]
  report mean n-values restarts [ score-for s1 s2 ]
end

to-report score-for [s1 s2]
  let my-history       []
  let your-history     []
  let my-total-payoff  0
  repeat rounds [
    let my-action        play s1 my-history your-history
    let your-action      play s2 your-history my-history
    let my-payoff        item your-action (item my-action payoff-matrix)
    set my-total-payoff  my-total-payoff + my-payoff
    set my-history       fput my-action   my-history   ; most recent actions go first
    set your-history     fput your-action your-history
  ]
  report my-total-payoff / rounds
end

to-report play [some-strategy my-history your-history] ;When the noise activates we just pick a random output (0 or 1) to simulate noise.
  report ifelse-value (random-float 1.0 < noise)
  [one-of [0 1]] [runresult (word some-strategy " my-history your-history")]
end

to do-plots
  let frequencies map [ [i] -> count patches with [ strategy = i ] ] indices
  set proportions map [ [i] -> i / count patches ] frequencies ; that's ok: count patches is an inexpensive operation
  let filtered-indices filter [ [i] -> item i frequencies > 0 ] indices ; filter dissapeared strategies
  let indices-sorted-by-proportion sort-by [ [f1 f2] -> item f1 frequencies > item f2 frequencies ] filtered-indices

  foreach indices-sorted-by-proportion ;We simply use our globals to connect the winning strategies to the current number of patches they own.
  [x ->
    (output-write count patches with [strategy = x])
    (output-type " ")
    (output-print item x strategies)
  ]

end

to perzisch-tapijt ;This part creates the 'Persian Carpet' it's used by pressing
  set CC-payoff-reward     3
  set CD-payoff-sucker     0
  set DC-payoff-temptation 6
  set DD-payoff-punishment 1
  set noise 0
  set rounds 3
  set restarts 3
  setup
  ;Then we simply set all but the middle patch to all-C, with the middle patch getting all-D.
  ask patches[
    set strategy 0
  set pcolor green
  ]
  ask patch 0 0
 [
    set strategy 1
    set pcolor red
  ]
end

to print-score-table ;;In order the print the score table we print the score-table...
  clear-output
  output-print score-table
end

to reset ;;we call upon the initialise-run to reseed the field WITHOUT recalculating the score table
  initialise-run
end

to recalc ;;we call upon calculate score-table to... recalculate the score table...
  calculate-score-table
  clear-output
  output-print(score-table)
end

;The following bits of code are all strategy coding, the first few need no explanation

to-report always-cooperate [ my-history your-history ]
  report 0
end

to-report always-defect [ my-history your-history ]
  report 1
end

to-report play-randomly [ my-history your-history ]
  report one-of [1 0]
end

to-report unforgiving [ my-history your-history] ;unforgiving makes sure to start with cooperate and fall into defect as soon as the opponent defects once.
  ifelse sum your-history > 0 [report 1] [report 0]
end

to-report tit-for-tat [ my-history your-history ] ;tit-for-tat starts with cooperate, then just picks whatever the opponent last picked.
  ifelse (your-history = [])
  [report 0]
  [report last your-history]
end

to-report pavlov [ my-history your-history ] ;Pavlov starts with cooperate, but will change strategy whenever the opponent defects.
  if your-history = []
  [report 0]
  ifelse (last your-history = 1)
  [if (last my-history = 1)
  [report 0]
  if (last my-history = 0)
  [report 1]]
  [report last my-history]
end
@#$#@#$#@
GRAPHICS-WINDOW
205
10
942
748
-1
-1
3.0
1
10
1
1
1
0
1
1
1
-121
121
-121
121
0
0
1
ticks
30.0

BUTTON
23
26
87
59
NIL
Setup\n
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
109
74
172
107
NIL
Go
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
19
221
191
254
CC-payoff-reward
CC-payoff-reward
0
100
3.0
1
1
NIL
HORIZONTAL

SLIDER
20
263
192
296
CD-payoff-sucker
CD-payoff-sucker
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
20
304
192
337
DC-payoff-temptation
DC-payoff-temptation
0
100
6.0
1
1
NIL
HORIZONTAL

SLIDER
20
347
193
380
DD-payoff-punishment
DD-payoff-punishment
0
100
1.0
1
1
NIL
HORIZONTAL

BUTTON
107
23
170
56
NIL
Go
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
953
578
1133
611
start-always-cooperate
start-always-cooperate
0
1
0.14
0.01
1
NIL
HORIZONTAL

SLIDER
954
615
1135
648
start-always-defect
start-always-defect
0
1
0.17
0.01
1
NIL
HORIZONTAL

PLOT
948
10
1705
379
Populace
ticks
Proportion
0.0
20.0
0.0
1.0
true
true
"" ""
PENS
"always-cooperate" 1.0 0 -13840069 true "" "plot (count patches with [strategy = 0] / count patches)"
"always-defect" 1.0 0 -2674135 true "" "plot (count patches with [strategy = 1] / count patches)"
"play-random" 1.0 0 -7500403 true "" "plot (count patches with [strategy = 2] / count patches)"
"unforgiving" 1.0 0 -15390905 true "" "plot (count patches with [strategy = 3] / count patches)"
"tit-for-tat" 1.0 0 -8630108 true "" "plot (count patches with [strategy = 4] / count patches)"
"pavlov" 1.0 0 -6459832 true "" "plot (count patches with [strategy = 5] / count patches)"

SLIDER
20
391
192
424
restarts
restarts
0
100
3.0
1
1
NIL
HORIZONTAL

SLIDER
19
431
191
464
rounds
rounds
1
100
3.0
1
1
NIL
HORIZONTAL

SLIDER
20
472
192
505
noise
noise
0
1
0.0
0.01
1
NIL
HORIZONTAL

BUTTON
25
75
91
108
Carpet
Perzisch-tapijt\n
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
25
123
89
156
Reset
reset
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
109
123
175
156
Recalc
recalc
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
950
388
1702
534
11

SLIDER
1147
576
1319
609
start-play-randomly
start-play-randomly
0
1
0.18
0.01
1
NIL
HORIZONTAL

SLIDER
1326
577
1498
610
start-tit-for-tat
start-tit-for-tat
0
1
0.17
0.01
1
NIL
HORIZONTAL

SLIDER
1147
616
1319
649
start-unforgiving
start-unforgiving
0
1
0.18
0.01
1
NIL
HORIZONTAL

SLIDER
1326
616
1498
649
start-pavlov
start-pavlov
0
1
0.16
0.01
1
NIL
HORIZONTAL

SWITCH
1147
541
1319
574
output-rank-list
output-rank-list
1
1
-1000

CHOOSER
20
164
192
209
Visualization
Visualization
"Current Strategy" "Average Payoff" "Strategy Dynamic"
2

BUTTON
955
542
1135
575
Print Score-Table
print-score-table
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1331
545
1592
575
The patches update quicker with output-rank-list turned off. Writing it down takes a second every tick.
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is a Netlogo model of the Iterated Prisoner's Dilemma. It contains all the basic features and boasts a few extra features, these extra features are:

- Option to visualize the patches by payoff. Lighter shades visualize higher payoffs and darker shades visualize lower payoffs.

- Option to visualize the patches by strategy changes. A patch will darken its color by 5% whenever it changes strategy, so darker patches are the ones that have changed the most.

- A button called Carpet which will visualize a 'Persian carpet' dynamic which implies the model updates in a synchronous manner.

Note that world wrapping is on and the world size is 121x121.

## HOW TO USE IT

While most components of the GUI should be pretty self-explanatory. A few parts could use some explaining.

- I chose to make visualization a chooser rather than 3 seperate buttons. The upside is it can be used while running go and you can actively see the changes happen. The downside is that go needs to run once before the visuals change.

- I made a output-rank-list switch because I noticed waiting for all the numbers to be written down caused a bit of a delay (since go doesn't want to loop before all the numbers are written down) so I put it on a switch to turn off.

- print-score-table is a button that well... prints the score table. Don't use it while running go since go clears the output and you'd have about a third of a second to see the score table.
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
