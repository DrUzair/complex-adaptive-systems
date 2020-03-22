globals[
  nb-infected-previous  ;; Number of infected people at the previous tick
  nb-infected-w-symptoms  ;; Number of infected people at the previous tick
  nb-infected-wo-symptoms  ;; Number of infected people at the previous tick
  cumsum-infected-child
  cumsum-infected-teen
  cumsum-infected-adult
  cumsum-infected-old
  beta-n                ;; The average number of new secondary
                        ;; infections per infected this tick
  gamma                 ;; The average number of new recoveries
                        ;; per infected this tick
  r0                    ;; The number of secondary infections that arise
                        ;; due to a single infected introduced in a wholly
                        ;; susceptible population
  restrict-mobility?    ;; false at time 0
]

turtles-own [
  is_infected?
  is_cured?
  show_symptoms?
  is_alive?           ;;
  infected_on         ;; first day of catching infection
  incubation-time     ;; Time (in dayes) it takes before the person shows symptoms of the infection
  recovery-time       ;; Time (in dayes) it takes before the person has a chance to recover from the infection
  is_susceptible?     ;; Tracks whether the person was initially susceptible
  nb-infected         ;; Number of secondary infections caused by an
                      ;; infected person at the end of the tick
  nb-recovered        ;; Number of recovered people at the end of the tick
  nb-infected-total   ;; total people infected by this agent
  mobility            ;; ability to move <step / day or tick>
  age-group                 ;; age <teen><twenties><thirties><forties><fifties><sixties>
  travel              ;; traveled distance
  homex               ;;
  homey
]

to setup
  if seed = 0 [set seed random 100]
  random-seed seed
  clear-all
  reset-ticks
  setup-population
  set restrict-mobility? false
  ask n-of 1 turtles with [(xcor < 5 and xcor > -5) and (ycor < 5 and ycor > -5)] [
    set is_infected? true
    set infected_on ticks
    show who
    assign-color
  ]



end

;to setup-pop-agewise[
;  child teen twent thirt fourt fift sixt old = [0.1 0.1 0.1 0.1 0.1 0.1]
;]
to setup-population
  set cumsum-infected-child 0
  set cumsum-infected-teen 0
  set cumsum-infected-adult 0
  set cumsum-infected-old 0
  (foreach [0.25 0.25 0.25 0.25] ["child" "teen" "middle-age" "old"] [0.5 0.6 1 0.8]
    [ [age-proportion age-grp size_] ->
       create-turtles total-population * age-proportion
       [
          setxy  random-xcor random-ycor
          set homex xcor
          set homey ycor
          set age-group age-grp
          set size size * size_
          set is_infected? false
          set is_cured? false
          set show_symptoms? false
          set is_susceptible? true
          set is_alive? true
          set nb-infected-total 0
          set mobility 2 + random-exponential mobility-mean ;; (- mean) * ln random-float 1.0
          set incubation-time random-normal show-symptoms-time 1
          set recovery-time incubation-time + average-recovery-time + random-exponential 1.2
          ;print(recovery-time)
          set travel 0
          set shape "person"
          set color white
          assign-color
       ]
     ]
    )
  tick
end

to assign-color  ;; turtle procedure
  if is_infected?
    [ set color blue ]
  if show_symptoms?
    [ set color red ]
  if is_cured?
    [ set color green ]
  if not is_alive?
    [ set color yellow ]
end

to go
  ifelse show-infection-links[ask links [show-link]][ask links [hide-link]]

  if all? turtles [ not is_infected? ][
    set cumsum-infected-child count turtles with [ is_cured? and age-group = "child"]
    set cumsum-infected-teen  count turtles with [ is_cured? and age-group = "teen"]
    set cumsum-infected-adult count turtles with [ is_cured? and age-group = "middle-age"]
    set cumsum-infected-old  count turtles with [ is_cured? and age-group = "old"]
    stop

  ]

  ifelse show-infection-links[ask links [show-link]][ask links [hide-link]]

  ask turtles
    [
      if is_alive? [
        move
        clear-count
      ]
    ]

  ask turtles with [ is_infected? ]
    [
      infect
      maybe-show-symptoms
      maybe-recover
    ]

  ask turtles
    [
      assign-color
      calculate-r0
    ]
  if (nb-infected-previous / total-population) * 100 > restrict-mobility-threshold [ set restrict-mobility? true] ; will be turned on once and for always



  tick
end

;; People move about at random.
to move  ;; turtle procedure
  ;if who = 123 [pen-down 123]

  let angle 0
  let steps 0
  let a random 360
  let angles [0 90 180 270 360]
  ifelse restrict-mobility? [
    if restrict-mobility-policy = "all"
    [
      set angle 90
      set steps 0.3
    ]
    if restrict-mobility-policy = "no"
    [
      set angle one-of angles ;  random-float 360
      set steps random-normal mobility 1
    ]
    if restrict-mobility-policy = "infected-with-symtoms"
    [
      ifelse show_symptoms? [
        set angle 90
        set steps 0.3
      ]
      [
        set angle one-of angles ; random-float 360
        set steps random-normal mobility 1
      ]
    ]
  ][
    set angle one-of angles ;  random-float 360
    set steps random-normal mobility 1
  ]
  if (age-group = "old" or age-group = "child") [
    ;print( word steps " steps for old " (steps * 0.1) )
    set steps 0.3
    set angle 90
  ]
  ;if who = 423 [print (word "away from home " sqrt( (xcor - homex) ^ 2 + (ycor - homey) ^ 2 ) )]
  if sqrt( (xcor - homex) ^ 2 + (ycor - homey) ^ 2 ) > 5 [
    setxy homex homey
  ]
  ;if who = 423 [print (word "away from home " sqrt( (xcor - homex) ^ 2 + (ycor - homey) ^ 2 ) )]
  rt angle
  fd steps
  set travel travel + steps
end

to clear-count
  set nb-infected 0
  set nb-recovered 0
end

;; Infection can occur to any susceptible person nearby
to infect  ;; turtle procedure
   ;print(word "infected " who)
   let source who
   let counter 0
   let nearby-uninfected (turtles-on neighbors) with [ not is_infected? and not is_cured? ]
     if nearby-uninfected != nobody
     [ ask nearby-uninfected
       [ if random-float 100 < infection-chance
         [
           ;print(word "nearby " who)
           let dest who
           set is_infected? true
           set infected_on ticks
           set nb-infected (nb-infected + 1)
           set counter counter + 1
           ask turtle source [
            create-link-to turtle dest
            ;show out-link-to turtle dest
           ]
         ]
       ]
     ]
  set nb-infected-total nb-infected-total + counter
end

to maybe-show-symptoms
  if random-float 100 < show-symptoms-chance [
    if (ticks - infected_on) > incubation-time and not show_symptoms? [
      set show_symptoms? true
    ]
  ]
end

to maybe-recover
  ;; If people have been infected for more than the recovery-time
  ;; then there is a chance for recovery
  if (ticks - infected_on) > recovery-time
  [
    ;set show_symptoms? false
    let rc recovery-chance
    if (age-group = "old") [
      set rc recovery-chance * 0.8
    ] ; recovery chance 20% less for old people

    if (age-group = "child" or age-group = "teen") [
      set rc recovery-chance + (100 - recovery-chance) - (0.01)
    ] ;

    ifelse random-float 100 < rc
    [
      set is_infected? false
      set is_cured? true
      print( who)
      set nb-recovered (nb-recovered + 1)
    ][
      set is_alive? false
    ]
  ]
end

to calculate-r0

  let new-infected sum [ nb-infected ] of turtles
  let new-recovered sum [ nb-recovered ] of turtles

  ;; Number of infected people at the previous tick:
  set nb-infected-previous
    count turtles with [ is_infected? ] + new-recovered - new-infected

  ;; Number of susceptibles now:
  let is_susceptible-t
    total-population - count turtles with [ is_infected? ] - count turtles with [ is_cured? ]

  ;; Initial number of susceptibles:
  let s0 count turtles with [ is_susceptible? ]

  ifelse nb-infected-previous < 10
  [ set beta-n 0 ]
  [
    ;; This is beta-n, the average number of new
    ;; secondary infections per infected per tick
    set beta-n (new-infected / nb-infected-previous)
  ]

  ifelse nb-infected-previous < 10
  [ set gamma 0 ]
  [
    ;; This is the average number of new recoveries per infected per tick
    set gamma (new-recovered / nb-infected-previous)
  ]

  ;; Prevent division by 0:
  if total-population - is_susceptible-t != 0 and is_susceptible-t != 0
  [
    ;; This is derived from integrating dI / dS = (beta*SI - gamma*I) / (-beta*SI):
    set r0 (ln (s0 / is_susceptible-t) / (total-population - is_susceptible-t))
    ;; Assuming one infected individual introduced in the beginning,
    ;; and hence counting I(0) as negligible, we get the relation:
    ;; N - gamma*ln(S(0)) / beta = S(t) - gamma*ln(S(t)) / beta,
    ;; where N is the initial 'is_susceptible' population
    ;; Since N >> 1
    ;; Using this, we have R_0 = beta*N / gamma = N*ln(S(0)/S(t)) / (K-S(t))
    set r0 r0 * s0 ]
end
@#$#@#$#@
GRAPHICS-WINDOW
415
10
1193
789
-1
-1
15.122
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
1
days
30.0

BUTTON
197
728
287
787
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

SLIDER
226
93
409
126
average-recovery-time
average-recovery-time
0
30
7.0
1
1
days
HORIZONTAL

SLIDER
30
10
212
43
total-population
total-population
0
2000
1006.0
1
1
NIL
HORIZONTAL

BUTTON
294
728
379
788
go
go
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
228
11
408
44
infection-chance
infection-chance
10
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
227
54
408
87
recovery-chance
recovery-chance
1
100
89.0
1
1
NIL
HORIZONTAL

TEXTBOX
767
647
1004
722
White: Not Infected \nGreen: Infected but Recovered\nBlue: Infected but not showing Symptoms\nRed: Infected and Showing Symptoms\nYellow: No recovery (Death)
12
0.0
1

SLIDER
29
92
213
125
show-symptoms-time
show-symptoms-time
0
7
5.0
1
1
days
HORIZONTAL

PLOT
1200
12
1530
289
Virus Spread
Days
Percentage of Population
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"infected wo Symp" 1.0 0 -13345367 true "" "plot (((count turtles with [ is_infected? and not show_symptoms? ]) / total-population) * 100)"
"Infected w Symp" 1.0 0 -2674135 true "" "plot ( ((count turtles with [ show_symptoms? ]) / total-population) * 100)"
"Cured" 1.0 0 -10899396 true "" "plot ((count turtles with [ is_cured? ] / total-population) * 100)"
"Ever Infected" 1.0 0 -7500403 true "" ";plot ( ( (count turtles with [ not is_infected? and not is_cured?]) / total-population) * 100)\nplot( 100 - ( (total-population - count turtles with [is_infected? or is_cured?] ) / total-population) * 100)"
"infected" 1.0 0 -955883 true "" "plot (((count turtles with [ is_infected? ]) / total-population) * 100)"

PLOT
1200
292
1528
521
Infections / Day
Days
Infections
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"child" 1.0 0 -2674135 true "" "plot count turtles with [age-group = \"child\" and is_infected?]"
"teen" 1.0 0 -13345367 true "" "plot count turtles with [age-group = \"teen\" and is_infected?]"
"adult" 1.0 0 -6459832 true "" "plot count turtles with [age-group = \"middle-age\" and is_infected?]"
"old" 1.0 0 -7500403 true "" "plot count turtles with [age-group = \"old\" and is_infected?]"

SLIDER
30
51
214
84
show-symptoms-chance
show-symptoms-chance
0
100
50.0
1
1
NIL
HORIZONTAL

CHOOSER
227
274
401
319
restrict-mobility-policy
restrict-mobility-policy
"infected-with-symtoms" "all" "no"
1

SLIDER
29
273
201
306
mobility-mean
mobility-mean
0
5
1.0
.1
1
NIL
HORIZONTAL

PLOT
227
323
406
471
Mobility
Mobility 
Number of People
1.0
15.0
0.0
15.0
true
false
"set-plot-pen-mode 1" ""
PENS
"default" 1.0 0 -2064490 false "" "histogram [mobility] of turtles"

SLIDER
30
480
407
513
restrict-mobility-threshold
restrict-mobility-threshold
0
5
0.1
.1
1
% of pop infected
HORIZONTAL

PLOT
32
322
209
472
Travel
NIL
NIL
0.0
200.0
0.0
40.0
true
true
"set-plot-pen-mode 1\n" ";set-plot-x-range 0 round max [travel] of turtles"
PENS
"old" 1.0 1 -7500403 true "" "histogram [travel] of turtles with [age-group = \"old\"]"
"teen" 10.0 1 -13345367 true "" "histogram [travel] of turtles with [age-group = \"teen\"]"
"child" 2.0 1 -955883 true "" "histogram [travel] of turtles with [age-group = \"child\"]"
"adult" 1.0 0 -6459832 true "" "histogram [travel] of turtles with [age-group = \"middle-age\"]"

PLOT
1530
292
1869
521
Death Toll
Days
Death Count
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"total" 1.0 0 -16777216 true "" "plot count turtles with [not is_alive?]"
"child" 1.0 0 -955883 true "" "plot count turtles with [not is_alive? and age-group = \"child\"]"
"teen" 1.0 0 -13345367 true "" "plot count turtles with [not is_alive? and age-group = \"teen\"]"
"adult" 1.0 0 -6459832 true "" "plot count turtles with [not is_alive? and age-group = \"middle-age\"]"
"old" 1.0 0 -7500403 true "" "plot count turtles with [not is_alive? and age-group = \"old\"]"

PLOT
1200
521
1528
769
New Infections & Recoveries / Day
Days
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"new-infect" 1.0 0 -2674135 true "" "plot sum [ nb-infected ] of turtles"
"new-recover" 1.0 0 -10899396 true "" "plot sum [ nb-recovered ] of turtles"

MONITOR
100
675
324
720
Population Density
word \"density \" (count turtles / count patches)
17
1
11

MONITOR
35
674
92
723
R0
R0
2
1
12

PLOT
33
518
212
668
Total Infections Spread 
Total Infections Spread 
Number of People
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -2674135 true "" "histogram [nb-infected-total] of turtles with [nb-infected-total > 0]"

SWITCH
239
521
403
554
show-infection-links
show-infection-links
0
1
-1000

PLOT
1532
13
1870
288
Cumulative Infections
Days
Number of Infections
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"chid" 1.0 0 -955883 true "" "plot count turtles with [ is_infected? or is_cured? and age-group = \"child\"]"
"teen" 1.0 0 -13345367 true "" "plot count turtles with [ is_infected? or is_cured? and age-group = \"teen\"]"
"adult" 1.0 0 -6459832 true "" "plot count turtles with [ is_infected? or is_cured? and age-group = \"middle-age\"]"
"old" 1.0 0 -7500403 true "" "plot count turtles with [ is_infected? or is_cured? and age-group = \"old\"]"
"total" 1.0 0 -16777216 true "" "plot count turtles with [ is_infected? or is_cured?]"

INPUTBOX
37
728
192
788
seed
2.0
1
0
Number

PLOT
30
129
213
265
incubation-time
days
people
0.0
15.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -2674135 true "" "histogram [incubation-time] of turtles with [is_infected? or is_cured?]"

PLOT
227
128
410
265
Recovery time
days
people
7.0
25.0
0.0
50.0
true
false
"" "set-plot-x-range (average-recovery-time - show-symptoms-time) (show-symptoms-time + average-recovery-time) * 2"
PENS
"default" 1.0 1 -10899396 true "" "histogram [recovery-time] of turtles "

@#$#@#$#@
## WHAT IS IT?

### Flatten the Curve
Spread of Corona Virus and prevailing "social distancing" efforts have made everybody think. 
 - How long is going to last ?
 - What if it comes back again ?

This simulation allows you to try different scenarios and find answers for your questions.

Virus spread 
- Symptoms show after several days of infection

Virus spread control by restricting mobility of
- Infected ones (showing symptoms)
- Everyone

## HOW IT WORKS

Infection
- Simulation randomly selects initially infected (source) person/s.

Mobility
-Each person goes out to work everyday. The mobility is age appropriate as for child, teen, adult and old person.
- Each person moves around in semi-human fashion (rectangular routs as in city blocks). 
- The movement is anchored to a home location, eveyone comes back home after a day of work
- Some of the people move way more than average people. Average people may more 2 blocks a day whereas a few exceptional cases may move as long as 15 blocks.

Spread
- Virus can spread to neighbors (immediate next person). There can possibly be 8 neighbors for a person.
- Once acquired, a virus becomes active but the symptoms of infection may appear after several days.

Spread Control
- User can tryout different control stretagies to contain the virus

## HOW TO USE IT

Select desired values for;
- Population 
- Infection-chance: % of times a healthy person contracts virus from infected one.
- Recovery-Chance: % of times an infected person recovers from infection
- Average-Recovery-Time: Normally distributed time-period after which patients recover
- Show-Symptoms-Time: Normally distributed time-period after which patients show symptoms
- Mobility-mean: each agent follows an exponential distribution of mobility. (90% move 2 block a day, rest of the 10% may move large distances)
- Restrict-Mobility-Threshold: % of infected population to enforce restriction on mobility
- Restrict-Mobility-Policy: All (Every one is restricted to home), Infected (Only patients), No (no one is restricted)

Color code:
- White: Not Infected 
- Green: Infected but Recovered
- Blue: Infected but not showing Symptoms
- Red: Infected and Showing Symptoms
- Yellow: No recovery (Death)

Press Setup
press Go

## THINGS TO NOTICE

- Increase the infection chance parameter
- 

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

- age appropriate mobility
- more humanistic mobility model
- different population densities side by side.

## NETLOGO FEATURES


## RELATED MODELS
EpiDEM Basic Model in NetLogo Models Library


## CREDITS AND REFERENCES
Uzair Ahmad
SPIME LAB, ON, Canada
L3R 5X2
uzairg@gmail.com
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
<experiments>
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>cumsum-infected-child</metric>
    <metric>cumsum-infected-teen</metric>
    <metric>cumsum-infected-adult</metric>
    <metric>cumsum-infected-old</metric>
    <metric>R0</metric>
    <metric>seed</metric>
    <enumeratedValueSet variable="seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-mobility-threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-recovery-time">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-symptoms-chance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-population">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="restrict-mobility-policy">
      <value value="&quot;all&quot;"/>
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infection-chance">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mobility-mean">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-symptoms-time">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="90"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
