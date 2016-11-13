(*
Autor: Jakub Wróblewski
Recenzent Bartosz Staneta*)
type wartosc = {a : float; b : float; ant : bool};;
(* a - początek przedziału
 * b - koniec przedziału
 * ant - bool określający czy wartosc jest antyprzedziałem
 *       dla ant = false <a,b>
 *       dla ant = true (-inf, a> + <b, +inf)
 *)

(*funkcje min i max naprawiajace bledy z nan*)
let nmin x y =
    if(classify_float x = FP_nan)
    then y
    else if(classify_float y = FP_nan)
         then x
         else min x y
    ;;
    
let nmax x y =
    if(classify_float x = FP_nan)
    then y
    else if(classify_float y = FP_nan)
         then x
         else max x y
    ;;
    
(*funkcja mnożenia która zmienia pewne wartości float (inf *. 0 = 0)*)
let mlp x y =
    if((x = 0. && (y = neg_infinity || y = infinity))
    || ((x = neg_infinity || x = infinity) && y = 0.))
    then 0.
    else x *. y
    ;;
    
(*funkcja dodawania która zmienia pewne wartości float (-inf + inf = -inf)*)
let add x y =
    if((x = neg_infinity && y = infinity)
    || (x = infinity && y = neg_infinity))
    then neg_infinity
    else x +. y
    ;;
    
let wartosc_dokladnosc x p =
    {a = nmin (x -. (x *. p /. 100.)) (x +. (x *. p /. 100.));
    b = nmax (x -. (x *. p /. 100.)) (x +. (x *. p /. 100.));
    ant = false}
    ;;
    
let wartosc_od_do x y =
    if(x<=y)
    then {a = x; b = y; ant = false}
    else {a = y; b = x; ant = true}
    ;;
    
let wartosc_dokladna x =
    {a = x; b = x; ant = false}
    ;;
    
let in_wartosc w x =
    if((w.ant = false && (w.a <= x && x <= w.b))
    || (w.ant = true && (x <= w.a || w.b <= x)))
    then true
    else false
    ;;
    
let min_wartosc w =
    match w.ant with
    | false -> w.a
    | true  -> neg_infinity
    ;;
    
let max_wartosc w =
    match w.ant with
    | false -> w.b
    | true  -> infinity
    ;;
    
let sr_wartosc w =
    match w.ant with
    | false -> (w.a +. w.b) /. 2.
    | true  -> nan
    ;;
    
(*funkcja dodająca przedziały*)
let plus w1 w2 =
    match (w1.ant, w2.ant) with
    | (false , false) -> {a = (add w1.a w2.a); b = (add w1.b w2.b); ant = false}
    | (true , false) -> if((w1.a +. w2.b) < (w1.b +. w2.a))
                    then {a = w1.a +. w2.b; b = w1.b +. w2.a; ant = true}
                    else {a = neg_infinity; b = infinity; ant = false}
    | (false , true) -> if((w2.a +. w1.b) < (w2.b +. w1.a))
                    then {a = w2.a +. w1.b; b = w2.b +. w1.a; ant = true}
                    else {a = neg_infinity; b = infinity; ant = false}
    | (true , true) -> {a = neg_infinity; b = infinity; ant = false}
    ;;
    
(*funkcja odejmująca przedziały*)
let minus w1 w2 =
    match (w1.ant, w2.ant) with
    | (false , false) -> {a = w1.a -. w2.b; b = w1.b -. w2.a; ant = false}
    | (true , false) -> if((w1.a -. w2.a) < (w1.b -. w2.b))
                    then {a = w1.a -. w2.a; b = w1.b -. w2.b; ant = true}
                    else {a = neg_infinity; b = infinity; ant = false}
    | (false , true) -> if((w1.b -. w2.b) < (w1.a -. w2.a))
                    then {a = w1.b -. w2.b; b = w1.a -. w2.a; ant = true}
                    else {a = neg_infinity; b = infinity; ant = false}
    | (true , true) -> {a = neg_infinity; b = infinity; ant = false}
    ;;
    
(*funkcja łącząca dwa przedziały w jeden*)
let sum w1 w2 =
    if(w1.a = 0. && w1.b = 0. && w2.a = 0. && w2.b = 0.)
    then {a = 0.; b = 0.; ant = false}
    else match (w1.ant, w2.ant) with
    | (false , false) ->
        if(w1.a = neg_infinity && w2.a = neg_infinity)
        then {a = neg_infinity; b = nmax w1.b w2.b; ant = false}
        else if(w1.a = neg_infinity && w2.b = infinity)
             then if(w1.b < w2.a)
                  then {a = w1.b; b = w2.a; ant = true}
                  else {a = neg_infinity; b = infinity; ant = false}
             else if(w1.b = infinity && w2.a = neg_infinity)
                  then if(w2.b < w1.a)
                       then {a = w2.b; b = w1.a; ant = true}
                       else {a = neg_infinity; b = infinity; ant = false}
                  else if(w1.b = infinity && w2.b = infinity)
                       then {a = (nmin w1.a w2.a); b = infinity; ant = false}
                       else {a = nan; b = nan; ant = false}
    | (true , false) ->
        if(w2.a = neg_infinity)
        then
            if(w2.b<w1.b)
            then {a = nmax w1.a w2.b; b=w1.b; ant = true}
            else {a = neg_infinity; b = infinity; ant = false}
        else
            if(w2.b = infinity)
            then
                if(w1.a < w2.a)
                then {a = w1.a;b = nmin w1.b w2.a; ant = true}
                else {a = neg_infinity; b = infinity; ant = false}
            else {a = nan; b = nan; ant = false}
    | (false , true) ->
        if(w1.a = neg_infinity)
        then
            if(w1.b < w2.b)
            then {a = nmax w2.a w1.b;b = w2.b; ant = true}
            else {a = neg_infinity; b = infinity; ant = false}
        else
            if(w1.b = infinity)
            then
                if(w2.a < w1.a)
                then {a = w2.a; b = nmin w1.a w2.b; ant = true}
                else {a = neg_infinity; b = infinity; ant = false}
            else {a = nan; b = nan; ant = false}
    | (true , true) ->
        if((nmax w1.a w2.a) < (nmin w1.b w2.b))
        then {a = nmax w1.a w2.a;b = nmin w1.b w2.b; ant = true}
        else {a = neg_infinity; b = infinity; ant = false}
    ;;
    
(*funkcja mnożąca przedziały*)
let rec razy w1 w2 = 
    match (w1.ant, w2.ant) with
    | (false , false) ->
        {a = nmin(nmin(nmin(mlp w1.a w2.a)
                           (mlp w1.a w2.b))
                      (mlp w1.b w2.a))
                 (mlp w1.b w2.b);
         b = nmax(nmax(nmax(mlp w1.a w2.a)
                           (mlp w1.a w2.b))
                      (mlp w1.b w2.a))
                 (mlp w1.b w2.b);
        ant = false}
    | (true , false) ->
        sum (razy {a = neg_infinity; b = w1.a; ant = false} w2)
            (razy {a = w1.b; b = infinity; ant = false} w2)
    | (false , true) ->
        sum(razy w1 {a = neg_infinity; b = w2.a; ant = false}) 
           (razy w1 {a = w2.b; b = infinity; ant = false})
    | (true , true) ->
        sum(sum(razy {a = neg_infinity; b = w1.a; ant = false}
                     {a = neg_infinity; b = w2.a; ant = false})
               (razy {a = neg_infinity; b = w1.a; ant = false} 
                     {a = w2.b; b = infinity; ant = false}))
           (sum(razy {a = w1.b; b = infinity; ant = false}
                     {a = neg_infinity; b = w2.a; ant = false})
               (razy {a = w1.b; b = infinity; ant = false}
                     {a = w2.b; b = infinity; ant = false}))
    ;;
    
(*funkcja dzieląca przedziały*)
let podzielic w1 w2 =
    if(w2.a = 0. && w2.b = 0.)
    then {a = nan; b = nan; ant = false}
    else match w2.ant with
    | false ->
        if(in_wartosc w2 0. = true)
        then
            if(w2.a = 0. || w2.b = 0.)
            then
                if(w2.a = 0.)
                then razy w1 
                    {a = 1. /. w2.b;
                     b = infinity;
                     ant = false}
                else razy w1 
                    {a = neg_infinity;
                     b = 1. /. w2.a;
                     ant = false}
            else razy w1
                {a = nmin (1. /. w2.a) (1. /. w2.b);
                 b = nmax (1. /. w2.a) (1. /. w2.b);
                 ant = true}
        else razy w1
            {a = nmin (1. /. w2.b) (1. /. w2.a);
             b = nmax (1. /. w2.b) (1. /. w2.a);
             ant = false}
    | true ->
        if(in_wartosc w2 0. = true)
        then razy w1
            {a = nmin (1. /. w2.a) (1. /. w2.b);
             b = nmax (1. /. w2.a) (1. /. w2.b);
             ant = true}
        else razy w1
            {a = nmin (1. /. w2.b) (1. /. w2.a);
             b = nmax (1. /. w2.b) (1. /. w2.a);
             ant = false}
    ;;
    

    
    
(*podzielic (wartosc_od_do (5.) (0.) ) (wartosc_od_do (-7.) (-22.) );;*)
(*razy(wartosc_od_do (5.) (0.)) (wartosc_od_do (-1. /. 22.)(-1. /. 7.));;*)
(*
let dod0 = wartosc_dokladna 5.
let dod1 = wartosc_dokladnosc 6. 50. (* <3, 9> *)
let dod2 = wartosc_od_do 2. 3.
let dod3 = wartosc_od_do 0. 1.
let dod4 = wartosc_od_do 5. infinity

let ujm0 = wartosc_dokladna (-4.)
let ujm1 = wartosc_dokladnosc (-8.) 50. (* <-12, -4> *)
let ujm2 = wartosc_od_do (-5.) (-3.)
let ujm3 = wartosc_od_do (-1.) 0.
let ujm4 = wartosc_od_do neg_infinity (-6.)

let mix0 = wartosc_dokladna 0.
let mix1 = wartosc_od_do (-10.) 5.
let mix2 = wartosc_od_do (-1.) 1.
let mix3 = wartosc_od_do (-0.5) 0.5
let mix4 = wartosc_od_do neg_infinity 8.
let mix5 = wartosc_od_do (-7.) infinity
let mix6 = wartosc_od_do neg_infinity infinity
let mix7 = wartosc_od_do 5. (-3.)
;;

assert(sr_wartosc mix5 = infinity);
assert(sr_wartosc mix4 = neg_infinity);
assert(sr_wartosc dod2 = 2.5);
assert(sr_wartosc mix7 == nan);

assert(max_wartosc mix4 = 8.);
assert(max_wartosc mix7 = infinity);
assert(max_wartosc mix0 = 0.);

assert(min_wartosc mix1 = (-10.));
assert(min_wartosc mix0 = 0.);
assert(min_wartosc mix7 = neg_infinity);

assert(in_wartosc mix7 (-10.) = true);
assert(in_wartosc mix7 6. = true);
assert(in_wartosc mix7 4. = false);
assert(in_wartosc mix0 0.1 = false);
assert(in_wartosc mix0 0. = true);
assert(in_wartosc mix6 0. = true);
assert(in_wartosc dod4 4.999 = false);
assert(in_wartosc dod4 5. = true);

assert(min_wartosc (podzielic mix5 mix4) = neg_infinity);
assert(min_wartosc (podzielic mix3 mix4) = neg_infinity);
assert(min_wartosc (podzielic mix6 mix6) = neg_infinity);
assert(min_wartosc (podzielic mix6 mix0) == nan);
assert(min_wartosc (podzielic mix1 mix2) = neg_infinity);

assert(min_wartosc (razy mix2 mix2) = (-1.));
assert(min_wartosc (razy mix1 mix2) = (-10.));
assert(min_wartosc (razy mix0 mix6) = 0.);

assert(min_wartosc (minus mix5 mix5) = neg_infinity);
assert(min_wartosc (minus mix4 dod3) = neg_infinity);
assert(min_wartosc (minus dod2 dod4) = neg_infinity);
assert(min_wartosc (minus dod2 dod3) = 1.);

assert(min_wartosc (plus mix1 mix2) = (-11.));
assert(min_wartosc (plus ujm2 dod2) = (-3.));
assert(min_wartosc (plus mix1 dod2) = (-8.));
assert(min_wartosc (plus mix5 dod2) = (-5.));
assert(min_wartosc (plus mix4 dod3) = neg_infinity);
assert(min_wartosc (plus mix5 dod4) = (-2.));
assert(min_wartosc (plus mix4 dod4) = neg_infinity);
assert(min_wartosc (plus mix6 dod4) = neg_infinity);
assert(min_wartosc (plus ujm4 dod4) = neg_infinity);
assert(min_wartosc (plus dod2 ujm2) = (-3.));
assert(min_wartosc (plus dod2 mix1) = (-8.));
assert(min_wartosc (plus dod2 mix5) = (-5.));
assert(min_wartosc (plus dod3 mix4) = neg_infinity);
assert(min_wartosc (plus dod4 mix5) = (-2.));
assert(min_wartosc (plus dod4 mix4) = neg_infinity);
assert(min_wartosc (plus dod4 mix6) = neg_infinity);
assert(min_wartosc (plus dod4 ujm4) = neg_infinity);
assert(min_wartosc (plus dod0 dod1) = 8.);

assert(max_wartosc (plus dod0 dod2) = 8.);
assert(max_wartosc (plus dod0 dod3) = 6.);
assert(max_wartosc (plus dod0 dod4) = infinity);
assert(max_wartosc (plus dod0 ujm0) = 1.);
assert(max_wartosc (plus dod0 ujm1) = 1.);
assert(max_wartosc (plus dod0 ujm2) = 2.);
assert(max_wartosc (plus dod0 ujm3) = 5.);
assert(max_wartosc (plus dod0 ujm4) = (-1.));
assert(max_wartosc (plus dod0 mix0) = 5.);
assert(max_wartosc (plus dod0 mix1) = 10.);
assert(max_wartosc (plus dod0 mix2) = 6.);
assert(max_wartosc (plus dod0 mix3) = 5.5);
assert(max_wartosc (plus dod0 mix4) = 13.);
assert(max_wartosc (plus dod0 mix5) = infinity);
assert(max_wartosc (plus dod0 mix6) = infinity);
assert(max_wartosc (plus dod1 dod2) = 12.);
assert(max_wartosc (plus dod1 dod3) = 10.);
assert(max_wartosc (plus dod1 dod4) = infinity);
assert(max_wartosc (plus dod1 ujm0) = 5.);
assert(max_wartosc (plus dod1 ujm1) = 5.);
;;



let test1 = if (podzielic (wartosc_od_do (4.) (6.) ) (wartosc_od_do (2.) (3.) ) = wartosc_od_do (4. /. 3.) (3.)) then 1 else 0;;
let test2 = if (podzielic (wartosc_od_do (4.) (6.) ) (wartosc_od_do (-1.) (1.) ) = wartosc_od_do (4.) (-4.)) then 2 else 0;;
let test3 = if (podzielic (wartosc_od_do (4.) (6.) ) (wartosc_od_do (-1.) (0.) ) = wartosc_od_do (neg_infinity) (-4.)) then 3 else 0;;
let test4 = if (podzielic (wartosc_od_do (-4.) (2.) ) (wartosc_od_do (-1.) (0.) ) = wartosc_od_do (neg_infinity) (infinity)) then 4 else 0;;
let test5 = if (podzielic (wartosc_od_do (-4.) (2.) ) (wartosc_od_do (2.) (3.) ) = wartosc_od_do (-2.) (1.)) then 5 else 0;;
let test6 = if (podzielic (wartosc_od_do (-4.) (2.) ) (wartosc_od_do (-3.) (-2.) ) = wartosc_od_do (-1.) (2.)) then 6 else 0;;
let test7 = if (podzielic (wartosc_od_do (2.) (4.) ) (wartosc_od_do (-2.) (2.) ) = wartosc_od_do (1.) (-1.)) then 7 else 0;;
let test8 = if (podzielic (wartosc_od_do (-4.) (-2.) ) (wartosc_od_do (-2.) (2.) ) = wartosc_od_do (1.) (-1.)) then 8 else 0;;
let test9 = if (podzielic (wartosc_od_do (2.) (4.) ) (wartosc_od_do (neg_infinity) (2.) ) = wartosc_od_do (1.) (0.)) then 9 else 0;;
let test10 = if (podzielic (wartosc_od_do (2.) (4.) ) (wartosc_od_do (-4.) (0.) ) = wartosc_od_do (neg_infinity) (-0.5)) then 10 else 0;;
let test11 = if (podzielic (wartosc_od_do (2.) (4.) ) (wartosc_od_do (0.) (5.) ) = wartosc_od_do (0.4) (infinity)) then 11 else 0;;
let test12 = if (podzielic (wartosc_od_do (2.) (4.) ) (wartosc_od_do (0.) (infinity) ) = wartosc_od_do (0.) (infinity)) then 12 else 0;;
let test13 = if (podzielic (wartosc_od_do (-4.) (-2.) ) (wartosc_od_do (-5.) (0.) ) = wartosc_od_do (0.4) (infinity)) then 13 else 0;;
let test14 = if (podzielic (wartosc_od_do (-4.) (-2.) ) (wartosc_od_do (0.) (5.) ) = wartosc_od_do (neg_infinity) (-0.4)) then 13 else 0;;
let test15 = if (podzielic (wartosc_od_do (2.) (4.) ) (wartosc_od_do (3.) (5.) ) = wartosc_od_do (2./.5.) (4./.3.)) then 14 else 0;;
let test16 = if (podzielic (wartosc_od_do (-5.) (0.) ) (wartosc_od_do (-3.) (0.) ) = wartosc_od_do (0.) (infinity)) then 15 else 0;;
let test17 = if (podzielic (wartosc_od_do (-5.) (0.) ) (wartosc_od_do (0.) (3.) ) = wartosc_od_do (neg_infinity) (0.)) then 16 else 0;;
let test18 = if (podzielic (wartosc_od_do (0.) (5.) ) (wartosc_od_do (0.) (3.) ) = wartosc_od_do (0.) (infinity)) then 17 else 0;;
let test19 = if (podzielic (wartosc_od_do (0.) (5.) ) (wartosc_od_do (-3.) (0.) ) = wartosc_od_do (neg_infinity) (0.)) then 18 else 0;;
let test20 = if (podzielic (wartosc_od_do (5.) (6.) ) (wartosc_od_do (2.) (4.) ) = wartosc_od_do (1.25) (3.)) then 19 else 0;;
let test21 = if (podzielic (wartosc_od_do (-1.) (1.) ) (wartosc_od_do (-6.) (-5.) ) = wartosc_od_do (-0.2) (0.2)) then 20 else 0;;
let test22 = if (podzielic (wartosc_od_do (-1.) (1.) ) (wartosc_od_do (5.) (6.) ) = wartosc_od_do (-0.2) (0.2)) then 21 else 0;;
let test23 = if (podzielic (wartosc_od_do (0.) (1.) ) (wartosc_od_do (0.) (1.) ) = wartosc_od_do (0.) (infinity)) then 22 else 0;;
let test24 = if (podzielic (wartosc_od_do (0.) (2.) ) (wartosc_od_do (5.) (6.) ) = wartosc_od_do (0.) (0.4)) then 23 else 0;;
let test25 = if (podzielic (wartosc_od_do (0.) (2.) ) (wartosc_od_do (-6.) (-5.) ) = wartosc_od_do (-0.4) (0.)) then 24 else 0;;
let test26 = if (podzielic (wartosc_od_do (5.) (6.) ) (wartosc_od_do (-1.) (1.) ) = wartosc_od_do (5.) (-5.)) then 25 else 0;;
let test27 = if (podzielic (wartosc_od_do (5.) (6.) ) (wartosc_od_do (0.) (5.) ) = wartosc_od_do (1.) (infinity)) then 26 else 0;;
let test28 = if (podzielic (wartosc_od_do (5.) (6.) ) (wartosc_od_do (-7.) (0.) ) = wartosc_od_do (neg_infinity) ((-5.)/.7.)) then 27 else 0;;  (*niedokladnosc floatow*)
let test29 = if (podzielic (wartosc_od_do (5.) (0.) ) (wartosc_od_do (-7.) (-22.) ) = wartosc_od_do (neg_infinity) (infinity)) then 28 else 0;;
*)

(*
let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <1, 1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, -2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s                          (* (-inf, 0) *)
;;

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
(* assert ((sr_wartosc c) == nan) *) (*OCaml ma kilka różnych rodzajów nan, nawet jak funkcja zwraca nan to ten test się krzeczy, 30 minut debugowania*)
assert (compare (sr_wartosc c) nan = 0); (*compare a b = {1 gdy a>b, 0 gdy a=b, -1 gdy a<b}, w przeciwienstwie do "==" "compare" ogarnia porownywanie nan*)
assert (in_wartosc c 0. = false);
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert ((compare nan (min_wartosc i), compare nan (sr_wartosc i), compare nan (max_wartosc i)) = (0, 0, 0));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, compare (sr_wartosc p) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc q, max_wartosc q, compare (sr_wartosc q) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc r, max_wartosc r, compare (sr_wartosc r) nan) = (neg_infinity, infinity, 0));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));
*)


(*
razy (wartosc_od_do 4. 8.) (wartosc_od_do (-1.) 1.);;
razy (wartosc_od_do 4. 8.) (wartosc_od_do 2. 4.);;
min_wartosc (podzielic (wartosc_od_do (-7.) infinity) (wartosc_od_do 5. infinity));;
razy (wartosc_od_do (-5.000000) (-2.000000)) (wartosc_od_do (-5.000000) (-2.000000));;
razy (wartosc_od_do (-5.000000) (-2.000000)) (wartosc_od_do neg_infinity infinity);;
podzielic (wartosc_od_do 5. 11.) (wartosc_od_do (-1.) 1.);;
wartosc_dokladnosc (-8.) 50.;;
razy (podzielic (wartosc_od_do 5. 11.) (wartosc_od_do (-1.) 1.)) (wartosc_dokladna 0.);;
razy (wartosc_od_do neg_infinity (-5.)) (wartosc_dokladna 0.);;
sr_wartosc(wartosc_od_do 10. infinity);;
*)


(*
let get_minV w = w.a;;
let get_maxV w = w.b;;

#use "tests/Test1.ml";;*)