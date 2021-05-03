open Graphics

let n = 256
and delta = 0.1

type point = { x : float; y : float; z : float }
type vector = { x : float; y : float; z : float }

let noir = {x=0.;y=0.;z=0.}
and blanc = {x=1.;y=1.;z=1.}

type sphere = {
    centre : point;
    r : float;
    c : vector;
    k : vector
  }

type source = {
  s : point;
  c : vector
}

type rayon = {
  start : point;
  dir : vector
  }

let inf = ({x = infinity; y = infinity; z = infinity} : point)

let sources = (ref [| {s = {x = 12.; y = -3. ; z = 3.}; c = {x = 1.; y = 0.; z = 0.}};  {s = {x = -.15.; y = -3. ; z = 15.}; c = {x = 0.; y = 1.; z = 0.}};  {s = {x = 1.; y = -4. ; z = 45.}; c = {x = 0.; y = 0.; z = 1.}} |] : source array ref)
let obj = (ref [| {centre = {x=0.;y=0.;z=5.}; r = 1.; c = {x=0.23;y=0.14;z=0.65}; k = {x=0.3;y=0.3;z=0.3}} |] : sphere array ref)

let acolor_to_color (c : vector) = (Graphics.rgb ((min (int_of_float (c.x)) 1) * 255) ((min (int_of_float (c.y)) 1) * 255) ((min (int_of_float c.z) 1) * 255))

let vector_of_point = ( fun a b ->
  { x = b.x -. a.x; y = b.y -. a.y; z = b.z -. a.z} : point -> point -> vector)

let pds (a : vector) (b : vector) =
  ((a.x *. b.x)) +. ((a.y *. b.y)) +. ((a.z *. b.z))

let hadamard (u :vector) (v : vector) =
  ({ x = u.x*.v.x; y = u.y*.v.y; z = u.z*.v.z } : vector)

let scalaire (s : float) (u : vector) =
  ({x = s *. u.x; y = s *. u.y; z = s *. u.z} : vector)

let sum (u : vector) (v : vector) =
  ({x = u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z} : vector)

let norme (a : vector) =
  sqrt (pds a a)

let normalized = (fun a ->
  let n = norme a in { x = a.x /. n; y = a.y /. n; z = a.z /. n } : vector -> vector)

let distance (a : point) (b : point) =
  norme (vector_of_point a b)

let intersection r s =
  (let ac = vector_of_point r.start s.centre in
  let a,b,c = pds r.dir r.dir, 2. *. pds ac r.dir, pds ac ac -. s.r *. s.r in
  let delta = b*.b -. 4.*.a*.c in
  if delta > 0. then
    let t1,t2 = (-.b +. sqrt delta)/.(2.*.a) , (-.b -. sqrt delta)/.(2.*.a) in
    if Float.abs t2 > Float.abs t1 then
      {x = r.start.x +. t1*.r.dir.x; y = r.start.y +. t1*.r.dir.y ;z = r.start.z +. t1*.r.dir.z}
    else
      {x = r.start.x +. t2*.r.dir.x; y = r.start.y +. t2*.r.dir.y ;z = r.start.z +. t2*.r.dir.z}
  else
    {x = infinity; y = infinity; z = infinity} : point)

let dir a b =
  normalized(vector_of_point a b)

let ray a b =
  { start = a; dir = dir a b }

let sp a b col k =
  { centre = a; r =  distance a b; c = col; k = k}

let au_dessus (s : sphere) (p : point) (src : source) =
  let inter = intersection (ray src.s p) s in
  if inter = p then true else false

let visible (obj : sphere array) (j : int) (p : point) (src : source) =
  if au_dessus obj.(j) p src then
    let rec aux pas_cache i =
      if i >= Array.length obj then pas_cache
      else if i = j then aux pas_cache (i+1)
      else if intersection (ray src.s p) obj.(i) <> inf then
        false
      else aux pas_cache (i+1)
    in aux true 0
  else false

let cosinus (u : vector) (v : vector) =
  pds (normalized u) (normalized v)

let couleur_diffusee (p:point) (src : source) (s:sphere) (n : vector) =
  (scalaire (cosinus n (ray src.s p).dir) (hadamard s.k src.c) : vector)

let grille i j =
  ({x = -.((float_of_int (n/2 - j + 1)) *. delta -. delta /. 2.); y = (float_of_int (n/2 - i + 1)) *. delta -. delta /. 2.; z = 0.} : point)

let rayon_ecran omega i j =
  ray omega (grille i j)

let interception (r : rayon) =
  let rec aux dist point indice i =
    if i >= Array.length !obj then point,indice
    else let inter = intersection r !obj.(i) in
         if inter <> inf then
           if dist > distance inter r.start then
             aux (distance inter r.start) inter i (i+1)
           else aux dist point indice (i+1)
         else aux dist point indice (i+1)
  in aux infinity inf (-1) 0;;

let couleur_diffusion (p : point) (j : int) =
  let n = normalized (vector_of_point !obj.(j).centre p) in
  let sources_visibles = (List.filter (fun src -> visible !obj j p src) (Array.to_list !sources)) in
  let rec aux s = function
    |[] -> s
    |t::q -> aux (sum s (couleur_diffusee p t !obj.(j) n)) q
  in aux noir sources_visibles

let lancer omega fond =
  let image = Array.make n (Array.make n fond) in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if (interception (rayon_ecran omega i j)) <> (inf,(-1)) then
        let p,indice = interception (rayon_ecran omega i j) in
        image.(i).(j) <- acolor_to_color (couleur_diffusion p indice)
    done;
  done;
  image;;

let omega = ({x = 0.; y = 0.; z = -4.} : point)
let fond = Graphics.rgb 156 42 39;;

Graphics.open_graph "";;

let image = Graphics.make_image (lancer omega fond);;
Graphics.draw_image image 0 0;;
