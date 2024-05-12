open Base

type t  = { x: float; y: float; z: float }

let create x y z = { x; y; z }

let (+|) v1 v2 = {
    x = v1.x +. v2.x; 
    y = v1.y +. v2.y; 
    z = v1.z +. v2.z
}

let (-|) v1 v2 = {
    x = v1.x -. v2.x; 
    y = v1.y -. v2.y; 
    z = v1.z -. v2.z
}

let ( *|) v1 s = {
    x = v1.x *. s; 
    y = v1.y *. s; 
    z = v1.z *. s
}

let ( /|) v1 s = {
    x = v1.x /. s; 
    y = v1.y /. s; 
    z = v1.z /. s
}

let dot v1 v2 = 
    v1.x *. v2.x +. 
    v1.y *. v2.y +. 
    v1.z *. v2.z

let length_square v1 = 
    dot v1 v1

let length v1 = 
    Float.sqrt (length_square v1)

let cross v1 v2 = {
    x = v1.y *. v2.z -. v1.z *. v2.y;
    y = v1.z *. v2.x -. v1.x *. v2.z;
    z = v1.x *. v2.y -. v1.y *. v2.x
}

let neg v1 = {
    x = (-.v1.x); 
    y = (-.v1.y); 
    z = (-.v1.z)
}

let unit v1 = 
  let l = (length v1) in {
    x = v1.x /. l;
    y = v1.y /. l;
    z = v1.z /. l;
}

let lerp v1 v2 t =
    (v1 *| (1. -. t)) +| (v2 *| t)
