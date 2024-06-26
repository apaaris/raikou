open Vec3

type t = {center: Vec3.t; radius: float}
type hit_record = {t: float; p: Vec3.t; normal: Vec3.t}

let create center radius = {center; radius}

let hit (r: Ray.t) ({center; radius}: t): hit_record option =
  let t_min = 0.
  and t_max = max_float in
  let oc = r.origin -| center in
  let a = Vec3.dot r.direction r.direction
  and half_b  = Vec3.dot oc r.direction
  and c = (Vec3.dot oc oc) -. radius *. radius in
  
  let hit_record_from_t t: hit_record option = 
    if (t < t_max && t > t_min) then
      let p = Ray.at t r in
        Some {t; p; normal = ((p -| center) /| radius)}
    else
      None
    in
     let quater_discriminant = half_b *. half_b -. a *. c in
       if quater_discriminant > 0. then
         let root = sqrt quater_discriminant in
          let t1 = (-.half_b -. root)/. a in
          match hit_record_from_t t1 with
          | Some hit_record -> Some hit_record
          | None ->
              let t2 = (-.half_b +. root)/. a in
              hit_record_from_t t2
    else
      None
