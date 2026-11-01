type 'a opt = OSome of 'a | ONone
(* same computation as cartwheel.rs; the rational type mirrors the extracted one *)
type positive = XI of positive | XO of positive | XH
type z = Z0 | Zpos of positive | Zneg of positive
type q = { qnum : z; qden : positive }
let rec pos_of_int n = if n = 1 then XH else if n land 1 = 1 then XI (pos_of_int (n lsr 1)) else XO (pos_of_int (n lsr 1))
let den = 1 lsl 40
let qc_of_float f : Obj.t =
  let n = int_of_float (Float.round (f *. float_of_int den)) in
  Obj.magic { qnum = (if n = 0 then Z0 else Zpos (pos_of_int n)); qden = pos_of_int den }
let cache = Hashtbl.create 32
let table target =
  match Hashtbl.find_opt cache target with
  | Some t -> t
  | None ->
    let sz = target + 8 in
    let m = ref (Array.make_matrix sz sz 0.0) in
    !m.(0).(0) <- 1.0;
    for c = 1 to target - 1 do
      !m.(c).(c-1) <- 0.7; !m.(c).(c+5) <- 0.2; !m.(c).(c+8) <- 0.1
    done;
    for c = target to target + 7 do !m.(c).(c) <- 1.0 done;
    for _ = 1 to 12 do
      let a = !m in
      let r = Array.make_matrix sz sz 0.0 in
      for y = 0 to sz-1 do for x = 0 to sz-1 do
        let s = ref 0.0 in
        for i = 0 to sz-1 do s := !s +. a.(y).(i) *. a.(i).(x) done;
        r.(y).(x) <- !s done done;
      m := r
    done;
    Hashtbl.add cache target !m; !m
let dist (money:int) (target:int) =
  if money >= target then Obj.magic [ (target, Obj.obj (qc_of_float 1.0)) ]
  else begin
    let t = table target in
    let l = ref [] in
    for e = target + 7 downto 0 do
      let p = t.(money).(e) in
      if p > 1e-12 then l := (e, Obj.obj (qc_of_float p)) :: !l
    done;
    Obj.magic !l
  end
