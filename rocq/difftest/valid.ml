(* Checks on the extracted model what ValidState says: from valid states, each outcome of each option must keep each
   clause, the probabilities of an option must sum to 1, and a chapter must offer an option. See valid.sh *)
open Model
open Names
let rec pf = function XH -> 1.0 | XO p -> 2.0 *. pf p | XI p -> 2.0 *. pf p +. 1.0
let zf = function Z0 -> 0.0 | Zpos p -> pf p | Zneg p -> -. pf p
let qf (q:q) = zf q.qnum /. pf q.qden
let books = Array.map (fun (n,b) -> expand_evasions n b) [| (1,b1);(2,b2);(3,b3);(4,b4);(5,b5) |]
let exists_ch bk c = match find_chapter c books.(bk-1) with Cw.OSome _ -> true | _ -> false
let idx i = let rec g n = function [] -> failwith "idx" | x::q -> if x = i then n else g (n+1) q in g 0 all_items
let rec sorted = function a::(b::_ as q) -> idx (fst a) < idx (fst b) && sorted q | _ -> true
let slot_is sl i = item_slot i = sl
let qty i l = try List.assoc i l with Not_found -> 0
let bp_count l = List.fold_left (fun a (i,q) -> if slot_is SBackpack i then a+q else a) 0 l
let wp_count l = List.fold_left (fun a (i,q) -> if slot_is SWeapon i then a+q else a) 0 l
let bp_used bk l = List.fold_left (fun a (i,q) -> if slot_is SBackpack i then a + q * backpack_slots bk i else a) 0 l
let stack i = (i = Gold || i = Meal || i = Laumspur)
let single l = List.for_all (fun (i,q) -> stack i || q <= 1) l
let bag l = sorted l && List.for_all (fun (_,q) -> q > 0) l
let nohelm l = not (qty Helmet l > 0 && qty (GenSpecial S10) l > 0)
let nodiscbp l = qty Backpack l > 0 || bp_count l = 0
let ndisc s = List.length s.disciplines
let nws s = List.length (List.filter (fun (d,_) -> match d with WeaponSkill _ -> true | _ -> false) s.disciplines)
(* The clauses of ValidState that can change while playing, and that a chapter exists, which ValidState can't say *)
let clauses : (string * (stt -> bool)) list = [
 "cur_pos", (fun s -> s.curendurance > 0);
 "cur_le_maxhp", (fun s -> s.curendurance <= max_hp s);
 "gold_le_50", (fun s -> sitems Gold s <= 50);
  "weapons_le_2", (fun s -> wp_count s.items0 <= 2);
 "items_bag", (fun s -> bag s.items0);
 "prev_bag", (fun s -> bag s.previtems);
 "prev_gold_le_50", (fun s -> qty Gold s.previtems <= 50);
  "prev_weapons_le_2", (fun s -> wp_count s.previtems <= 2);
 "backpack_le_8", (fun s -> bp_used s.book s.items0 <= 8);
 "prev_backpack_le_8", (fun s -> bp_used s.book s.previtems <= 8);
 "single_nonstack", (fun s -> single s.items0);
 "prev_single_nonstack", (fun s -> single s.previtems);
 "contents_need_backpack", (fun s -> nodiscbp s.items0);
 "prev_contents_need_backpack", (fun s -> nodiscbp s.previtems);
 "not_two_helmets", (fun s -> nohelm s.items0);
 "prev_not_two_helmets", (fun s -> nohelm s.previtems);
 "chapter_exists", (fun s -> exists_ch s.book s.chapter);
]
let viol = Hashtbl.create 64
let note name bk c s0 s1 =
  let k = (name,bk,c) in
  if not (Hashtbl.mem viol k) then Hashtbl.add viol k (s0,s1,ref 0);
  let (_,_,r) = Hashtbl.find viol k in incr r
let fixed (a:stt) (b:stt) = a.book=b.book && a.maxendurance=b.maxendurance && a.sk=b.sk && a.disciplines=b.disciplines
let check s0 s1 =
  List.iter (fun (n,p) -> if p s0 && not (p s1) then note n s0.book s0.chapter s0 s1) clauses;
  if not (fixed s0 s1) then note "fixed_fields" s0.book s0.chapter s0 s1
let badsum = Hashtbl.create 16 and empty = Hashtbl.create 16
let sh l = String.concat "," (List.map (fun (i,q) -> name_of i ^ ":" ^ string_of_int q) l)
let show (s:stt) = Printf.sprintf "c%d e%d/%d i[%s] p[%s]" s.chapter s.curendurance (max_hp s) (sh s.items0) (sh s.previtems)
let split c s = List.filter (fun x -> x <> "") (String.split_on_char c s)
let items s = List.map (fun kv -> match String.split_on_char ':' kv with [k;v] -> (item_of k, int_of_string v) | _ -> failwith kv) (split ',' s)
let pick o =
  let r = ref (Random.float 1.0) and res = ref [] in
  List.iter (fun (ns,p) -> if !res = [] then begin r := !r -. qf p; if !r <= 0.0 then res := [ns] end) o;
  match !res with [x] -> [x] | _ -> (match List.rev o with (x,_)::_ -> [x] | [] -> [])
let states = ref 0
let expand (s:stt) =
  incr states;
  let opts = step books.(s.book-1) s in
  if opts = [] && exists_ch s.book s.chapter then Hashtbl.replace empty (s.book,s.chapter) (show s);
  List.iter (fun o ->
    let t = List.fold_left (fun a (_,p) -> a +. qf p) 0.0 o in
    if abs_float (t -. 1.0) > 1e-9 then Hashtbl.replace badsum (s.book,s.chapter) (t, show s);
    List.iter (fun (ns,_) -> match ns with Lost -> () | Won x | New x -> check s x) o) opts;
  opts
let rec walk s d =
  if d > 0 then
    match expand s with
    | [] -> ()
    | opts -> (match pick (List.nth opts (Random.int (List.length opts))) with
              | [New x] -> walk x (d-1) | _ -> ())
let () =
  Random.init 7;
  let ic = open_in Sys.argv.(1) in
  (try while true do
    let l = input_line ic in
    (match String.split_on_char '|' l with
     | [mode;bk;cs;maxe;cur;ch;d;it;fl;pv] ->
       let s = mk_stt (int_of_string bk) (int_of_string maxe) (int_of_string cur) (int_of_string cs)
         (List.map disc_of (split ',' d)) (int_of_string ch) (items it) (List.map flag_of (split ',' fl)) (items pv) in
       if mode = "W" then walk s 2000 else ignore (expand s)
     | _ -> failwith l)
  done with End_of_file -> ());
  Printf.printf "states expanded %d\n" !states;
  let sum = Hashtbl.create 32 in
  Hashtbl.iter (fun (n,bk,c) (_,_,r) ->
    let (a,b,l) = try Hashtbl.find sum n with Not_found -> (0,0,[]) in
    Hashtbl.replace sum n (a+ !r, b+1, (bk,c)::l)) viol;
  List.iter (fun (n,_) -> if not (Hashtbl.mem sum n) then Printf.printf "OK   %s\n" n) clauses;
  Hashtbl.iter (fun n (a,b,l) ->
    let l = List.sort compare l in
    Printf.printf "FAIL %s: %d times, %d chapters: %s\n" n a b
      (String.concat " " (List.filteri (fun i _ -> i < 14) (List.map (fun (b,c) -> Printf.sprintf "B%dc%d" b c) l)));
    let (bk,c) = List.hd l in let (s0,s1,_) = Hashtbl.find viol (n,bk,c) in
    Printf.printf "     e.g. %s\n       -> %s\n" (show s0) (show s1)) sum;
  Printf.printf "probabilities not summing to 1: %d chapters\n" (Hashtbl.length badsum);
  Hashtbl.iter (fun (b,c) (t,s) -> Printf.printf "  B%dc%d sum=%f %s\n" b c t s) badsum;
  Printf.printf "existing chapter with no option: %d\n" (Hashtbl.length empty);
  let e = Hashtbl.fold (fun k v a -> (k,v)::a) empty [] |> List.sort compare in
  List.iteri (fun i ((b,c),s) -> if i < 12 then Printf.printf "  B%dc%d %s\n" b c s) e;
  if Hashtbl.length viol + Hashtbl.length badsum + Hashtbl.length empty > 0 then exit 1
