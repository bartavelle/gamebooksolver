open Model
open Names
let split c s = List.filter (fun x -> x <> "") (String.split_on_char c s)
let items s = List.map (fun kv -> match String.split_on_char ':' kv with [k;v] -> (item_of k, int_of_string v) | _ -> failwith kv) (split ',' s)
let rec pf = function XH -> 1.0 | XO p -> 2.0 *. pf p | XI p -> 2.0 *. pf p +. 1.0
let zf = function Z0 -> 0.0 | Zpos p -> pf p | Zneg p -> -. pf p
let qf (q:q) = zf q.qnum /. pf q.qden
let sh_items l = String.concat "," (List.sort compare (List.filter_map (fun (i,q) -> if q > 0 then Some (name_of i ^ ":" ^ string_of_int q) else None) l))
let sh_flags l = String.concat "," (List.sort compare (List.map (fun (f,_) -> flag_name f) l))
let body (s:stt) = Printf.sprintf "e:%d f:%s i:%s p:%s" s.curendurance (sh_flags s.flags) (sh_items s.items0) (sh_items s.previtems)
let show = function
  | Lost -> "L"
  | Won s -> "W " ^ body s
  | New s -> Printf.sprintf "c:%d %s" s.chapter (body s)
let () =
  let books = Array.map (fun (n,b) -> expand_evasions n b) [| (1,b1);(2,b2);(3,b3);(4,b4);(5,b5) |] in
  let ic = open_in Sys.argv.(1) in
  let n = ref 0 in
  try while true do
    let l = input_line ic in
    (match String.split_on_char '|' l with
     | [bk;cs;maxe;cur;ch;d;it;fl;pv] ->
       let bk = int_of_string bk in
       let s = mk_stt bk (int_of_string maxe) (int_of_string cur) (int_of_string cs)
           (List.map disc_of (split ',' d)) (int_of_string ch) (items it) (List.map flag_of (split ',' fl)) (items pv) in
       let opts = (try step books.(bk-1) s with Stack_overflow -> failwith ("overflow "^l)) in
       let so = List.map (fun o ->
           let h = Hashtbl.create 16 in
           List.iter (fun (ns,p) -> let k = show ns in
                       Hashtbl.replace h k (qf p +. (try Hashtbl.find h k with Not_found -> 0.0))) o;
           let kv = List.sort compare (Hashtbl.fold (fun k v acc -> if v > 1e-9 then (k,v)::acc else acc) h []) in
           String.concat ";" (List.map (fun (k,v) -> Printf.sprintf "%s=%.8f" k v) kv)) opts in
       Printf.printf "%d|%s\n" !n (String.concat " || " (List.sort compare so))
     | _ -> failwith l);
    incr n
  done with End_of_file -> ()
