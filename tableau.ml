open! Batteries
module List = struct include List include List.Exceptionless end

(*
  prenom, nom, <liste des passages : note, commentaire>
*)

let separator = '|'
let file = "eleves.csv"

type eleve = {
  prenom: string;
  nom: string;
  passages: (float * string) list;
}

let rec collect_passages = function
  | note :: commentaire :: xs ->
    (float_of_string note, commentaire) :: collect_passages xs
  | [] -> []
  | [_] -> assert false

let dump_passages passages =
  List.fold_right (fun (note, com) acc ->
      string_of_float note :: com :: acc
    ) passages []

let eleve_of_line = function
  | prenom :: nom :: passages ->
    { prenom; nom; passages = collect_passages passages }
  | _ -> assert false

let line_of_eleve eleve =
  eleve.prenom :: eleve.nom :: (dump_passages eleve.passages)

let load_eleves () : eleve list =
  Csv.load ~separator file |> List.map eleve_of_line

let write_eleves eleves =
  Csv.save ~separator file (List.map line_of_eleve eleves)

let pick abs eleves =
  let open Option.Infix in
  List.filter (neg (flip List.mem abs)) eleves
  |> List.group (fun e1 e2 -> compare (List.length e1.passages) (List.length e2.passages))
  |> List.hd
  >>= (List.enum
       %> Random.shuffle
       %> Array.to_list
       %> List.hd)

let rec ask_yesno msg =
  Printf.printf (msg ^^ " [y/n]%!");
  let ret = read_line () in
  match ret with
  | "o" | "O" | "y" | "Y" -> true
  | "n" | "N" -> false
  | _ -> ask_yesno msg

let rec read_float_retry () =
  try read_float () with
    Failure s -> print_endline ("Failure: " ^ s); read_float_retry ()

let add_passage eleve =
  Printf.printf "note ? %!";
  let note = read_float_retry () in
  Printf.printf "commentaire ? %!";
  let com = read_line () in
  { eleve with passages = (note, com) :: eleve.passages }

let max_passages eleves =
  List.fold_left
    (fun mx eleve -> max mx (List.length eleve.passages))
    0 eleves

let update_eleves eleves eleve =
  List.map (fun eleve' ->
      if eleve'.prenom = eleve.prenom && eleve'.nom = eleve.nom then eleve
      else eleve'
    ) eleves

let main () =
  let rec loop eleves abs =
    match pick abs eleves with
    | None -> ()
    | Some eleve ->
      Printf.printf "-> %s %s\n%!" eleve.prenom eleve.nom;
      if ask_yesno "présent ?" then (
        let eleves' = update_eleves eleves (add_passage eleve) in
        write_eleves eleves';
        loop eleves' abs)
      else (
        Printf.printf "%i passage(s) sur %i\n%!"
          (List.length eleve.passages)
          (max_passages eleves);
        loop eleves (eleve :: abs))
  in
  loop (load_eleves ()) []

let () =
  Random.self_init ();
  main()
