open Process_tracker_lib.Activity_repo

let parse_command cmd =
    match cmd with 
    | Some c -> begin
        match int_of_string_opt c with
            | Some c' -> c'
            | None -> 0
        end
    | None -> 0

let read_cmd () = In_channel.input_line stdin

let get_activity () =
    print_string "Name of new activity(empty to cancel): ";
    flush stdout;
    let acti = read_cmd () in
    acti

let rec id_activity activities index =
    match activities with 
    | [] -> []
    | h::t -> (index, h) :: id_activity t (index + 1)

let select_distinct_activity () =
    let distinct_activities = ActivityRepo.get_distinct_activities () in
    if List.is_empty distinct_activities then Either.left "no saved activities" else
    let ided = id_activity distinct_activities 1 in
    List.iter (fun (i, a) -> print_endline ((string_of_int i) ^ " - " ^ a)) ided;
    let selectionOpt = read_cmd () in
    let selection =
        match selectionOpt with 
        | None -> Either.left "expected a selection"
        | Some s -> begin match int_of_string_opt s with
            | None -> Either.left "not an option"
            | Some i -> Either.right i end in
    Either.map_right
        (fun r ->
            match (List.find_opt (fun x -> fst x == r) ided) with
                | None -> "selection not found"
                | Some s -> snd s) 
        selection

let track_activity () =
    let message = " 1. Add a new activity\n 2. Track existing activity\n>" in
    print_string message;
    flush stdout;
    let cmd_str = read_cmd () in
    let cmd = parse_command cmd_str in
    match cmd with
    | 0 -> ()
    | 1 -> begin
        let read_ac = get_activity () in
        match read_ac with
        | None -> print_endline "didn't get an activity"
        | Some "" -> print_endline "empty activity, not persisting"
        | Some a -> begin
            let persisted = (ActivityRepo.create_entry a (int_of_float @@ Unix.time ())) in
            print_endline "Enter to stop....";
            let _ = read_cmd () in
            let _ = ActivityRepo.set_end_time (Int64.to_int persisted.id) in
            ()
            end
        end
    | 2 ->
        let found = select_distinct_activity () in
        Either.fold
            ~left:(fun l -> print_endline l)
            ~right:(fun r ->
                let ac = ActivityRepo.create_entry r (int_of_float @@ Unix.time ()) in
                print_endline "Enter to stop....";
                let _ = read_cmd () in
                let _ = ActivityRepo.set_end_time (Int64.to_int ac.id) in
                ())
            found
    | _ -> ()

let to_date_time_string ts = 
    let gm = Unix.gmtime @@ ts in
    Printf.sprintf "%d-%d-%d %d:%d" (gm.tm_year + 1900) gm.tm_mon gm.tm_mday gm.tm_hour gm.tm_min

let seconds_to_minutes secs =
    let mins = secs / 60 in
    let rem = secs mod 60 in
    Printf.sprintf "%d:%d" mins rem

let list_activities () = 
    let activity_selected_either = select_distinct_activity () in
    match activity_selected_either with
        | Either.Left e -> print_endline e
        | Either.Right activity_selected ->
            let activity_entries: ActivityRepo.activity list = ActivityRepo.get_activity_entries activity_selected in
            let toUi = List.fold_left (fun acc (a:ActivityRepo.activity) -> 
                match a.end_time with
                | None -> failwith "acitivity is in process"
                | Some e -> 
                    let time = Int64.sub e a.start_time in
                    (Int64.add (fst acc) time, (to_date_time_string @@ Int64.to_float a.start_time) ^ "  duration: " ^ (seconds_to_minutes @@ Int64.to_int time) ^ " mins\n" ^ (snd acc))) (Int64.zero, "") activity_entries in
            print_endline @@ snd toUi;
            print_endline @@ "Total: " ^ (seconds_to_minutes @@ Int64.to_int @@ fst toUi) ^ " secs\n"

let rec repl () =
    let start_message = " 1. Track a activity\n 2. List tracked activities\n 0. Quit\n> " in
    print_string start_message;
    flush stdout;
    let cmd_str = read_cmd () in
    let cmd = parse_command cmd_str in
    match cmd with
    | 0 -> ()
    | 1 -> 
        track_activity ();
        repl ()
    | 2 -> 
        list_activities ();
        repl ()
    | _ -> ()

let () = 
    let _ = ActivityRepo.create_tables () in
    repl ()
