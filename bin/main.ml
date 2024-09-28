open Process_tracker_lib

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
    print_string "Name of activity: ";
    flush stdout;
    let acti = read_cmd () in
    acti

let rec idActivity activities index =
    match activities with 
    | [] -> []
    | h::t -> (index, h) :: idActivity t (index + 1)

let select_distinct_activity () =
    let distinct_activities = Activity_repo.retrive_distinct_activities () in
    if List.is_empty distinct_activities then failwith "no saved activities" else
    let ided = idActivity distinct_activities 1 in
    List.iter (fun (i, a) -> print_endline ((string_of_int i) ^ " - " ^ a)) ided;
    let selectionOpt = read_cmd () in
    let selection = 
        match selectionOpt with 
        | None -> Either.left "expected a selection"
        | Some s -> begin match int_of_string_opt s with
            | None -> Either.left "not an option"
            | Some i -> Either.right i end in
    Either.fold 
        ~left:(fun l -> Either.left l)
        ~right:(fun r ->
            match (List.find_opt (fun x -> fst x == r) ided) with
                | None -> Either.left "selection not found"
                | Some s -> Either.right @@ snd s)
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
            let persisted = (Activity_repo.create_entry a (int_of_float @@ Unix.time ())) in
            print_endline "Enter to stop....";
            let _ = read_cmd () in
            let _ = Activity_repo.set_end_time (Int64.to_int persisted.id) in
            ()
            end
        end
    | 2 ->
        let found = select_distinct_activity () in
        Either.fold
            ~left:(fun l -> print_endline l)
            ~right:(fun r ->
                let ac = Activity_repo.create_entry r (int_of_float @@ Unix.time ()) in
                print_endline "Enter to stop....";
                let _ = read_cmd () in
                let _ = Activity_repo.set_end_time (Int64.to_int ac.id) in
                ())
            found
    | _ -> ()

let list_activities () = 
    let activity_selected_either = select_distinct_activity () in
    match activity_selected_either with
        | Either.Left e -> print_endline e
        | Either.Right activity_selected ->
            let activity_entries: Activity_repo.activity list = Activity_repo.get_activity_entries activity_selected in
            let toUi = List.fold_left (fun acc (a:Activity_repo.activity) -> 
                match a.end_time with
                | None -> failwith "acitivity is in process"
                | Some e -> 
                    let time = Int64.sub e a.start_time in
                    (Int64.add (fst acc) time, (Int64.to_string @@ time) ^ " secs\n" ^ (snd acc))) (Int64.zero, "") activity_entries in
            print_endline @@ snd toUi;
            print_endline @@ "Total: " ^ (Int64.to_string @@ fst toUi) ^ " secs\n"

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
    let _ = Activity_repo.create_tables () in
    repl ()
