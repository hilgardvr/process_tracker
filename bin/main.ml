open Process_tracker_lib

let db = Sqlite3.db_open "process_tracker.db"


let start_message = " 1. Track a activity\n 2. List tracked activities\n> "

let print_flush str = Printf.printf "flushnig %s %!"str

let parse_command cmd =
    match cmd with 
    | Some c -> begin
        match int_of_string_opt c with
            | Some c' -> c'
            | None -> 0
        end
    | None -> 0

let read_cmd () = In_channel.input_line stdin

let add_new_activity db = 
    print_string "Name of activity: ";
    flush stdout;
    let acti = read_cmd () in
    match acti with
    | None -> None
    | Some a -> Some (Activity_repo.create_entry db a (int_of_float @@ Unix.time ()))

let track_activity db =
    let message = " 1. Add a new activity\n 2. Track existing activity\n>" in
    print_string message;
    flush stdout;
    let cmd_str = read_cmd () in
    let cmd = parse_command cmd_str in
    match cmd with
    | 0 -> ()
    | 1 -> begin
        let ac = add_new_activity db in 
        match ac with 
        | None -> ()
        | Some a -> begin
            print_endline "Enter to stop....";
            let _ = read_cmd () in
            let _ = Activity_repo.set_end_time db (Int64.to_int a.id) in
            ()
            end
        end
    | 2 -> 
        let activities = Activity_repo.retrive_distinct_activities db in
        List.iter (fun x -> print_endline x) activities
    | _ -> ()


let rec repl db =
    print_string start_message;
    flush stdout;
    let cmd_str = read_cmd () in
    let cmd = parse_command cmd_str in
    match cmd with
    | 0 -> ()
    | 1 -> 
        track_activity db;
        repl db
    | _ -> ()
(*
    let ret = Activity_repo.retrive_entry db in
    print_endline (Activity_repo.activity_to_string ret);
*)

let () = 
    let db = db in
    let _ = Activity_repo.create_tables db in
    repl db
