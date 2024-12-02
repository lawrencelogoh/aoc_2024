let input = "input.txt";;
let number = ref 0 ;;
let left_list = ref [];;
let right_list= ref [];;

(* Read input line by line *)
let read_file input =
  let ic = open_in input in
  try
    let rec read_line () =
      try
        let line = input_line ic in
        (* Split line and filter out empty lists *)
        let parts = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in

        (* Add to left and right lists *)
        match parts with
        | [left; right] ->
           left_list := [int_of_string left] @ !left_list;
           right_list := [int_of_string right] @ !right_list;
           
           read_line ();
        | _ ->             
        print_endline "Error";
        
      with End_of_file -> ()
    in
    read_line ();
    close_in ic;
    (* Sort left and right lists *)
    left_list := List.sort compare !left_list;
    right_list := List.sort compare !right_list;          
    
    (* Add all the absolute differences of the sorted lists *)
    
    List.iter2
      (fun left right ->
        number := !number + abs(left - right);
      )
      !left_list
      !right_list;
    
    print_int !number;
  with e ->
    close_in_noerr ic;
    raise e;;


read_file input;;
