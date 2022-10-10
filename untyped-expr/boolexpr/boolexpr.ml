let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s
;;

if (Array.length(Sys.argv) <> 2)
then failwith "Usage: dune exec ./eval.exe <file>"
else
  let s_exp = read_whole_file Sys.argv.(1)
  in print_string "Evaluating: "; print_string s_exp; print_endline "...";
     print_endline (string_of_bool (Interp.Main.interp s_exp))



