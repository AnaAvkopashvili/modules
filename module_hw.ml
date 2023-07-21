module type Readable = sig
  type t
  type arg
  val begin_read : arg -> t
  val end_read : t -> unit
  val at_end : t -> bool
  val read_line : t -> (t * string)
  end 

  module ReadableString : Readable = struct
    type t = {
      lines: string list;
      current_line: int;
    }
    type arg = string
    let begin_read input = {
      lines = String.split_on_char '\n' input;
      current_line = 0;
    }
    let end_read _ = ()
    let at_end state = state.current_line >= List.length state.lines
    let read_line state =
      let line = List.nth state.lines state.current_line in
      let next_state = { state with current_line = state.current_line + 1 } in
      next_state, line
  end
  
  
  (*2*)

  module ReadableFile = struct 
    type t = {
      channel: in_channel;
      current_line: string option;
    }
    type arg = string
    let at_end file_state =
      match file_state.current_line with
      | None -> true
      | _ -> false
    let read_line file_state =
      match file_state.current_line with
      | Some line ->
          let new_line_opt = try Some (input_line file_state.channel)
            with End_of_file -> None in
          { file_state with current_line = new_line_opt }, line
      | None -> file_state, ""
    let begin_read file_name =
      let channel = open_in file_name in
      let current_line_opt = try Some (input_line channel)
        with End_of_file -> None in
      { channel; current_line = current_line_opt }
    let end_read file_state =
      close_in file_state.channel
  end

(*3*)
  module Reader (R : Readable) = struct 
    include R
    let read_all state =
      let rec read_lines state acc =
        if at_end state then
          (state, String.concat "\n" (List.rev acc))
        else
          let (next_state, line) = read_line state in
          read_lines next_state (line :: acc)
      in
      read_lines state []
  end