type 'a return = { return : 'b . 'a -> 'b }

let with_return (type a) f =
  let module M = struct
    (* Raised to indicate ~return was called.  Local so that the exception is tied to a
       particular call of [with_return]. *)
    exception Return of a
  end
  in
  let is_alive = ref true in
  let return a =
    if not !is_alive
    then failwith "use of [return] from a [with_return] that already returned";
    raise (M.Return a)
  in
  try
    let a = f { return } in
    is_alive := false;
    a
  with
  | exn ->
    is_alive := false;
    (match exn with
     | M.Return a -> a
     | _ -> raise exn)

let some_exn = function
   | None -> failwith "Expected some"
   | Some v -> v
;;
