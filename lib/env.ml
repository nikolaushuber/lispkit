type 'a t = (string * 'a option ref) list

let lookup env name = 
  try 
    match !(List.assoc name env) with 
    | None -> failwith "Trying to access pending element in env" 
    | Some t -> t 
  with 
    _ -> failwith ("Unknown reference: " ^ name)

let bind env name value = (name, ref (Some value)) :: env 

let bind_empty env name = (name, ref None) :: env

let set env name value = 
  if List.mem_assoc name env then 
    (List.assoc name env) := Some value 
  else 
    failwith ("Unknown reference: " ^ name)   