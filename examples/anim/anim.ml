module Ease = Ease

type repeat =
    | Count of int
    | Infinite

type direction =
    | Forward
    | Mirror of direction
    | Backward

(* TODO - maybe give info about repeats *)
type reason =
    | Done
    | Canceled

type description = {
    delay : float;
    duration : float (* option *);
    repeat : repeat;
    repeat_delay : float;
    update : float -> unit;
    complete : (int -> reason -> unit) option;
    direction : direction option;
    ease : Ease.t option;
}

type anim =
    | Serial of bool (* true if children contain infinite *) * description * anim list
    | Parallel of bool (* true if children contain infinite *) * description * anim list
    | Leaf of description

(* TODO -
    type anim = Serial of bool * desc list
              | Parallel of bool * desc list
              | Leaf
    and desc = {
        ...
        kind : anim;
    }
*)

let invert_direction = function
    | Some (Mirror Backward) -> Some (Mirror Forward)
    | Some (Mirror Forward) -> Some (Mirror Backward)
    | Some Forward -> Some Backward
    | Some Backward -> Some Forward
    | None -> Some Backward
    | Some (Mirror (Mirror _)) -> assert false
;;

let get_repeat (desc : description) =
    match desc.repeat with
    | Infinite -> 1
    | (Count times) -> times
;;

let calc_duration (anim : anim) =
    match anim with
    | Leaf desc
    | Serial (_, desc, _)
    | Parallel (_, desc, _) -> 
        let repeat = float (get_repeat desc) in
        desc.delay +. (desc.duration +. desc.repeat_delay)*.repeat
;;

let rec invert_desc = function
    | Leaf ({direction; _} as r) -> 
        Leaf {r with direction = 
            invert_direction direction
        }
    | Serial (b, ({direction; _} as r), lst) ->
        let direction = invert_direction r.direction in
        Serial (b, {r with direction}
            , List.map invert_desc lst
        )
    | Parallel (b, ({direction; _} as r), lst) ->
        let direction = invert_direction r.direction in
        Parallel (b, {r with direction}
            , List.map invert_desc lst
        )
;;

let is_infinite = function
    | None -> false
    | Some Infinite -> true
    | Some _ -> false
;;

let rec has_infinite = function
    | Leaf { repeat = Infinite; _ } -> true
    | Leaf _ -> false
    | Parallel (_, { repeat = Infinite; _}, _) -> true
    | Serial (_, { repeat = Infinite; _ }, _) -> true
    | Serial (b, _, lst)
    | Parallel (b, _, lst) -> 
        if b then true
        else List.exists has_infinite lst
;;

let has_infinite_lst = 
    List.exists has_infinite

let filter_infinite_serial =
    let seen = ref false in
    List.filter (fun desc ->
        if !seen then false
        else (
            seen := has_infinite desc;
            true (* keep the first infinite *)
        )
    )
;;

let mk_desc
    ?delay
    ?ease
    ?complete
    ?repeat
    ?repeat_delay
    ?direction
    duration
    update
= {
    delay = Option.value ~default:0. delay;
    ease;
    repeat = Option.value ~default:(Count 1) repeat;
    repeat_delay = Option.value ~default:0. repeat_delay;
    direction;
    complete;
    update;
    duration;
}

let create
    ?delay
    ?ease
    ?complete
    ?repeat
    ?repeat_delay
    ?direction
    duration
    update
=
    Leaf (mk_desc ?delay ?ease ?complete ?repeat ?repeat_delay ?direction duration update)
;;

let propagate_direction lst = function
    | None -> lst
    | Some direction ->
        let rec replace = function
            | Leaf ({ direction=None; _ } as r) -> Leaf {r with direction=Some direction}
            | Serial (b, ({direction=None; _} as r), lst) ->
                Serial (b, {r with direction=Some direction},
                    List.map replace lst)
            | Parallel (b, ({direction=None; _} as r), lst) ->
                Parallel (b, {r with direction=Some direction},
                    List.map replace lst)
            | anim -> anim
        in
        List.map replace lst
;;

let serial 
    ?delay
    ?ease
    ?complete
    ?repeat
    ?repeat_delay
    ?direction
    (lst : anim list)
=
    let inf = has_infinite_lst lst in
    let lst = filter_infinite_serial lst in
    let duration = List.fold_left (fun total anim -> 
        total +. calc_duration anim
    ) 0. lst in
    let self = mk_desc ?delay ?ease ?complete ?repeat ?repeat_delay ?direction duration ignore in
    let lst = propagate_direction lst direction in
    Printf.printf "Start\n%!";
    List.iter (function
        | Leaf d -> Printf.printf " L %.2f (%.2f)\n%!" d.delay d.duration
        | Serial (_, d, _) -> Printf.printf " S %.2f (%.2f)\n%!" d.delay d.duration
        | Parallel (_, d, _) -> Printf.printf " P %.2f (%.2f)\n%!" d.delay d.duration
    ) lst;
    (* Propagate our repeat to children if not none *)
    
    print_endline "END";
    Serial (inf, self, lst)
;;

let parallel
    ?delay
    ?ease
    ?complete
    ?repeat
    ?repeat_delay
    ?direction
    (lst : anim list)
=
    let inf = has_infinite_lst lst in
    let duration = 
        List.fold_left (fun max_d anim -> 
            Float.max max_d (calc_duration anim)
        ) Float.min_float lst 
    in
    let lst = propagate_direction lst direction in
    let self = mk_desc ?delay ?ease ?complete ?repeat ?repeat_delay ?direction duration ignore in
    Parallel (inf, self, lst)
;;

module Driver = struct
    type concrete = {
        id : int;
        delay : float;
        duration : float;
        mutable elapsed : float;
        mutable repeat : repeat;
        mutable direction : direction;
        repeat_delay : float;
        ease :Ease.t;
        update : float -> unit;
        complete : int -> reason -> unit;
    }

    let anim_of_description id offset (d : description) : concrete = {
        id;
        delay = offset +. d.delay;
        duration = d.duration;
        elapsed = 0.;
        repeat = d.repeat;
        repeat_delay = d.repeat_delay;
        direction = Option.value ~default:Forward d.direction;
        ease = Option.value ~default:Ease.linear d.ease;
        update = d.update;
        complete = Option.value ~default:(fun _ _ -> 
            Printf.printf "DUMMY COMPLETE\n%!";
        ) d.complete;
    }

    type elt = {
        time : float; 
        concrete : concrete;
    }

    module PQ = MoreLabels.Set.Make(struct
        type t = elt
        let compare t1 t2 = 
            let c = Float.compare t1.time t2.time in
            if c = 0 then Int.compare t1.concrete.id t2.concrete.id
            else c
    end)

    type t = {
        mutable time : float;
        mutable queue : PQ.t;
        mutable active : concrete list;
        mutable next_id : int;
    }

    let create () = {
        time = 0.;
        queue = PQ.empty;
        active = [];
        next_id = 0;
    }

    let make_entry t c = {
        time = t +. c.delay;
        concrete=c;
    }

    let get_next_id t =
        let id = t.next_id in
        t.next_id <- t.next_id + 1;
        id
    ;;

    let enqueue_anim (t : t) (anim : concrete) =
        let entry = make_entry t.time anim in
        anim.elapsed <- 0.;
        t.queue <- PQ.add entry t.queue
    ;;

    let dir_str = function
        | Some Forward -> "FW"
        | Some Backward -> "FW"
        | Some (Mirror Forward) -> "MFW"
        | Some (Mirror Backward) -> "MBW"
        | Some _ -> "unknown"
        | None -> "None"
    ;;

    let add_basic (t : t) id offset desc =
        let anim = anim_of_description id offset desc in
        Printf.printf " ADD %.2f (%.2f) %s\n%!" anim.delay anim.duration (dir_str (Some anim.direction));
        enqueue_anim t anim
    ;;

    let get_end (desc : description) =
        let duration = desc.duration in
        desc.delay +. duration
    ;;

    let is_mirror = function
        | Serial (_, {direction = Some (Mirror _)}, _)
        | Parallel (_, {direction = Some (Mirror _)}, _) -> true
        | _ -> false
    ;;

    let has_repeat = function
        | Infinite -> true
        | (Count x) -> x >= 0
    ;;

    let replace_delay_with_repeat_delay = function
        | Leaf r -> Leaf { r with delay = r.repeat_delay }
        | Serial (b, r, l) -> Serial (b, {r with delay = r.repeat_delay}, l)
        | Parallel (b, r, l) -> Parallel (b, {r with delay = r.repeat_delay}, l)
    ;;

    let check_and_decr_repeat rep =
        match !rep with
        | Infinite -> true
        | Count x ->
            rep := Count (x-1);
            x > 1
    ;;

    let rec add (t : t) (id : int) (delay : float) (anim : anim) =
        match anim with
        | Leaf desc -> add_basic t id delay desc
        | Parallel (inf, desc, lst) ->
            let offset = delay +. desc.delay in
            (* We repeat if our child is not infinite or we are infinite *)
            List.iter (add t id offset) lst;
            if not inf && has_repeat desc.repeat then (
                let complete id reason =
                    Option.iter (fun c -> c id reason) desc.complete;
                    let requeue anim =
                        let anim = 
                            if is_mirror anim then               
                                invert_desc anim
                            else anim
                        in
                        let anim = replace_delay_with_repeat_delay anim in
                        add t id 0. anim 
                    in
                    match desc.repeat with
                    | Infinite -> requeue anim
                    | Count x ->
                        if x > 1 then (
                            requeue (Parallel (inf, {desc with repeat = Count (x-1)}, lst))
                        ) 
                in
                let dummy = 
                    mk_desc 
                    ~delay:(get_end desc)
                    ~complete:complete
                    0.
                    ignore
                in
                add t id offset (Leaf dummy)
            );
        | Serial (inf, desc, lst) ->
            let offset = delay +. desc.delay in
            (* We repeat if our child is not infinite or we are infinite *)
            (* Can calculate proper offset here *)
            begin match desc.direction with
            | None | Some Forward | Some (Mirror Forward) ->
                List.fold_left (fun offset anim ->
                    add t id offset anim;
                    offset +. calc_duration anim
                ) offset lst |> ignore

            | Some Backward | Some (Mirror Backward) ->
                List.fold_right (fun anim offset ->
                    add t id offset anim;
                    offset +. calc_duration anim
                ) lst offset |> ignore

            | _ -> assert false
            end;

            if not inf && has_repeat desc.repeat then (
                let complete id reason =
                    Option.iter (fun c -> c id reason) desc.complete;
                    let requeue anim =
                        let anim = 
                            if is_mirror anim then (
                                invert_desc anim
                            ) else anim
                        in
                        let anim = replace_delay_with_repeat_delay anim in
                        add t id 0. anim 
                    in
                    match desc.repeat with
                    | Infinite -> requeue anim
                    | Count x ->
                        if x > 1 then (
                            requeue (Serial (inf, {desc with repeat = Count (x-1)}, lst))
                        ) 
                in
                let dummy = 
                    mk_desc 
                    ~delay:(get_end desc)
                    ~complete:complete
                    0.
                    ignore
                in
                add t id offset (Leaf dummy)
            );
    ;;

    let start (t : t) (anim : anim) : int =
        let id = get_next_id t in
        add t id 0. anim;
        id
    ;;

    let start_ (t : t) (anim : anim) : unit =
        start t anim |> ignore

    let rec check_queue (t : t) (dt : float) =
        match PQ.min_elt_opt t.queue with
        | None -> ()
        | Some elem ->
            if elem.time <= (t.time +. dt) then (
                let c = elem.concrete in
                (* no +. dt because we add it later *)
                c.elapsed <- t.time -. elem.time;
                (* should go after to do updates in the proper order *)
                t.active <- (*c :: t.active;*) List.append t.active [c];
                t.queue <- PQ.remove elem t.queue;
                check_queue t dt
            )
    ;;

    let swap_mirror (anim : concrete) =
        match anim.direction with
        | Mirror Backward -> anim.direction <- Mirror Forward
        | Mirror Forward -> anim.direction <- Mirror Backward
        | _ -> ()
    ;;

    let check_repeat_and_requeue (t : t) (anim : concrete) =
        swap_mirror anim;
        match anim.repeat with
        | Infinite -> enqueue_anim t anim
        | Count times ->
            if times > 1 then ( 
                anim.repeat <- Count (times-1);
                enqueue_anim t anim
            )
    ;;

    let update_animation (t : t) (dt : float) (anim : concrete) =
        anim.elapsed <- Float.min (anim.elapsed +. dt) anim.duration;
        let l = anim.ease (anim.elapsed /. anim.duration) in
        let l =
            if anim.direction = Backward
                || (anim.direction = Mirror Backward)
            then (1. -. l)
            else l
        in
        anim.update l;
        if anim.elapsed >= anim.duration then (
            anim.complete anim.id Done;
            check_repeat_and_requeue t anim;
            None
        ) else (
            Some anim
        )
    ;;

    let is_list_empty = function
        | [] -> true
        | _ -> false
    ;;

    let is_empty (t : t) =
        is_list_empty t.active && PQ.is_empty t.queue
    ;;

    let tick (t : t) (dt : float) =
        check_queue t dt;
        t.time <- t.time +. dt;
        t.active <-
            List.filter_map (fun (anim : concrete) ->
                update_animation t dt anim
            ) t.active
        ;
    ;;

    let check_if_empty_and_reset (t : t) =
        if is_empty t then (
            (* Maybe don't set this to 0? if things depend
               on a fixed timestamp, this could cause jumps.
               as in, always add 0.016, then cancel_all, we're
               now at 0 instead of some remainder
             *)
            t.time <- 0.;
            t.next_id <- 0;
        )
    ;;

    let cancel (t : t) (id : int) =
        t.active <- List.filter (fun anim -> 
            if anim.id = id then (
                anim.complete anim.id Canceled;
                false
            ) else true
        ) t.active;
        t.queue <- PQ.filter (fun elem -> 
            if elem.concrete.id = id then (
                elem.concrete.complete id Canceled;
                false
            ) else true
        ) t.queue;
        check_if_empty_and_reset t;
    ;;

    let cancel_all (t : t) =
        t.time <- 0.;
        t.next_id <- 0;
        List.iter (fun anim -> anim.complete anim.id Canceled) t.active;
        PQ.iter (fun elem -> elem.concrete.complete elem.concrete.id Canceled) t.queue;
        t.active <- [];
        t.queue <- PQ.empty;
    ;;
end

let lerp a b t =
    (a *. (1.0 -. t)) +. (b *. t)
;;
