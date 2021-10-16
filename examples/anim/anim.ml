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

type kind = Serial of bool * anim list
          | Parallel of bool * anim list
          | Leaf

and anim = {
    delay : float;
    duration : float (* option *);
    repeat : repeat;
    repeat_delay : float;
    update : float -> unit;
    complete : (int -> reason -> unit) option;
    direction : direction option;
    kind : kind;
    ease : Ease.t option;
}

let invert_direction = function
    | Some (Mirror Backward) -> Some (Mirror Forward)
    | Some (Mirror Forward) -> Some (Mirror Backward)
    | Some Forward -> Some Backward
    | Some Backward -> Some Forward
    | None -> Some Backward
    | Some (Mirror (Mirror _)) -> assert false
;;

let get_repeat (desc : anim) =
    match desc.repeat with
    | Infinite -> 1
    | (Count times) -> times
;;

let calc_duration (desc : anim) =
    let repeat = float (get_repeat desc) in
    desc.delay +. (desc.duration +. desc.repeat_delay)*.repeat
;;

let rec invert_desc desc = 
    let kind = 
        match desc.kind with
        | Leaf -> Leaf
        | Serial (b, lst) ->
            Serial (b, List.map invert_desc lst)
        | Parallel (b, lst) ->
            Parallel (b, List.map invert_desc lst)
    in
    { desc with 
        kind; 
        direction = invert_direction desc.direction
    }
;;

let is_infinite = function
    | None -> false
    | Some Infinite -> true
    | Some _ -> false
;;

let rec has_infinite desc =
    if desc.repeat = Infinite then true
    else (
        match desc.kind with
        | Leaf -> false
        | Serial (_, lst)
        | Parallel (_, lst) ->
            List.exists has_infinite lst
    )
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
    kind
= {
    delay = Option.value ~default:0. delay;
    ease;
    repeat = Option.value ~default:(Count 1) repeat;
    repeat_delay = Option.value ~default:0. repeat_delay;
    direction;
    kind;
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
    mk_desc ?delay ?ease ?complete ?repeat ?repeat_delay ?direction duration update Leaf
;;

let propagate_direction lst = function
    | None -> lst
    | Some direction ->
        let rec replace = function
            | { direction=None; _ } as r -> 
                {r with 
                    direction=Some direction;
                    kind = match r.kind with
                         | Leaf -> Leaf
                         | Serial (b, lst) -> Serial (b, List.map replace lst)
                         | Parallel (b, lst) -> Parallel (b, List.map replace lst)
                }
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
    (* Propagate our repeat to children if not none *)
    let lst = propagate_direction lst direction in
    mk_desc ?delay ?ease ?complete ?repeat ?repeat_delay ?direction duration ignore (Serial (inf, lst))
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
    mk_desc ?delay ?ease ?complete ?repeat ?repeat_delay ?direction duration ignore (Parallel (inf, lst))
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

    let anim_of_description id offset (d : anim) : concrete = {
        id;
        delay = offset +. d.delay;
        duration = d.duration;
        elapsed = 0.;
        repeat = d.repeat;
        repeat_delay = d.repeat_delay;
        direction = Option.value ~default:Forward d.direction;
        ease = Option.value ~default:Ease.linear d.ease;
        update = d.update;
        complete = Option.value ~default:(fun _ _ -> ()) d.complete;
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
        enqueue_anim t anim
    ;;

    let get_end (desc : anim) =
        let duration = desc.duration in
        desc.delay +. duration
    ;;

    let is_mirror : anim -> bool = function
        | {direction = Some (Mirror _)} -> true
        | _ -> false
    ;;

    let has_repeat = function
        | Infinite -> true
        | (Count x) -> x >= 0
    ;;

    let replace_delay_with_repeat_delay (desc : anim) = 
        { desc with delay = desc.repeat_delay }
    ;;

    let check_and_decr_repeat rep =
        match !rep with
        | Infinite -> true
        | Count x ->
            rep := Count (x-1);
            x > 1
    ;;

    let rec add (t : t) (id : int) (delay : float) (anim : anim) =
        match anim.kind with
        | Leaf -> add_basic t id delay anim
        | Parallel (inf, lst) ->
            let offset = delay +. anim.delay in
            (* We repeat if our child is not infinite or we are infinite *)
            List.iter (add t id offset) lst;
            if not inf && has_repeat anim.repeat then (
                let complete id reason =
                    Option.iter (fun c -> c id reason) anim.complete;
                    let requeue anim =
                        let anim = 
                            if is_mirror anim then               
                                invert_desc anim
                            else anim
                        in
                        let anim = replace_delay_with_repeat_delay anim in
                        add t id 0. anim 
                    in
                    match anim.repeat with
                    | Infinite -> requeue anim
                    | Count x ->
                        if x > 1 then (
                            requeue {anim with repeat = Count (x-1)}
                            (*(Parallel (inf, {desc with repeat = Count (x-1)}, lst))*)
                        ) 
                in
                let dummy = 
                    mk_desc 
                    ~delay:(get_end anim)
                    ~complete:complete
                    0.
                    ignore
                    Leaf
                in
                add t id offset dummy
            );
        | Serial (inf, lst) ->
            let offset = delay +. anim.delay in
            (* We repeat if our child is not infinite or we are infinite *)
            (* Can calculate proper offset here *)
            begin match anim.direction with
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

            if not inf && has_repeat anim.repeat then (
                let complete id reason =
                    Option.iter (fun c -> c id reason) anim.complete;
                    let requeue anim =
                        let anim = 
                            if is_mirror anim then (
                                invert_desc anim
                            ) else anim
                        in
                        let anim = replace_delay_with_repeat_delay anim in
                        add t id 0. anim 
                    in
                    match anim.repeat with
                    | Infinite -> requeue anim
                    | Count x ->
                        if x > 1 then (
                            requeue {anim with repeat = Count (x-1)}
                        ) 
                in
                let dummy = 
                    mk_desc 
                    ~delay:(get_end anim)
                    ~complete:complete
                    0.
                    ignore
                    Leaf
                in
                add t id offset dummy
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
