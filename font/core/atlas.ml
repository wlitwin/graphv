open Graphv_core_lib

module AtlasNode = struct
    type t = {
        mutable x : int;
        y : int;
        mutable width : int;
    }
end

type t = {
    mutable width : int;
    mutable height : int;
    nodes : AtlasNode.t DynArray.t;
}

let create ~width ~height ~ncount =
    let n0 = AtlasNode.{
        x = 0;
        y = 0;
        width;
    } in
    let nodes = DynArray.create ncount n0 in
    DynArray.add nodes n0;
    {
        width;
        height;
        nodes;
    }
;;

let insert t idx x y width =
    let node = AtlasNode.{ x; y; width } in
    DynArray.insert t.nodes idx node;
;;

let remove t idx =
    DynArray.remove t.nodes idx 
;;

let reset t width height =
    t.width <- width;
    t.height <- height;
    DynArray.clear t.nodes;

    DynArray.add t.nodes AtlasNode.{
        x = 0;
        y = 0;
        width;
    }
;;

let add_skyline_level t idx x y w h =
    insert t idx x (y+h) w;

    (* TODO - rewrite control flow so it's clearer *)
    let len = DynArray.length t.nodes in
    let i = ref (idx+1) in
    while !i < len do
        let n = DynArray.get t.nodes !i in
        let nlast = DynArray.get t.nodes (!i-1) in
        if n.x < nlast.x + nlast.width then (
            let shrink = nlast.x + nlast.width - n.x in
            n.x <- n.x + shrink;
            n.width <- n.width - shrink;
            if n.width <= 0 then (
                remove t !i;
                i := !i - 1;
            ) else (
                i := len;
            );
            incr i;
        ) else (
            i := len;
        )
    done;

    (*let len = DynArray.length t.nodes - 1 in*)
    let i = ref 0 in
    while !i < (DynArray.length t.nodes) - 1 do
        let n = DynArray.get t.nodes !i in
        let n1 = DynArray.get t.nodes (!i+1) in
        if n.y = n1.y then (
            n.width <- n.width + n1.width;
            remove t (!i+1);
        ) else (
            incr i
        )
    done
;;

let rect_fits t i w h =
    Utils.with_return (fun r ->
        let n = DynArray.get t.nodes i in
        let x = n.x in
        if x+w > t.width then (
            r.return None
        ) else (
            let spaceLeft = ref w in
            let y = ref n.y in
            let i = ref i in
            while !spaceLeft > 0 do
                if !i = DynArray.length t.nodes then (
                    r.return None;
                );
                let n = DynArray.get t.nodes !i in
                y := max !y n.y;
                if !y + h > t.height then (
                    r.return None;
                );
                spaceLeft := !spaceLeft - n.width; 
                incr i;
            done;
            r.return (Some !y)
        )
    )
;;

let add_rect t rw rh =
    let besth = ref t.height in
    let bestw = ref t.width in
    let besti = ref ~-1 in
    let bestx = ref ~-1 in
    let besty = ref ~-1 in

    let len = DynArray.length t.nodes-1 in
    for i=0 to len do
        let y = rect_fits t i rw rh in
        match y with
        | None -> ()
        | Some y ->
            let n = DynArray.get t.nodes i in
            if y + rh < !besth || (y + rh = !besth && n.width < !bestw) then (
                besti := i;
                bestw := n.width;
                besth := y + rh;
                bestx := n.x;
                besty := y;
            );
    done;

    if !besti = ~-1 then (
        None
    ) else (
        add_skyline_level t !besti !bestx !besty rw rh;
        Some (!bestx, !besty)
    )
;;
