type t = Move_to of { x:float; y:float }
       | Line_to of { x:float; y:float }
       | Rect of {
           x0:float; y0:float;
           x1:float; y1:float;
           x2:float; y2:float;
           x3:float; y3:float;
         }
       | Bezier_to of {
           c1x : float;
           c1y : float;
           c2x : float;
           c2y : float;
           x : float;
           y :float
       }
       | Winding of Graphv_core_lib.Winding.t
       | Close
