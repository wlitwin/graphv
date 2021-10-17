type t = Move_to of { x:float; y:float }
       | Line_to of { x:float; y:float }
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
