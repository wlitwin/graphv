open Tgles2

module Gv = Graphv_gles2

let _ =
    GLFW.init();
    at_exit GLFW.terminate;
    GLFW.windowHint ~hint:GLFW.ClientApi ~value:GLFW.OpenGLESApi;
    GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:2;
    GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:0;

    let window = 
        GLFW.createWindow ~width:400 ~height:400 ~title:"window" () 
    in

    GLFW.makeContextCurrent ~window:(Some window);
    GLFW.swapInterval ~interval:1;

    Gl.clear_color 0.3 0.3 0.32 1.;

    let vg = Gv.create
        ~flags:Gv.CreateFlags.(antialias lor stencil_strokes) 
        () 
    in

    while not GLFW.(windowShouldClose ~window) do
        let win_w, win_h = GLFW.getWindowSize ~window in
        Gl.viewport 0 0 win_w win_h;
        Gl.clear (
            Gl.color_buffer_bit
            lor Gl.depth_buffer_bit
            lor Gl.stencil_buffer_bit
        );

        Gv.begin_frame vg
            ~width:(float win_w)
            ~height:(float win_h)
            ~device_ratio:1.
            ;

        Gv.Path.begin_ vg;
        Gv.Path.rect vg ~x:40. ~y:40. ~w:320. ~h:320.;
        Gv.set_fill_color vg 
            ~color:Gv.Color.(rgba ~r:154 ~g:203 ~b:255 ~a:200);
        Gv.fill vg;

        Gv.end_frame vg;

        GLFW.swapBuffers ~window;
        GLFW.pollEvents();
    done;
;;
