include Flags.Base

let left = 1
let center = 2
let right = 4
let top = 8
let middle = 16
let bottom = 32
let baseline = 64

let v_align t =
    remove t ~flag:(left lor center lor right)

let h_align t =
    remove t ~flag:(top lor middle lor bottom lor baseline)
