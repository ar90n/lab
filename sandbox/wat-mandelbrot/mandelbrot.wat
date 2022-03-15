(module
    (import "env" "buffer" (memory 160))
    (global $cnvs_size (import "env" "cnvs_size") i32)
    (global $ns_start (import "env" "ns_start") i32)
    (global $cmap_start (import "env" "cmap_start") i32)
    (global $org_real (import "env" "org_real") f64)
    (global $org_imag (import "env" "org_imag") f64)
    (global $dx (import "env" "dx") f64)
    (global $dy (import "env" "dy") f64)

    (func $get_pixel_bytes (result i32)
        global.get $cnvs_size
        global.get $cnvs_size
        i32.mul
        i32.const 4
        i32.mul
    )

    (func $init
        (local $i i32)
        (local $pixel_bytes i32)

        call $get_pixel_bytes
        local.set $pixel_bytes

        (loop $pixel_loop
            (i32.store (local.get $i) (i32.const 0x00000000))
            (i32.store (i32.add (local.get $i) (global.get $ns_start)) (i32.const 0x00000000))
            (i32.add (local.get $i) (i32.const 4))
            local.set $i

            (i32.lt_u (local.get $i) (local.get $pixel_bytes))
            br_if $pixel_loop
        )
    )

    (func $fill_screen
        (local $i i32)
        (local $pixel_bytes i32)

        call $get_pixel_bytes
        local.set $pixel_bytes

        (loop $pixel_loop
            (i32.store
                (local.get $i)
                (i32.load
                    (i32.add
                        (global.get $cmap_start)
                        (i32.mul
                            (i32.load 
                                (i32.add
                                    (local.get $i)
                                    (global.get $ns_start)
                                )
                            )
                            (i32.const 4)
                        )
                    )
                )
            )
            (i32.add (local.get $i) (i32.const 4))
            local.set $i

            (i32.lt_u (local.get $i) (local.get $pixel_bytes))
            br_if $pixel_loop
        )
    )
    (func $load_n (param $i i32) (result i32)
        (i32.load 
            (i32.add
                (local.get $i)
                (global.get $ns_start)
            )
        )
    )
    (func $store_n  (param $v i32) (param $i i32)
        (i32.store
            (i32.add
                (local.get $i)
                (global.get $ns_start)
            )
            (local.get $v)
        )
    )

    (func $calc_next_real (param $x f64) (param $y f64) (param $cr f64) (result f64)
        (f64.mul
            (local.get $x)
            (local.get $x)
        )
        (f64.mul
            (local.get $y)
            (local.get $y)
        )
        (f64.sub)
        (local.get $cr)
        (f64.add)
    )

    (func $calc_next_imag (param $x f64) (param $y f64) (param $ci f64) (result f64)
        (f64.const 2.0)
        (f64.mul
            (local.get $x)
            (local.get $y)
        )
        (f64.mul)
        (local.get $ci)
        (f64.add)
    )

    (func $calc_next_n (param $n i32) (param $x f64) (param $y f64) (result i32)
        (i32.and
            (f64.lt
                (f64.add
                    (f64.mul
                        (local.get $x)
                        (local.get $x)
                    )
                    (f64.mul
                        (local.get $y)
                        (local.get $y)
                    )
                )
                (f64.const 4000000)
            )
            (i32.lt_s (local.get $n) (i32.const 15))
        )
        (i32.add (local.get $n))
    )

    (func $update
        (local $i i32)
        (local $cr f64)
        (local $ci f64)
        (local $cur_real f64)
        (local $cur_imag f64)
        (local $cur_n i32)
        (local $last_n i32)
        (local $pixel_bytes i32)

        call $get_pixel_bytes
        local.set $pixel_bytes

        (local.set $cr (global.get $org_real))
        (local.set $ci (global.get $org_imag))
        (loop $pixel_loop
            (local.set $cur_real (f64.const 0))
            (local.set $cur_imag (f64.const 0))
            (local.set $cur_n (i32.const 0))

            (loop $iter_loop
                (local.get $cur_n)
                (local.tee $last_n)
                (call $calc_next_real
                    (local.get $cur_real)
                    (local.get $cur_imag)
                    (local.get $cr)
                )
                (call $calc_next_imag
                    (local.get $cur_real)
                    (local.get $cur_imag)
                    (local.get $ci)
                )
                (local.set $cur_imag)
                (local.tee $cur_real)
                (local.get $cur_imag)
                (call $calc_next_n)
                (local.tee $cur_n)
                (i32.ne (local.get $last_n))
                br_if $iter_loop
            )
            (call $store_n (local.get $cur_n)(local.get $i) )

            (i32.add (local.get $i) (i32.const 4))
            local.set $i

            (i32.rem_s
                (local.get $i)
                (i32.mul (global.get $cnvs_size) (i32.const 4))
            )
            (i32.eq (i32.const 0))
            if
                (local.set $cr (global.get $org_real))
                (local.set $ci (f64.add (local.get $ci) (global.get $dy)))
            else
                (local.set $cr (f64.add (local.get $cr) (global.get $dx)))
            end

            (i32.lt_u (local.get $i) (local.get $pixel_bytes))
            br_if $pixel_loop
        )
    )
    (func (export "main")
        (call $init)
        (call $update)
        (call $fill_screen)
    )
)