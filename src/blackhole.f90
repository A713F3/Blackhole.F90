module blackhole_mod
    use :: circle
    use :: constants
    implicit none

    type :: blackhole_t
        integer :: x, y
        real :: mass, rs
    end type

    interface blackhole_t
        procedure :: new_blackhole
    end interface

contains

    function new_blackhole(x, y, mass)
        type(blackhole_t) :: new_blackhole
        integer :: x, y
        real :: mass

        new_blackhole%x = x
        new_blackhole%y = y
        new_blackhole%mass = mass
        new_blackhole%rs = (2.0 * G * mass) / (c*c)
        print*, "Rs: ", new_blackhole%rs
        print*, "G: ", G
    end function new_blackhole

    function sdl_render_draw_blackhole(renderer, blackhole)
        type(c_ptr), intent(out) :: renderer
        type(blackhole_t), intent(in) :: blackhole
        integer(kind=c_int)      :: sdl_render_draw_blackhole

        integer :: rc

        type(sdl_circle) :: bh_circle, rs_circle, outer_rs

        bh_circle = sdl_circle(blackhole%x, blackhole%y, int(blackhole%rs))
        rs_circle = sdl_circle(blackhole%x, blackhole%y, int(blackhole%rs) * 3)
        outer_rs  = sdl_circle(blackhole%x, blackhole%y, int(blackhole%rs / 2.0) + int(blackhole%rs) * 3)

        
        ! Draw outer accretion disk
        rc = sdl_set_render_draw_color(renderer, &
                                    uint8(255), &
                                    uint8(95), &
                                    uint8(31), &
                                    uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_fill_circle(renderer, outer_rs)

        ! Fill space between ac and blackhole
        rc = sdl_set_render_draw_color(renderer, &
                                    uint8(51), &
                                    uint8(51), &
                                    uint8(51), &
                                    uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_fill_circle(renderer, rs_circle)

        ! Draw blackhole
        rc = sdl_set_render_draw_color(renderer, &
                                    uint8(0), &
                                    uint8(0), &
                                    uint8(0), &
                                    uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_fill_circle(renderer, bh_circle)


        sdl_render_draw_blackhole = rc

    end function

end module blackhole_mod