module photon_mod
    use :: circle
    use :: constants
    implicit none

    type :: photon_t
        integer :: x, y
        real, dimension(2) :: velo
    end type

    interface photon_t
        procedure :: new_photon
    end interface

contains

    function new_photon(x, y, velo)
        type(photon_t) :: new_photon
        integer :: x, y
        real, dimension(2) :: velo

        new_photon%x = x
        new_photon%y = y
        new_photon%velo = velo
    end function

    subroutine update_photon(photon)
        type(photon_t) :: photon

        photon%x = int(photon%x + photon%velo(1))
        photon%y = int(photon%y + photon%velo(2))
    end subroutine update_photon

    function sdl_render_draw_photon(renderer, photon)
        type(c_ptr), intent(out)   :: renderer
        type(photon_t), intent(in) :: photon
        integer(kind=c_int)        :: sdl_render_draw_photon

        integer :: rc
        type(sdl_circle) :: p

        p = sdl_circle(photon%x, photon%y, 3)

        rc = sdl_set_render_draw_color(renderer, &
                                    uint8(255), &
                                    uint8(233), &
                                    uint8(0), &
                                    uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_fill_circle(renderer, p)

        sdl_render_draw_photon = rc

    end function sdl_render_draw_photon

end module photon_mod