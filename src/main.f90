! example.f90
program main
    use, intrinsic :: iso_c_binding, only: c_associated, c_null_char, c_ptr
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    use :: sdl2
    use :: circle
    use :: blackhole_mod
    use :: photon_mod
    implicit none

    integer, parameter :: SCREEN_WIDTH  = 1200
    integer, parameter :: SCREEN_HEIGHT = 700

    type(c_ptr)     :: window
    type(c_ptr)     :: renderer
    type(sdl_event) :: event
    integer         :: rc
    logical         :: is_running

    type(blackhole_t) :: blackhole
    type(photon_t), dimension(40) :: photons

    integer :: p

    ! Initialise SDL.
    if (sdl_init(SDL_INIT_VIDEO) < 0) then
        write (stderr, *) 'SDL Error: ', sdl_get_error()
        stop
    end if

    ! Create the SDL window.
    window = sdl_create_window('Black Hole Simulation' // c_null_char, &
                            SDL_WINDOWPOS_UNDEFINED, &
                            SDL_WINDOWPOS_UNDEFINED, &
                            SCREEN_WIDTH, &
                            SCREEN_HEIGHT, &
                            SDL_WINDOW_SHOWN)

    if (.not. c_associated(window)) then
        write (stderr, *) 'SDL Error: ', sdl_get_error()
        stop
    end if

    ! Create the renderer.
    renderer = sdl_create_renderer(window, -1, 0)

    ! Define blackhole.
    blackhole = blackhole_t(x=600, y=450, mass=3500.0)

    ! Initialize photons.
    do p = 1, size(photons)
        photons(p) = photon_t(x=1100, y=450 - p*10, velo=(/ -1, 0 /) )
    end do

    ! Event loop.
    is_running = .true.

    do while (is_running)
        ! Catch events.
        do while (sdl_poll_event(event) > 0)
            select case (event%type)
                case (SDL_QUITEVENT)
                    is_running = .false.
            end select
        end do

        ! Fill screen background.
        rc = sdl_set_render_draw_color(renderer, &
                                    uint8(51), &
                                    uint8(51), &
                                    uint8(51), &
                                    uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_clear(renderer)


        rc = sdl_render_draw_blackhole(renderer, blackhole)

        do p = 1, size(photons)
            rc = sdl_render_draw_photon(renderer, photons(p))
            call update_photon(photons(p))
            call attract(blackhole, photons(p))
        end do

        ! Render to screen and wait 20 ms.
        call sdl_render_present(renderer)
        call sdl_delay(20)
    end do

    ! Quit gracefully.
    call sdl_destroy_renderer(renderer)
    call sdl_destroy_window(window)
    call sdl_quit()
end program main