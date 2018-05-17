module fpn4rastk

    use conversions
    use frapcon41, only : frapcon_type

    implicit none

    type(frapcon_type) :: fpn

contains

    subroutine init(nr, na, x)

        integer :: na          ! number of nodes in the axial mesh
        integer :: nr          ! number of radial nodes in a pellet
        integer :: ngasr = 45  ! number of radial gas release nodes
        integer :: nce = 5     ! number of radial elements in the cladding for the FEA model

        integer :: mechan  = 2 ! cladding mechanical model (1: FEA, 2: FRACAS-I)
        integer :: ngasmod = 2 ! fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)

        real(8) :: x(:)        ! Elevation in each qf, x array defining a power shape

        call fpn % init(na, ngasr, nr, nce, mechan, ngasmod)

        fpn % x(:) = x(:)

        call fpn % make()

    end subroutine init

    subroutine next(dt)

       real(8) :: dt

       call fpn % next(dt)

    end subroutine next

    subroutine set_linear_power_ratio(qf)

        real(8) :: qf(:) ! Ratio of linear power at x(n) elevation to axially average value for each M-th power shape

        fpn % qf(:) = qf(:)

    end subroutine set_linear_power_ratio


end module fpn4rastk