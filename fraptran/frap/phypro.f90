MODULE phypro_h
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module replaces the comdeck phypro
    !> It stores the physical properties of the fuel & cladding materials
    TYPE phypro_var
        ! Fuel Type
        INTEGER(ipk) :: imox
        ! Fuel melting temperature (K)
        REAL(r8k) :: ftmelt
        ! Heat of fusion of the fuel (J/kg)
        REAL(r8k) :: fhefus
        ! Cladding melting temperature (K)
        REAL(r8k) :: ctmelt
        ! Cladding heat of fusion (J/kg)
        REAL(r8k) :: chefus
        ! Liquid/Solid coexistence temperature (K)
        REAL(r8k) :: fdelta
        ! Burnup (=bu(1)) (mw-s/kg-U)
        REAL(r8k) :: bumtp
        ! Fraction of PuO2(=frpo2) (wt%)
        REAL(r8k) :: compmt
        ! Oxygen concentration (=OxygenConcenAve(1)) (kg oxygen/kg zirc)
        REAL(r8k) :: deloxy
    END TYPE phypro_var
    !
    INTEGER(ipk) :: imox
    REAL(r8k) :: ftmelt
    REAL(r8k) :: fhefus
    REAL(r8k) :: ctmelt
    REAL(r8k) :: chefus
    REAL(r8k) :: fdelta
    REAL(r8k) :: bumtp
    REAL(r8k) :: compmt
    REAL(r8k) :: deloxy
    !
    !
    END MODULE phypro_h


