MODULE Temperature
    USE Kinds
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE energy (EOSTemp, RadialBound, fotmtl, tempcs, EnrgyMeltZ, EnrgyMeltZp1, qmaxmelt, &
      &               qmaxmeltp1, enthp, tbar, k, nvoid, igpnod)
    USE Kinds
    USE conversions_fraptran, ONLY : tfk
    USE phypro_h
    USE variables_fraptran, ONLY : naxn, nmesh
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> Subroutine energy computes radially averaged fuel enthalpy
    !
    ! Input
    !
    ! EOSTemp(m,k)      - temperature at radial node m, axial node k, F
    ! RadialBound(l)    - radius to radial node l, ft
    ! fotmtl            - fuel oxygen to metal ratio
    ! tempcs            - cold state temperature of fuel rod  (F)
    ! EnrgyMeltZ(m,k)   - heat absorbed in melting for half mesh on left side of radial node m, axial node k
    ! EnrgyMeltZp1(m,k) - absorbed heat for half mesh on right side (l/2) - heat absorbed in each half mesh at completion of melting
    ! nvoid             - indicator of central void. (0 = No, 1 = Yes)
    !
    ! Output
    !
    ! enthp             - radially averaged fuel enthalpy (J/kg)
    ! tbar              - radially averaged temperature (K)
    !
    INTEGER(ipk) :: l
    INTEGER(ipk), INTENT(IN) :: igpnod, k, nvoid
    REAL(r8k) :: tcormn, ensum, tsum, vsum, dvl, dvr, frmltl, frmltr, tkr, enl, enr, tkl, en_ref, en_Left, en_Right
    REAL(r8k), INTENT(IN) :: fotmtl, tempcs
    REAL(r8k), INTENT(OUT) :: tbar, enthp
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), DIMENSION(:), INTENT(IN) :: RadialBound, qmaxmelt, qmaxmeltp1
    REAL(r8k), DIMENSION(:,:), INTENT(IN) :: EOSTemp, EnrgyMeltZ, EnrgyMeltZp1
    !
    tcormn = 300.0_r8k
    ensum = 0.0_r8k
    tsum = 0.0_r8k
    vsum = 0.0_r8k
    ! calculate number of meshes in fuel
    DO l = 1, (igpnod - 1)
        dvl = (0.5_r8k * (RadialBound(l) + RadialBound(l+1))) ** 2 - RadialBound(l) ** 2
        dvr = RadialBound(l+1) ** 2 - (0.5_r8k * (RadialBound(l) + RadialBound(l+1))) ** 2
        frmltl = EnrgyMeltZp1(l,k) / qmaxmeltp1(l)
        frmltr = EnrgyMeltZ(l+1,k) / qmaxmelt(l+1)
        tkl = tfk(EOSTemp(l,k))
        tkr = tfk(EOSTemp(l+1,k))
        ! Reference
        en_Ref = MatProperty (Material='FUEL', Property='ENTHALPY', Temperature=tfk(tempcs), OMRatio=fotmtl, Pu=compmt, &
          &                   HeatofFusion=fhefus, Fraction_Melt=zero, Tmelt=ftmelt)
        ! Left
        en_Left = MatProperty (Material='FUEL', Property='ENTHALPY', Temperature=tkl, OMRatio=fotmtl, Pu=compmt, &
          &                    HeatofFusion=fhefus, Fraction_Melt=frmltl, Tmelt=ftmelt)
        enl = en_Left - en_Ref
        ! Right
        en_Right = MatProperty (Material='FUEL', Property='ENTHALPY', Temperature=tkr, OMRatio=fotmtl, Pu=compmt, &
          &                     HeatofFusion=fhefus, Fraction_Melt=frmltr, Tmelt=ftmelt)
        enr = en_Right - en_Ref
        IF (l == 1 .AND. nvoid == 1) THEN
            dvl = 0.0_r8k
            dvr = 0.0_r8k
        ENDIF
        ensum = ensum + dvl * enl + dvr * enr
        tsum = tsum + dvl * tkl + dvr * tkr
        vsum = vsum + dvl + dvr
    ENDDO
    enthp = ensum / vsum
    tbar = tsum / vsum
    !
    END SUBROUTINE energy
    !
END MODULE Temperature

