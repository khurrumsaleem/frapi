MODULE ZircSpecHeat_fraptran
    USE Kinds_fraptran
    USE conversions_fraptran
    USE functions_fraptran, ONLY : polate
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE ccpmod (RhoCp, BOSTemp, FinalTemp, k, rhoc, RhoCp0)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk
    USE variables_fraptran, ONLY : ounit, nmesh, ncladi, Time, ndebug
    IMPLICIT NONE
    !>@brief
    !> This Subroutine modifies cladding specific heat to assure that all energy required to complete alpha to beta phase
    !> transition is accounted for, even if large cladding temperature increase during time step occurs
    !>@author
    !>
    !
    ! Input
    !
    ! RhoCp0(l,k)  -  start of time step RhoCp (btu/ft3-F)
    ! BOSTemp(l,k) -  temperature at start of time step (F)
    ! FinalTemp(l) -  temperature at end of time step   (F)
    ! ncladi       -  node number of cladding inside surface
    ! nmesh        -  number of radial nodes
    ! rhoc         -  cladding density  (lb/ft3)
    !
    ! Output
    !
    ! RhoCp(l,k)   -  cladding specific heat * density (btu/ft3-F)
    !
    INTEGER(ipk) :: l
    INTEGER(ipk), INTENT(IN) :: k
    REAL(r8k) :: tk1, tk2, dltcp, dlttmp, cpsi
    REAL(r8k), INTENT(IN) :: rhoc
    REAL(r8k), PARAMETER :: tstarta2b = 1050.0_r8k  ! Temperature at start of alpha to beta phase transition
    REAL(r8k), PARAMETER :: tenda2b = 1300.0_r8k    ! Temperature at end of alpha to beta phase transition
    REAL(r8k), PARAMETER :: dtmin = 2.0_r8k      !
    REAL(r8k), PARAMETER :: cpcnvt = 4.184e+3_r8k ! Convert cpsi from j/kg-K to btu/lb-F
    REAL(r8k), DIMENSION(:), INTENT(IN) :: FinalTemp
    REAL(r8k), DIMENSION(:,:), INTENT(IN) :: RhoCp0, BOSTemp
    REAL(r8k), DIMENSION(:,:), INTENT(OUT) :: RhoCp
    !
    IF (ndebug) THEN
        WRITE(ounit,900) Time, ncladi, nmesh
900     FORMAT(' CCPMOD: Time = ',e13.6,' ncladi = ',i3,' nmesh = ',i3)
903     FORMAT(8(2x,e11.4))
        WRITE(ounit,913)
913     FORMAT(' Start of time step RhoCp0')
        WRITE(ounit,903) (RhoCp0(l,k),l = ncladi,nmesh-1)
        WRITE(ounit,905)
905     FORMAT(' Start of time step temperature(s)')
        WRITE(ounit,903) (BOSTemp(l,k),l = ncladi,nmesh-1)
        WRITE(ounit,907)
907     FORMAT(' End of time step temperature(s)')
        WRITE(ounit,903) (FinalTemp(l),l = ncladi,nmesh-1)
    ENDIF
    !
    DO l = ncladi, (nmesh - 1)
        tk1 = 0.5_r8k * (tfk(FinalTemp(l)) + tfk(FinalTemp(l+1)))
        tk2 = 0.5_r8k * (tfk(BOSTemp(l,k)) + tfk(BOSTemp(l+1,k)))
        IF (tk1 < tstarta2b .AND. tk2 < tstarta2b) CYCLE
        IF (tk1 > tenda2b .AND. tk2 > tenda2b) CYCLE
        IF (ABS(tk2 - tk1) < dtmin) CYCLE
        ! Compute integral of ccp(t) function between temperature interval tk1 and tk2
        dltcp = ccpint (tk1, tk2)
        dlttmp = tk2 - tk1
        cpsi = dltcp / dlttmp
        IF (ndebug) WRITE(ounit,909) cpsi, dltcp, dlttmp, rhoc, tk1, tk2, l
909     FORMAT(' cpsi = ',e11.4,' dltcp = ',e11.4,' dlttmp = ',e11.4, &
          &       ' rhoc = ',e11.4/' tk1 = ',e11.4,' tk2 = ',e11.4, ' l = ',i2)
        ! convert cpsi from j/kg-K to btu/lb-F and multiply by cladding density
        RhoCp(l,k) = 2.0_r8k * (cpsi / cpcnvt) * rhoc - RhoCp0(l,k)
    ENDDO
    !
    IF (ndebug) THEN
        WRITE(ounit,911) k
911     FORMAT('output RhoCp array for axial node ',i3)
        WRITE(ounit,903) (RhoCp(l,k), l = ncladi,nmesh-1)
    ENDIF
    !
    END SUBROUTINE ccpmod
    !
    !
    !
    REAL(r8k) FUNCTION ccpint (t1, t2)
    USE Kinds_fraptran
    USE functions_fraptran, ONLY : polate
    USE variables_fraptran, ONLY : CladType, ounit
    IMPLICIT NONE
    !>@brief
    !> ccpint integrates the function ccp between the temperature limits of t1 and t2
    !>@author
    !>
    !
    ! Inputs
    !
    ! t1       - Lower bound temperature  (K)
    ! t2       - Upper bound temperature  (K)
    ! CladType - Cladding indicator
    !
    ! Output
    !
    ! ccpint   - Integral of ccp  (J/kg)
    !
    INTEGER(ipk) :: iwwer
    INTEGER(ipk), PARAMETER :: ncpi = 16
    REAL(r8k) :: cpi1, cpi2
    REAL(r8k), INTENT(IN) :: t1, t2
    ! generic Zry Data
    REAL(r8k), DIMENSION(32), PARAMETER :: cpi = [ &
      &      0.0_r8k,  290.0_r8k,    2800.0_r8k,  300.0_r8k, &
      &  31950.0_r8k,  400.0_r8k,  107910.0_r8k,  640.0_r8k, 266760.0_r8k, 1090.0_r8k, &
      & 268075.0_r8k, 1093.0_r8k,  278995.0_r8k, 1113.0_r8k, 291045.0_r8k, 1133.0_r8k, &
      & 304385.0_r8k, 1153.0_r8k,  319735.0_r8k, 1173.0_r8k, 335595.0_r8k, 1193.0_r8k, &
      & 349485.0_r8k, 1213.0_r8k,  360365.0_r8k, 1233.0_r8k, 366553.0_r8k, 1248.0_r8k, &
      & 741062.0_r8k, 2300.0_r8k, 1.1683e+6_r8k, 3500.0_r8k ]
    ! Data for Zr-Nb from RRC-KI
    ! low heat rate
    REAL(r8k), DIMENSION(32), PARAMETER :: cpw1 = [ &
      &      0.0_r8k,  290.0_r8k,   35195.0_r8k,  400.0_r8k, &
      &  70731.0_r8k,  500.0_r8k,  145246.0_r8k,  700.0_r8k, 214374.0_r8k,  880.0_r8k, &
      & 245414.0_r8k,  940.0_r8k,  285763.0_r8k, 1000.0_r8k, 325447.0_r8k, 1050.0_r8k, &
      & 370510.0_r8k, 1113.0_r8k,  390210.0_r8k, 1153.0_r8k, 408714.0_r8k, 1200.0_r8k, &
      & 428314.0_r8k, 1250.0_r8k,  447914.0_r8k, 1300.0_r8k, 467514.0_r8k, 1350.0_r8k, &
      & 839914.0_r8k, 2300.0_r8k, 1.3103e+6_r8k, 3500.0_r8k ]
    ! high heat rate
    REAL(r8k), DIMENSION(32), PARAMETER :: cpw2 = [ &
      &      0.0_r8k,  290.0_r8k,  33198.0_r8k,  400.0_r8k, &
      & 110054.0_r8k,  640.0_r8k, 181100.0_r8k,  840.0_r8k, 242520.0_r8k, 1000.0_r8k, &
      & 262549.0_r8k, 1050.0_r8k, 307641.0_r8k, 1113.0_r8k, 321956.0_r8k, 1133.0_r8k, &
      & 336271.0_r8k, 1153.0_r8k, 350586.0_r8k, 1173.0_r8k, 369911.0_r8k, 1200.0_r8k, &
      & 387469.0_r8k, 1250.0_r8k, 405336.0_r8k, 1300.0_r8k, 423512.0_r8k, 1350.0_r8k, &
      & 827588.0_r8k, 2300.0_r8k, 1.4975e6_r8k, 3500.0_r8k ]
    !
    SELECT CASE (CladType)
    CASE (6, 8) ! RRC-KI Zr-1%Nb property
        ! This block calculates the enthalpy at constant pressure for H1-alloy cladding. If iwwer = 1 Then Data is derived from
        ! Volkov B.Yu. et. al."Material property library for H1-alloy cladding", Preprint IAE-4941/11, 1989. This property is
        ! induced by low heating rate. If iwwer = 2, data is derived from Ljusternik V.E. et. al."Experimental research of 
        ! zirconium reactor materials thermal properties: 110-alloy". M.,J. High Temperature Thermal Physics." v.31 n.4, 1993. 
        ! This property is induced by high-rate heating >= 1000 K/s.
        !
        ! 5/5/03: default to iwwer=1 until can set this flag up
        iwwer = 1
        SELECT CASE (iwwer)
        CASE (1) ! Low heat rate
            cpi1 = polate(cpw1, t1, ncpi)
            cpi2 = polate(cpw1, t2, ncpi)
            ccpint = cpi2 - cpi1
        CASE (2) ! High heat rate
            cpi1 = polate(cpw2, t1, ncpi)
            cpi2 = polate(cpw2, t2, ncpi)
            ccpint = cpi2 - cpi1
        END SELECT
        !
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry Properties
        cpi1 = polate(cpi, t1, ncpi)
        cpi2 = polate(cpi, t2, ncpi)
        ccpint = cpi2 - cpi1
    CASE DEFAULT
        WRITE (ounit, *) 'Execution terminated in Subroutine: ccpint; Wrong value of Cladtype. CladType =',CladType
        ERROR STOP 'Execution terminated in Subroutine: ccpint; Wrong value of CladType selected.'
    END SELECT
    !
    END FUNCTION ccpint
    !
END MODULE ZircSpecHeat_fraptran













