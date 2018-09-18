MODULE UntinpData_fraptran
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This module contains subroutines that perform unit conversions_fraptran. It contains the subroutine Convert_Input_Units.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> Last Updated 5/6/2016
    !
    PRIVATE
    PUBLIC :: Convert_Input_Units
    
    INTERFACE Convert
        MODULE PROCEDURE Array_KelvintoFahrenheit
        MODULE PROCEDURE Array_SuppliedConversion
    END INTERFACE Convert
    
    CONTAINS
    
    SUBROUTINE Convert_Input_Units
    USE Kinds_fraptran
    USE conversions_fraptran
    USE variables_fraptran, ONLY : zelev, fmesh, nfmesh, cmesh, ncmesh, RodAvePower, &
      & NumAxProfiles, AxPowProfile, nprad, RadPowProfile, &
      & nunopt, ibnopt, gasphs, gasths, nradsh, rshrd, nepp0, eppinp, radpel, cladid, cladod, fuelpeldiam, rf
    USE CoolantProperties_fraptran, ONLY : nbrtmp, temptm, nbrfdr, fldrat, nbrpst, &
      & prestm, nbrliq, flxsec, hrad, hydiam, tshrda, nsrad3, &
      & elvrad, nelrad, trad1, trad2, trad3, hinta, hupta, pbh, gbh, hbh, htclev, ntprs, tblka, &
      & nhprs, htca, npprs
    USE bcdcom_h_fraptran, ONLY : ncooli, nhtc, hlqcl1, achn, dhe, dhy
    USE resti_h_fraptran
    USE excb_h_fraptran, ONLY : nrefld, coldbp, pitch, rsntr, spdbp, splbp, volbp, dofset
    USE scalr_h_fraptran
    IMPLICIT NONE
    !> @brief
    !> Subroutine performs unit conversions for all input units when necessary.
    !> Outputs are in british units unless otherwisise noted.
    !
    ! Conversion variables:
    !
    ! jkbtup - joules/kilogram to btu/pound
    ! tkf    - Kelvin to degree Fahrenheit (function)
    ! kmspfh - kilograms/meter**2/second to pounds/foot**2/hour
    ! km3pf3 - kilograms/meter**3 to pounds/foot**3
    ! ftom   - kilowatt/meter to kilowatt/foot
    ! ftmetr - meters to feet
    ! msins  - meter/second to inches/second
    ! m2ft2  - meters**2 to feet**2
    ! m3ft3  - meters**3 to feet**3
    ! m3in3  - meter**3 to inches**3
    ! psinm2 - newtons/meter**2 to pounds/inch**2 or psia
    ! wmkbfh - Watts/meter**2/Kelvin to btu/foot**2/Fahrenheit/hour
    !
    INTEGER(ipk) :: i
    REAL(r8k), PARAMETER :: jkbtup = 4.302142870e-4_r8k
    REAL(r8k), PARAMETER :: kmspfh = 7.3740276e2_r8k
    REAL(r8k), PARAMETER :: km3pf3 = 6.238562745e-2_r8k
    REAL(r8k), PARAMETER :: msins = 39.37007874_r8k
    REAL(r8k), PARAMETER :: m2ft2 = 10.76365_r8k
    REAL(r8k), PARAMETER :: m3ft3 = 35.31466670_r8k
    REAL(r8k), PARAMETER :: wmkbfh = 0.1761101943_r8k
    REAL(r8k), PARAMETER :: m3in3 = 6.102376e4_r8k
    
    ! $bcdinp
    
    ! Flow shroud temperature
    IF (nradsh >= 1) CALL Convert (tshrda, nradsh, 2)

    ! Multiple radiation option arrays
    IF (nsrad3 > 0) THEN
        ! Convert elevations
        IF (ibnopt(31) == 1) CALL Convert (elvrad, nelrad, 1, ftmetr)
        IF (ibnopt(33) > 0) THEN
            ! Convert temperatures
            DO i = 1, nelrad
                CALL Convert (trad1(:,i), nsrad3, 2)
                CALL Convert (trad2(:,i), nsrad3, 2)
                CALL Convert (trad3(:,i), nsrad3, 2)
            END DO
        END IF
    END IF
    
    ! Coolant condition arrays
    IF (ncooli /= 0) THEN
        
        ! Lower and upper plenum enthalpy
        IF (nhinta >= 1 .OR. nhupta >= 1) THEN  
            CALL Convert (hinta, nhinta, 2, jkbtup)
            CALL Convert (hupta, nhupta, 2, jkbtup)
        END IF
        
        ! Average core pressure
        IF (npbh >= 1) CALL Convert (pbh, npbh, 2, psinm2)
        
        ! Average mass flux
        IF (ngbh >= 1) CALL Convert (gbh, ngbh, 2, kmspfh)
        
        ! Core average enthalpy
        IF (nhbh >= 1) CALL Convert (hbh, nhbh, 2, jkbtup)
        
        ! Lower plenum temperature
        ! IF (ibnopt(28) == 1) CALL untin2 (hinta, nhinta, 2)
    END IF
    
    ! Heat transfer coeffient arrays
    IF (nhtc /= 0 .AND. nqchn /= 4 .AND. nhtcz /= 0) THEN
        ! Tape input
        ! Elevations for htc zones
        CALL Convert (htclev, nhtcz,  1, ftmetr)
        ! Core pressure
        CALL Convert (pbh, npbh, 2, psinm2)
        
        DO i = 1, nhtcz
            
            ! Coolant Temperature
            CALL Convert (tblka(:,i), ntprs(i), 2)
            
            ! Heat Transfer Coefficient
            CALL Convert (htca(:,i), ntprs(i), 2, wmkbfh)

        END DO
        
    END IF
    
    ! Reflood arrays
    IF (nrefld /= 0) THEN
        ! Inlet temperature
        IF (nbrtmp >= 1) CALL Convert (temptm, nbrtmp, 2)
        ! Flooding rate
        IF (nbrfdr >= 1) CALL Convert (fldrat, nbrfdr, 2, msins)
        ! System pressure
        IF (nbrpst >= 1) CALL Convert (prestm, nbrpst, 2, psinm2)
        ! Liquid level height
        IF (nbrliq >= 1) CALL Convert (hlqcl1, nbrliq, 2 , ftmetr)
    END IF
    
    ! Single variables
    achn = achn * m2ft2
    dhe = dhe * ftmetr
    dhy = dhy * ftmetr
    flxsec = flxsec * m2ft2
    hrad = hrad * wmkbfh
    hydiam = hydiam * ftmetr
    rshrd = rshrd * ftmetr
    
    ! $powinp
    
    ! Power history
    CALL Convert (RodAvePower, nptha, 2, ftom)
    
    ! Axial power profile, convert axial elevation from meters to ft
    DO i = 1, NumAxProfiles
        CALL Convert (AxPowProfile(2:,i), npaxp, 2, ftmetr)
    END DO
    
    ! Radial power profile, convert radii from meters to ft
    ! Commented out because input manual states that shall always be input in meters_fraptran.
    ! Don't convert units when input is in SI
    !CALL Convert (RadPowProfile(2:), nprad, 2, ftmetr)
    !
    !DO i = 1, naxn
    !    CALL Convert (fuelrad(i,:), nradq, 1, ftmetr)
    !END DO
    ! Single variables
    powop  = powop * ftom
    
    ! $intinp
    
    ! Fast neutron flux axial profile heights
    IF (nfastf /= 0) CALL Convert (fluxz(2:), nfastf, 2, ftmetr)
    
    ! Cladding plastic hoop strains
    IF (nepp0 /= 0) CALL Convert (eppinp(2:), nepp0, 2, ftmetr)
    
    ! Pellet radius deviations
    IF (nPelRadDeviat /= 0) CALL Convert (radpel, 2*nPelRadDeviat, 1, ftmetr)
    
    ! Single variables
    cladid = cladid * ftmetr
    claDOd = claDOd * ftmetr
    coldbp = coldbp * ftmetr
    dishd = dishd * ftmetr
    dishv0 = dishv0 * m3ft3
    drod = drod * ftmetr
    gappr0(1) = gappr0(1) * psinm2
    pelh = pelh * ftmetr
    FuelPelDiam = FuelPelDiam * ftmetr
    pitch = pitch * ftmetr
    rf = rf * ftmetr
    rl = rl * ftmetr
    rshd = rshd * ftmetr
    rsntr = rsntr * ftmetr
    scd(1) = scd(1) * ftmetr
    spdbp = spdbp * ftmetr
    spl(1) = spl(1) * ftmetr
    splbp = splbp * ftmetr
    swd(1) = swd(1) * ftmetr
    tgas0 = tkf(tgas0)
    volbp = volbp * m3ft3
    vplen(1) = vplen(1) * m3ft3
    
    ! $numinp
    
    ! Axial node elevation
    IF (nunopt(11) == 2) CALL Convert (zelev, naxn, 1, ftmetr)
    
    ! Radial node radii for fuel
    IF (nunopt(12) == 2) CALL Convert (fmesh, nfmesh, 1, ftmetr)
    
    ! Radial node radii for cladding
    IF (nunopt(13) == 2) CALL Convert (cmesh, ncmesh, 1, ftmetr)
    
    ! Single variables
    tmaxc = tkf(tmaxc)
    tmaxf = tkf(tmaxf)
    t0c = tkf(t0c)
    t0f = tkf(t0f)
    
    ! $modinp
    
    ! Single variables
    dofset = dofset * ftmetr
    rvoid = rvoid * ftmetr
    zvoid1 = zvoid1 * ftmetr
    zvoid2 = zvoid2 * ftmetr
    
    ! Prescribed internal gas pressure vs time
    IF (ngaspr >= 1) CALL Convert (gasphs, ngaspr, 2, psinm2)
    
    ! Prescribed plenum temperature vs time
    
    ! Upper Plenum
    IF (ngastmp(1) >= 1) CALL Convert (gasths(:,1), ngastmp(1), 2)
    
    ! Lower Plenum
    IF (ngastmp(2) >= 1) CALL Convert (gasths(:,2), ngastmp(2), 2)
    
    END SUBROUTINE Convert_Input_Units
    !
    !
    !
    SUBROUTINE Array_SuppliedConversion (a, n, ns, b)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Performs unit conversion on an array based on the value supplied for b
    !>@author
    !> Updated by Ian Porter, NRC
    !>@date
    !> 5/6/2016
    
    INTEGER(ipk) :: i_end, i
    INTEGER(ipk), INTENT(IN) :: n, ns
    REAL(r8k), INTENT(IN) :: b
    REAL(r8k), DIMENSION(:) :: a
    
    ! Only perform when number of supplied values in array is > 0
    IF (n == 0) THEN
        WRITE (0,101)
        WRITE (ounit,101)
101     FORMAT('WARNING: Bad value sent into Subroutine: Array_SuppliedConversion. NO CONVERSION PERFORMED.')
        RETURN
    END IF
    
    ! Set the end value (the array may be larger than the # of values being converted)
    SELECT CASE (ns)
    CASE (1)
        i_end = n
    CASE (2)
        i_end = 2 * n - 1
    CASE DEFAULT
        ERROR STOP 'Bad value supplied for number of skips (ns). Execution terminated in Subroutine: Array_SuppliedConversion'
    END SELECT
    
    ! Convert values by (b) incrimentally (based on ns) to i_end
    DO i = 1, i_end, ns
        a(i) = a(i) * b
    END DO
    
    END SUBROUTINE Array_SuppliedConversion
    !
    !
    !
    SUBROUTINE Array_KelvintoFahrenheit (a, n, ns)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tkf
    IMPLICIT NONE
    !>@brief
    !> Converts kelvin to fahrenheit over an array of values
    !>@author
    !> Updated by Ian Porter, NRC
    !>@date
    !> 5/6/2016
    
    INTEGER(ipk) :: i_end, i
    INTEGER(ipk), INTENT(IN) :: n, ns
    REAL(r8k), DIMENSION(:) :: a
    
    ! Only perform when number of supplied values in array is > 0
    IF (n == 0) RETURN
    
    ! Set the end value (the array may be larger than the # of values being converted)
    SELECT CASE (ns)
    CASE (1)
        i_end = n
    CASE (2)
        i_end = 2 * n - 1
    CASE DEFAULT
        ERROR STOP 'Bad value supplied for number of skips (ns). Execution terminated in Subroutine: Array_KelvintoFahrenheit'
    END SELECT
    
    ! Convert temperatures incrimentally (based on ns) to i_end
    DO i = 1, i_end, ns
        a(i) = tkf(a(i))
    END DO
    
    END SUBROUTINE Array_KelvintoFahrenheit
    !
END MODULE UntinpData_fraptran













