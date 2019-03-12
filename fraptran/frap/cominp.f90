MODULE Initialization_fraptran
    USE Kinds_fraptran
    USE RadialNodes_fraptran, ONLY : Radheatsource, weights
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE cominp
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : pi, ftmetr
    USE functions_fraptran, ONLY : polate
    USE variables_fraptran
    USE PlotFile_fraptran
    USE rlpst_fraptran, ONLY : rlpst1
    USE sth2x_fraptran, ONLY : sth2xi
    USE NCGases_fraptran, ONLY : ngases
    USE ErrorMsg_fraptran, ONLY : errori
    IMPLICIT NONE
    !> @brief
    !> This subroutine calulates all common block variables which are directly derived from input
    !> and which require data from multiple Data blocks
    !
    ! common block thcntl is link whereby thermal-hydraulic code controls the FrapTran calculations
    !
    INTEGER(ipk) :: iu=1, ncols, isf, isc, k2, k3, k, i, j, kk, ncmvec, lktabl, iindx, ia, nm2, &
      &             l, ns1, nr, ir, l1, kconds, k1, k4, idat, mchf
    INTEGER(ipk), PARAMETER :: nbrpts = 17
    REAL(r8k) :: f, evenaxnodlen, areaf, arear, asum, diffa, s, dum, dum1, dum2, &
      &          spvol, sump, radnorm, p, rmeter, rs1, rs2, qr1, qr2, bu1, bu2, c, fnbr, tottl, deltl, &
      &          hl1, tl1, tlm, tls, hint, hl2, tl2, xmesh, dum3, da, dasum, powsum, pnorm, pnormi, &
      &          totsrc, srcold, srcnew, volwtl, volwtr, volsrc, cruf
    REAL(r8k), DIMENSION(34), PARAMETER :: zfapif = [ 0.0_r8k, 0.0_r8k, 1.79_r8k, 0.871_r8k, 2.42_r8k, 1.289_r8k, &
      &                       3.00_r8k, 1.786_r8k, 3.58_r8k, 2.435_r8k, 4.17_r8k, 3.187_r8k, 4.88_r8k, 4.223_r8k, &
      &                       5.42_r8k, 5.069_r8k, 6.00_r8k,  6.00_r8k, 6.58_r8k, 6.931_r8k, 7.12_r8k, 7.777_r8k, &
      &                       7.83_r8k, 8.813_r8k, 8.42_r8k, 9.565_r8k, 9.00_r8k, 10.214_r8k, 9.58_r8k, 10.711_r8k, &
      &                      10.21_r8k, 11.129_r8k, 12.00_r8k, 12.00_r8k ]
    !
    IF (nlac == 1) THEN
        ndim = 0
        swllfr = 0.0_r8k
    ENDIF
    !
    WRITE(ounit,524)
524 FORMAT(40x,'input common block definitions'/)
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    ! Definition of problem numerics variables
    !
    ! Dynamic dimensioning parameters
    ! skip if restart is being performed and nodalization is not changed
    ! set up length of dynamic storage array for heat-1 subcode
    !
    IF (ncool == 7) THEN
        !
        CALL rlpst1 (igpnod, ncladi, ncols, RadialBound)
        !
        nfmesh = igpnod
        ncmesh = ncols - igpnod
        ! Check to see if restart has the same number of nodes
        IF (ncards == 1) THEN
            nmesh = nfmesh + ncmesh
        ELSE IF (nmesh /= ncols) THEN
            ierr = 1
            WRITE(ounit,523)
523         FORMAT(/'Number of radial nodes are different than on restart, check file names')
            ERROR STOP 'Number of radial nodes in restart file are different than in input file. &
              &         Execution stopped in Subroutine: cominp'
        END IF
    ELSE
        IF (ncards == 1) THEN
            igpnod = nfmesh
            ncladi = igpnod + 1
            nmesh = nfmesh + ncmesh
        END IF
    END IF
    !
    length = 1
    IF (ncool == 7) THEN
        la1max = 1
        ! Skip if there are no heat-1 calculations
    ELSE IF (ncards == 1) THEN
        ! length required by heat-1 arrays
        la1fil = 57 + 13 * (nmesh - 1)
        ! length required by thermal property arrays
        ! nkf = number of conductivity versus temperature pairs for fuel
        ! nsf = number of specific heat versus temperature pairs for fuel
        ! nkc = number of conductivity versus temperature pairs for clad
        ! nsc = number of specific heat versus temperature pairs for clad
        !
        IF (nkf <= 1) nkf = 2
        IF (nsf == 0 .OR. nsf == nkf) nsf = -nkf
        isf = ABS(nsf)
        IF (nsf + nkf < 0) nsf = isf
        IF (nkc <= 1) nkc = 2
        IF (nsc == 0 .OR. nsc == nkc) nsc = -nkc
        isc = ABS(nsc)
        IF (nsc + nkc < 0) nsc = isc
        !
        k2 = 9 + 2 * nkf + isf
        IF (nsf > 1) k2 = k2 + nsf
        k3 = k2 + 4 + 2 * nkc + isc
        IF (nsc > 1) k3 = k3 + nsc
        ncmvec = k3 + 6
        ! length requirements for fuel and cladding thermal conductivity tables
        lktabl = 2 * nkf + 2 * nkc
        ! set up index for top of heat conduction space
        indx = lktabl
        ! setup space for scratch for heat1
        length = 3 * (nmesh - 1) + 1
        ! define total length for heat conduction space
        la1max = la1fil + lktabl + ncmvec + length
        ! debug
        la1max = 1500
    END IF
    IF (ncards == 1) THEN
        la1req = la1max + 1
        ! set up space for array with channel data and coolant conditions
        IF (ithymx > 0) THEN
            ! add space for azimuthally varying coolant conditions
            ixazmn = la1req
            la1req = 6 * ithymx * nvol + ithymx + 2 + la1req
        ENDIF
        la1req = la1req + naxn * nchn
        ! add space for r-theta calculations
        IF (naz /= 1) la1req = la1req + 7 * naz * nnaz * nmesh + 24 * naz * nnaz
    END IF
    ! If (ncards == 1 .OR. nopt(2) /= 0) THEN
    ! Initialized material properties (melting points, etc)
    CALL phyprp
    ! Set up fuel and cladding thermal conductivity tables for subroutine qcon
    ! Store thermal properties in a1 array for heat1 calculations
    IF (ncool /= 7) Call thmprp (a1, k2, k3, isc, isf)
    !
    ! END IF
    ! * * * * * numerical control * * * * *
    ! multiDimensional heat conduction starting time
    IF (ndim > 0) timmd = t0
    !
    ! * * * * * convergence control * * * * *
    ! none
    !
    ! * * * * * nodalization initialization * * * * *
    IF (ncards == 1) THEN
        ! set up AxNodElevat array if axial nodes have been specified
        ! check to see if evenly spaced nodes was specified
        SELECT CASE (nunopt(11))
        CASE (1) ! Evently spaced axial mesh
            EvenAxNodLen = rl / naxn
            ! Correct location of top axial node for evenly spaced mesh
            AxNodElevat(naxn) = rl - EvenAxNodLen / 2.0_r8k
            AxialNodLen(naxn) = EvenAxNodLen
            zelev(naxn) = rl - EvenAxNodLen / 2.0_r8k
            IF (naxn > 1) THEN
                AxNodElevat(1) = EvenAxNodLen / 2.0_r8k
                AxialNodLen(1) = EvenAxNodLen
                zelev(1) = EvenAxNodLen / 2.0_r8k
                !
                DO k = 2, (naxn - 1)
                    AxialNodLen(k) = EvenAxNodLen
                    zelev(k) = zelev(k-1) + EvenAxNodLen
                    AxNodElevat(k) = zelev(k)
                ENDDO
            ENDIF
        CASE DEFAULT ! User specified elevations
            IF (naxn <= 1) THEN
                AxialNodLen(1) = rl
            ELSE
                AxialNodLen(1) = zelev(1) + 0.5_r8k * (zelev(2) - zelev(1))
                AxialNodLen(naxn) = rl - zelev(naxn) + 0.5_r8k * (zelev(naxn) - zelev(naxn-1))
                AxNodElevat(1) = zelev(1)
                AxNodElevat(naxn) = zelev(naxn)
                IF (naxn >= 3) THEN
                    DO k = 2, (naxn - 1)
                        AxNodElevat(k) = zelev(k)
                        AxialNodLen(k) = 0.5_r8k * (zelev(k+1) - zelev(k-1))
                    ENDDO
                ENDIF
            ENDIF
        END SELECT
        ! radial nodes, skip If t-h codes provides nodes
        IF (ncool /= 7) THEN
            ! Set up RadialBound array for fuel and cladding
            ! Check to see If only the number of fuel nodes was specified
            IF (nunopt(12) == 1) THEN
                ! Use nfmesh and fuel radius to calculate nodes for equal area rings_fraptran
                !
                ! If there is no central void, nvoid and rvoid are zero
                !
                ! fuel pellet cross-sectional area
                areaf = pi * (FuelPelDiam / 2.0_r8k) ** 2
                ! area of equal area rings
                arear = areaf / (nfmesh - 1)
                RadialBound(1) = 0.0_r8k
                iindx = 2
                ! Adjust for a central void
                IF (nvoid /= 0) THEN
                    iindx = 3
                    areaf = pi * ((FuelPelDiam / 2.0_r8k) ** 2 - rvoid ** 2)
                    arear = areaf / (nfmesh - 1)
                    RadialBound(2) = rvoid
                ENDIF
                ! Determine radii of nodes, sum up areas as a check
                asum = 0.0_r8k
                DO i = iindx, (nfmesh - 1)
                    RadialBound(i) = SQRT(arear / pi + RadialBound(i-1) ** 2)
                    asum = asum + pi * (RadialBound(i) ** 2 - RadialBound(i-1) ** 2)
                ENDDO
                RadialBound(nfmesh) = FuelPelDiam / 2.0_r8k
                asum = asum + pi * (RadialBound(nfmesh) ** 2 - RadialBound(nfmesh-1) ** 2)
                dIffa = (areaf - asum) / areaf
                IF (ABS(dIffa) > 0.01_r8k) THEN
                    WRITE(ounit,*) 'Execution stopped in Subroutine: compinp. Fuel pellet area after nodalization is incorrect'
                    ERROR STOP 'Execution stopped in Subroutine: compinp. Fuel pellet area after nodalization is incorrect'
                END IF
            ELSE
                ! User specified fuel nodes positions,
                ! fmesh readin in NUMINP instead of nfmesh
                RadialBound(1:nfmesh) = fmesh(1:nfmesh)
            END IF
            ! Check to see if evenly spaced cladding nodes were specified
            IF (nunopt(13) == 1) THEN
                ! Evenly spaced cladding nodes
                dum = (cladod - cladid) * 0.5_r8k / (ncmesh - 1)
                RadialBound(nfmesh+1) = cladid / 2.0_r8k
                !
                j = nfmesh + 1
                DO i = 2, ncmesh
                    j = j + 1
                    RadialBound(j) = RadialBound(j-1) + dum
                ENDDO
            ELSE
                ! User specified nodes
                DO i = 1, ncmesh
                    RadialBound(nfmesh+i) = cmesh(i)
                ENDDO
            END IF
            ! Check for central void and force second node to correspond to void
            ! If adjacent node is smaller than the void set error
            IF (nvoid /= 0 .AND. (ABS(RadialBound(2) - rvoid) > 1.0e-10_r8k)) THEN
                ierr = 1
                WRITE(ounit,402) RadialBound(2), rvoid
402             FORMAT(/10x,'second radial node ,',1pe13.4,' is not equal to the void radius',1pe13.4)
            END IF
        END IF
        IF (ncool == 7 .AND. nvoid /= 0 .AND. (ABS(rvoid - RadialBound(2)) > 1.0e-10_r8k)) THEN
            ! Set void radius to second node if necessary
            WRITE(ounit,528) rvoid, RadialBound(2)
528         FORMAT(/,10x,'input specified void radius, ',1pe13.4,' is not equal to second node radius, ',1pe13.4,/, &
              &      10x,'void radius set to second node radius')
            rvoid = RadialBound(2)
        END IF
        ! Compute indexs in dynamic storage for radial nodes at fuel pellet surface and cladding inside surface
        ! set up nodal variables for multi Dimensional calculations
        ! copy axial nodes at which r-theta computations are to be done into NumAzmuthNod array
        ! zero out NumAzmuthNod array
        NumAzmuthNod(1:naxn) = 1
        !
        IF (ndim /= 0) THEN
            NumAzmuthNod(1:nnaz) = naz
            IF (naazp == 2 .AND. azpang(2) <= 0.0_r8k) pazp(3,1) = RadialBound(nfmesh)
        END IF
    END IF
    ! Units
    IF (luout == 1) unit = .FALSE.
    ! Plot file initialization
    IF (ntplot > 0) THEN
        ! Call routine to initialize the cswf graphics file
        tplot = t0
        CALL grafini
    END IF
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    ! problem definition variables
    !
    ! * * * * * initial condition variables * * * * *
    !
    ! nopt(4) = switch for initial conditions input
    !
    IF (ncards == 1) THEN
        ! Define variables which have dimensions dependent on unit conversion subroutine
        ! Pellet Data
        ! Pellet radius deviations
        IF (nPelRadDeviat == 0) THEN
            PelRadDeviat(1:naxn) = 0.0_r8k
        ELSE
            ! Linearly interpolate
            DO k = 1, naxn
                PelRadDeviat(k) = polate (radpel,zelev(k),nPelRadDeviat)
            ENDDO
        ENDIF
        !
        ! Cladding Data
        ! Fast flux profile
        DO k = 1, naxn
            IF (nfastf >= 1) THEN
                FastFlux(k) = cfluxa * polate (fluxz, zelev(k), nfastf)
            ELSE If (ncool == 7) THEN
                ! Constant Value
                FastFlux(k) = cfluxa
            ELSE
                ! Default value, same profile as the first axial power profile
                FastFlux(k) = cfluxa * polate (AxPowProfile(1:2*npaxp,1), zelev(k), npaxp)
            ENDIF
        ENDDO
        ! Cladding plastic hoop strains
        IF (nepp0 == 0) THEN !Initialize hoop strains to 0.0
            CldPlasStrn(1:naxn,1) = 0.0_r8k
        ELSE ! Hoop strains are input
            ! Include cladding intial plastic strain only when the FE-Based mechanical module is used_fraptran
            IF (mechan == 1) THEN
                DO k = 1, naxn
                    CldPlasStrn(k,1) = polate (eppinp, zelev(k), nepp0)
                ENDDO
            ELSE
                CldPlasStrn(1:naxn,1) = 0.0_r8k
            ENDIF
        ENDIF
        ! Check to see if there is an upper plenum
        IF (vplen(1) > 0.0_r8k) THEN ! There is an upper plenum
            ! Subtract spring volume from plenum volume
            dum1 = ncs(1)
            dum2 = spl(1) / dum1
            spvol = sprvol (dum1, swd(1), dum2, scd(1))
            vplen(1) = vplen(1) - spvol
        ENDIF
        ! Check to see if there is a lower plenum
        IF (nbotpl == 1) THEN ! There is a lower plenum
            ! Subtract spring volume from plenum volume
            dum1 = ncolbp
            dum2 = splbp / dum1
            spvol = sprvol (dum1, spdbp, dum2, coldbp)
            volbp = volbp - spvol
        ENDIF
    ENDIF
    
    ! Axial power profile
    IF (npaxp /= 0) THEN
        ia = npaxp * 2
        nm2 = ia - 2
        ! Check cannot be made if input axial top and bottom elevations do not correspond to top and bottom of fuel rod
        DO j = 1, NumAxProfiles 
            sump = 0.0_r8k
            IF (ABS(AxPowProfile(ia,j) - rl) <= 1.0e-3_r8k .AND. AxPowProfile(2,j) <= 1.0e-3_r8k) THEN
                sump = AxPowProfile(1,j) * AxPowProfile(4,j) + AxPowProfile(ia-1,j) * (rl - AxPowProfile(nm2,j))
                IF (nm2 >= 4) THEN
                    DO i = 4, nm2, 2
                        sump = sump + AxPowProfile(i-1,j) * (AxPowProfile(i+2,j) - AxPowProfile(i-2,j))
                    ENDDO
                ENDIF
                sump = 0.5_r8k * sump / rl
                DO i = 1, (ia-1), 2
                    AxPowProfile(i,j) = AxPowProfile(i,j) / sump
                ENDDO
            ENDIF
        ENDDO
        
        ! Print out normalized and interpolated axial power profiles
        WRITE(ounit, 1806)
1806    FORMAT (/10x,'Normalized and interpolated axial power profiles')  
        DO j = 1, NumAxProfiles
            WRITE(ounit, 3077) j 
3077        FORMAT(/15x,'Axial power profile number',i4)
            WRITE(ounit,3073)
3073        FORMAT(15x,3x,' Axial  ',7x,'Axial',/, &
              &    15x,3x,'Distance',7x,'Power',/, &
              &    15x,3x,'(meters)',7x,'Ratio')
            DO k = 1, naxn
                AxialPowr(k) = polate(AxPowProfile(1:2*npaxp,j), AxNodElevat(k), npaxp)
                WRITE(ounit,3072) AxNodElevat(k) / 3.2808_r8k, AxialPowr(k)
3072            FORMAT(15x,1pe13.4,2x,1pe13.4)
            ENDDO
        ENDDO
    END IF
    
    ! Radial power profile
    IF (nradq /= 0 .AND. NFrapconInitialization == 0) THEN
        
        ! Convert input radial power profile (from POWINP) to internal node structure by interpolation
        DO k = 1, naxn
            
            ! Normalize radii to defined fuel radius
            ! (rf = user-defined pellet diameter / 2, fuelrad = user-defined RadPowProfile locations)
            radnorm = (rf / ftmetr) / fuelrad(k,nradq)
            fuelrad(k,1:nradq) = fuelrad(k,1:nradq) * radnorm
            
            ! Set radial power profile min and max bounds
            radsrco(k,1) = radtemp(k,1)
            radsrco(k,igpnod) = radtemp(k,nradq)
            
            ns1 = 1
            ns2 = 2
            DO l = 2, (igpnod - 1)
                rmeter = RadialBound(l) / ftmetr
                Radial_Power_Loop: DO
                    rs1 = fuelrad(k,ns1)
                    rs2 = fuelrad(k,ns2)
                    IF (rs2 >= rmeter) EXIT Radial_Power_Loop
                    ns1 = ns2
                    ns2 = ns2 + 1
                END DO Radial_Power_Loop
                qr1 = radtemp(k,ns1)
                qr2 = radtemp(k,ns2)
                radsrco(k,l) = qr1 + (qr2 - qr1) * (rmeter - rs1) / (rs2 - rs1)
            ENDDO
            
            ! Normalize the radial power profile
            sump = 0.0_r8k
            IF (AxNodElevat(k) > zvoid2 .OR. AxNodElevat(k) < zvoid1) THEN
                nr = 1
            ELSE
                nr = 2
            ENDIF
            DO i = nr, (nfmesh - 1)
                sump = sump + (radsrco(k,i) + radsrco(k,i+1)) * (RadialBound(i+1) ** 2 - RadialBound(i) ** 2)
            ENDDO
            
            IF (AxNodElevat(k) > zvoid2 .OR. AxNodElevat(k) < zvoid1) THEN
                sump = sump / (2.0_r8k * RadialBound(nfmesh) ** 2)
            ELSE
                sump = sump / (2.0_r8k * (RadialBound(nfmesh) ** 2 - rvoid ** 2))
            ENDIF
                
            radsrco(k,1:nfmesh) = radsrco(k,1:nfmesh) / sump
            
            ! Normalize if integral is off by 1%
            radsrco(k,1:nfmesh) = radsrco(k,1:nfmesh) / sump

        END DO
        
        ! Print out radial power profiles
        WRITE(ounit,207)
207     FORMAT(/10x,'Normalized and interpolated radial power profiles in fuel for each axial node')
        DO k = 1, naxn
            WRITE(ounit,2077) k
2077        FORMAT(/,15x,'Axial node:',i4,/, &
              &      15x,'   Radial  ',7x,'Radial',/, &
              &      15x,'  Distance ',7x,'Power ',/, &
              &      15x,'  (meters) ',7x,'Ratio ')
            DO j = 1, nfmesh
                rmeter = RadialBound(j) / ftmetr
                WRITE(ounit,2072) rmeter, radsrco(k,j)
2072            FORMAT(15x,1pe13.4,2x,1pe13.4)
            ENDDO
        ENDDO
        !
    END IF
    
    ! Radial burnup profile
    IF (nbuq /= 0) THEN
        ! Convert input radial burnup profile (from POWINP) to internal node structure by interpolation
        DO k = 1, naxn
            
            ! Normalize radii to defined fuel radius
            ! (rf = user-defined pellet diameter / 2, fuelrad = user-defined RadPowProfile locations)
            radnorm = (rf / ftmetr) / fuelrad(k,nradq)
            fuelrad(k,1:nradq) = fuelrad(k,1:nradq) * radnorm
            
            ! Set burnup profile min and max bounds
            burad(k,1) = buradv(k,1)     
            burad(k,igpnod) = buradv(k,nbuq)
            
            
            ns1 = 1
            ns2 = 2
            DO l = 2, (igpnod - 1)
                rmeter = RadialBound(l) / ftmetr
                Radial_BU_Loop: DO
                    rs1 = bufrad(ns1,k)
                    rs2 = bufrad(ns2,k)
                    IF (rs2 >= rmeter) EXIT Radial_BU_Loop
                    ns1 = ns2
                    ns2 = ns2 + 1
                END DO Radial_BU_Loop
                
                bu1 = buradv(k,ns1)
                bu2 = buradv(k,ns2)
                burad(k,l) = bu1 + (bu2 - bu1) * (rmeter - rs1) / (rs2 - rs1)
            END DO
            
        END DO
        
        ! Print out radial burnup profiles
        WRITE(ounit,407)
407     FORMAT(/10x,' Interpolated radial burnup profiles in fuel for each axial node')
        DO k = 1, naxn
            WRITE(ounit,477) k
477         FORMAT(/15x,'Axial node:',i4,/, &
              &     15x,'   Radial  ',7x,' Burnup  ',/, &
              &     15x,'  Distance ',7x,'(MWd/MTM)',/, &
              &     15x,'  (meters) ')
            DO j = 1, nfmesh
                rmeter = RadialBound(j) / ftmetr
                WRITE(ounit,472) rmeter, burad(k,j)
472             FORMAT(15x,1pe13.4,2x,1pe13.4)
            END DO
        END DO
        
    END IF
    
    ! Check to see if axisymmetric power should be generated
    IF (ndim == 1 .AND. azpang(2) <= 1.0e-10_r8k) THEN
        azpang(1) = 0.0_r8k
        azpang(2) = 180.0_r8k
        naazp = 2
        nrazp = nprad
        !
        DO k = 1, naazp
            pazp(1:2*nprad,k) = RadPowProfile(1:2*nprad)
        ENDDO
    ENDIF
    !
    ! * * * * * boundary condition variables * * * * *
    !
    ! Setup and call sth2xi to initialize the steam-water properties
    l1 = 17000
    ! If htc are prescribed, bypass read of water property tables
    ! To bypass steam tables when h.t.c. input, add following cards.
    IF (nqchn /= 3 .AND. nqchn /= 4) THEN
        CALL sth2xi (aasth, l1)
        IF (l1 < 0) Call errori (2,0)
    ENDIF
    DO k = 1, naxn
        IF (nqbow == 0) ExtentOfBow(k) = 0.0_r8k
        IF (nqbow == 1) ExtentOfBow(k) = polate (bowrat,AxNodElevat(k),nbowr)
    ENDDO
    ! Copy coolant data in a1 array
    IF (ncooli /= 0) THEN
        acond(9,1)  = achn
        acond(10,1) = dhe
        acond(11,1) = dhy
        IF (nqchn == 1) acond(17,1) = hbh(1)
        IF (nqchn == 0) acond(17,1) = hinta(1)
    ENDIF
    ! Smooth input of collapsed liquid level history. Divide into 20 intervals
    IF (nbrliq > 20) THEN
        hlqclp(1) = hlqcl1(1)
        fnbr = 19.0_r8k
        k = 1
        hlqclp(2) = hlqcl1(2)
        k2 = 2 * (nbrliq - 1) + 2
        kconds = 1
        tottl = hlqcl1(k2) - hlqcl1(2)
        deltl = tottl / fnbr
        hl1 = hlqcl1(1)
        tl1 = 0.0_r8k
        LiqLoop1: DO
            tlm = tl1 + deltl
            tls = tl1
            !
            ! hint = integrated collapsed liquid level
            ! integrate from tl1 to tl2
            !
            hint = 0.0_r8k
            kconds = kconds + 1
            LiqLoop2: DO
                k  = k + 1
                k1 = 2 * (k - 1) + 1
                k3 = k1 + 2
                k2 = k1 + 1
                k4 = k2 + 2
                IF (hlqcl1(k4) > tlm) THEN
                    hl2 = polate (hlqcl1,tlm,nbrliq)
                    tl2 = tlm
                ELSE
                    hl2 = hlqcl1(k3)
                    tl2 = hlqcl1(k4)
                ENDIF
                hint = hint + 0.5_r8k * (hl2 + hl1) * (tl2-tl1)
                tl1 = tl2
                hl1 = hl2
                IF (tl2 >= tlm) EXIT LiqLoop2
            END DO LiqLoop2
            kk = 2 * (kconds - 1) + 1
            hlqclp(kk) = hint / (tlm - tls)
            hlqclp(kk+1) = tls + 0.5_r8k * (tlm - tls)
            IF (kconds >= 20) EXIT LiqLoop1
            k = k - 1
        END DO LiqLoop1
        !
        nbrliq = 20
        idat = ABS(nbrliq)
        WRITE(ounit,1976) idat,(hlqclp(i),i = 1,2*idat)
1976    FORMAT(/10x,'Smooth collapsed liquid level history',/, &
            &   15x,'No. of pairs',3x,3(5x,'Level',10x,'Time',6x)/&
            &   30x,3(5x,'(ft)',11x,'(s)',7x)/&
            &   15x,i10,5x,6(2x,1pe13.4)/&
            &  (30x,6(2x,1pe13.4)))
    ELSE IF (nbrliq > 0) THEN
        hlqclp(1:2*nbrliq) = hlqcl1(1:2*nbrliq)
    END IF
    ! Define integrated flecht axial power profile
    nbrfht = nbrpts
    flthit(1:2*nbrfht) = zfapif(1:2*nbrfht)
    !
    IF (ncool /= 7) THEN
        !
        ! heat conduction variables
        ! define heat-1 array POINTER and identfiers
        !
        ! set up POINTERs in heat-1 array
        ! array index          description
        ! -----------          -----------
        ! IndexFinTemp    POINTER to final temperature array
        ! IndexInitTemp     POINTER to initial temperature array
        ! IndexGeom       POINTER to geometry array
        ! IndexBC         POINTER to boundary condition array
        ! IndexThermCon     POINTER to thermal conductivity array
        ! IndexThermConAdv  POINTER to advanced conductivity array
        !
        ! output temperature
        IndexFinTemp = 34 + 8 * (nmesh - 1) + ncmvec
        ! initial temperature array
        IndexInitTemp = 33 + 7 * (nmesh - 1) + ncmvec
        ! geometry array
        IndexGeom = 30 + ncmvec
        ! material property arrays
        IndexThermCon = 35 + 9 * (nmesh - 1) + ncmvec
        ! advanced material property arrays
        IndexThermConAdv = 49 + 11 * (nmesh - 1) + ncmvec
        ! boundary condition array
        IndexBC = 43 + 11 * (nmesh - 1) + ncmvec
        areao = 0.0_r8k
        arean = pi * drod
        iindx = ncmvec + 29
        !
        ! Composition array
        ! Fuel composition
        imaterials(1:nfmesh-1) = 1
        ! Gap
        imaterials(nfmesh) = 3
        ! Cladding
        imaterials(nfmesh+1:nmesh) = 2
        !
        ! radial power source term
        ! compute source terms at each radial node using interpolation
        ! compute weighted source
        !
        ! debug
        ! WRITE(ounit,*) 'nfmesh, nmesh = ',nfmesh, nmesh
        !
        !LOJ qt15: Calculate weights for temperature solution module.
        !  Avoid having a pellet-cladding gap equal to zero, since that
        !  results in a singular value for the area weight.

        ! Minimum pellet clad gap size, depending on surface roughnesses
        cruf = (roughf + roughc) / 2.54_r8k
        gapmin = MAX(cruf, 0.5e-05_r8k) / 12.0_r8k
        !
        DO k = 1, naxn
            iu = 0
            iindx = ncmvec + 5 * (nmesh - 1) + 32
            !
            ! Compute surface weight, and right and left volume weights.
            CALL Weights (nmesh, gapmin, RadialBound, VolumeWeightL(:,k), VolumeWeightR(:,k), AreaWeight(:,k))
            !
            ! Calculate the radial heat source distribution.
            IF (NFrapconInitialization == 0) CALL RadHeatSource (nmesh, nfmesh, nvoid, k, CladdingPower(k), AxNodElevat(k), &
              &                                                  zvoid1, zvoid2, 1.0_r8k, RadialBound, VolumeWeightL, &
              &                                                  VolumeWeightR, radsrco, radsrc )
            !
            ! Copy source into RadialPower
            DO i = 1, nmesh
                !RadialPower(i) = radsrc(k,i)  !Lars commented this out
                PrevIterateTemp(i,k) = DefaultTemp
            ENDDO
            !
        ENDDO
        !
        !
        ! * * * * * Deformation input variables * * * * *
        !
        ! None at present
        !
        ! * * * * * Internal gas pressure variables * * * * *
        !
    END IF
    IF (swllfr /= 0.0_r8k .AND. naxn < 3) THEN
        ierr = 1
        WRITE(ounit,519)
519     FORMAT(/10x,'Gas flow model selected but number of nodes is less than 3')
        ERROR STOP 'Code execution stopped in Subroutine: cominp. Gas flow model selected but number of nodes is < 3.'
    END IF
    mchf = lhtc / 10
    IF (mchf == 7) THEN
        lhtc = lhtc - 30
        WRITE(ounit,5292)
5292    FORMAT(20x,'*** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *',/, &
          &    20x,'*                                                                         *',/, &
          &    20x,'* The macbeth chf correlation is not available in pchf---default is ce-1  *',/, &
          &    20x,'*                                                                         *',/, &
          &    20x,'* * *** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *')
    ENDIF
    IF (nchfmd >= 1) THEN
        ! Store axial nodes with prescribed film boiling
        DO l = 1, nchfmd
            nodchf(l) = ndchfi(l)
            tschf(l) = tschfi(l)
            techf(l) = techfi(l)
        ENDDO
    ENDIF
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    ! End of common block definition
    WRITE(ounit,525)
525 FORMAT(//,' Input common block definition complete'//)
    !
    !
    CONTAINS
        !
        PURE FUNCTION sprvol (f, s, p, c) RESULT (Volume)
        IMPLICIT NONE
        !>@brief
        !> Calculation for the volume of the spring
        REAL(r8k), INTENT(IN) :: f, s, p, c
        REAL(r8k) :: Volume
        !
        Volume = (pi ** 2 / 4.0_r8k) * f * (s ** 2) * (SQRT((p / 2.0_r8k) ** 2 + c ** 2) - s)
        !
        END FUNCTION sprvol
        !
    END SUBROUTINE cominp
    !
    !
    !
    SUBROUTINE initia
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : pi, tfk
    USE functions_fraptran, ONLY : polate
    USE variables_fraptran
    USE Restart_fraptran, ONLY : restfs
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE void_fraptran, ONLY : gapprs
    USE NCGases_fraptran, ONLY : ngases
    IMPLICIT NONE
    !> @brief
    !> This Subroutine initializes all variables in transient calculations
    !
    ! The following arrays can be Dimensioned to length of 1 for relap4 type link with thermal/hydraulic code.
    ! Otherwise, set length to 22
    !
    ! afrap(1) = time (s)
    ! afrap(2) = index for afrap of last written word
    !
    ! Input/Output
    !
    INTEGER(ipk) ::  indxst, lzrout, indxmx, mode, npmax, k, l, ndum1, ndum2, ndum3, Index, kk, &
      &              i, iend, ip1, in1, in2, iu = 1, ip = 1
    REAL(r8k) :: cladthk, nodethk, rsum, porosf, dum3, dum4, sigh, sigz, emodc, &
      &          cpoira, eph, epz, deltu, deltw, dum, dum2, dum5, volgs1, volgs2, gapx0, sump, da, &
      &          dasum, powsum, pnorm, pnormi, totsrc, srcold, srcnew, volwtl, volwtr, volsrc, buave, fuelvol, &
      &          bumwd, xsave, reloc, fbu, pk, pf, Asum, Bsum, Csum, Dsum, CladNodes
    REAL(r8k), PARAMETER :: coba0 = 0.0012_r8k
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), PARAMETER :: patm = 14.7_r8k
    REAL(r8k), DIMENSION(naxn) :: colddec, power
    !
    power = 0.0_r8k
    colddec = 0.0_r8k
    ies(1) = 0
    roi(1) = RadialBound(nmesh)
    ! knonue = axial node (dynamic) at which cladding hoop strain first becomes non-uniform
    knonue = 0
    !
    indxmd = 0
    lengmd = 0
    !
    ! compute incremental azimuthal angle
    SELECT CASE (ndim)
    CASE (1, 3)
        dtheta = 90.0_r8k / (naz - 1)
        IF (nsymm == 1) dtheta = 180.0_r8k / (naz - 1)
        IF (nsymm == 2) dtheta = 360.0_r8k / naz
    CASE DEFAULT
        dtheta = 0.0_r8k
    END SELECT
    !
    rsum = 0.0_r8k
    ncall = 0
    nswpm(1) = 0
    !
    IF (ncool /= 7) Time = t0
    kbot = 1
    zmesh = 0.0_r8k
    zbot = 0.0_r8k
    ! *************************************************************
    ! * Initialize variables needed for the flt-c-set correlation *
    !**************************************************************
    toldfc = 0.0_r8k
    zqflt = 0.0_r8k
    pdeint = 0.0_r8k
    tfoldf = 0.0_r8k
    npaxpf = npaxp
    zqch = 0.0_r8k
    mode = 0
    fuelrd = RadialBound(igpnod)
    ! Assume oxygen concentration of zircaloy = 0.
    zro = 0.0_r8k
    fr = 10.0_r8k
    frchmx = 0.0_r8k
    itdmx = 0
    nitdt = 0
    frchdt = 1.0E30_r8k
    ! permit immediate reset of time step from steady-state value
    numdta = 100
    numdta = 101
    ! ndtred = no. of time step reductions without time step increase
    ndtred = 0
    vsn(1) = 0.0_r8k
    TotalVoidVol = 0.0_r8k
    NSteadyTrans = 1
    npramp = 0
    IF (ncool /= 7) THEN
        ! If power on first time step high enough to cause fuel_fraptran-cladding
        ! contact, internally force power ramp to show gradual closure of gap
        npmax = 2 * nptha
        CALL powrmp (npmax, RodAvePower, t0, dppowi, powimx, powict, nprsw)
    ENDIF
    ! Start new problem at t0
    ntstep = 0
    Time = t0
    TimeIncrement = polate (dtmaxa, Time, ndtmax)
    tpo = t0
    time0 = t0
    !
    PelletRad(1:naxn) = RadialBound(igpnod)
    !
    kflgb = 0
    tc1 = t0
    tc2 = tc1
    !
    TimeofGBSep(1:naxn) = 0.0_r8k
    RhoCp(1:nmesh,1:naxn) = 0.0_r8k
    RhoCp0(1:nmesh,1:naxn) = 0.0_r8k
    SELECT CASE (ncool)
    CASE (7)
        DO k = 1, naxn
            BOSTemp(1:nmesh,k) = BulkCoolTemp(k)
        ENDDO
        FinalTemp(1:nmesh) = BulkCoolTemp(naxn)
        BoundaryCondition(6) = BulkCoolTemp(naxn)
    CASE DEFAULT
        BOSTemp(1:nmesh,1:naxn) = tblka(1,1)
        FinalTemp(1:nmesh) = tblka(1,1)
        BoundaryCondition(6) = tblka(1,1)
    END SELECT
    !
    gpthk0 = RadialBound(ncladi) - RadialBound(igpnod)
    !
    IF (OpenPorosityFraction == 0.0_r8k) THEN
        CALL porcor (frden, porosf)
    ELSE
        porosf = OpenPorosityFraction
    ENDIF
    !
    ! If tgas0 input is greater than zero, compute moles of gas in fuel rod
    IF (tgas0 >= 1.0E-10_r8k .AND. NFrapconInitialization == 0) THEN
        dum3 = 1.0_r8k
        dum4 = 0.0_r8k
        ndum2 = 0
        ndum1 = 0
        ndum3 = 1
        ! Estimate elastic straining of cladding due to pressure loading
        ! Assume atmospheric pressure equals 14.7 psia
        !
        sigh = (gappr0(1) * RadialBound(ncladi) - patm * RadialBound(nmesh)) / (RadialBound(nmesh) - RadialBound(ncladi))
        sigz = (gappr0(1) * RadialBound(ncladi) * RadialBound(ncladi) - patm * RadialBound(nmesh) * &
          &     RadialBound(nmesh)) / (RadialBound(nmesh) * RadialBound(nmesh) - RadialBound(ncladi) * &
          &     RadialBound(ncladi))
        ! Elastic Modulus
        emodc = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=tfk(tgas0), &
          &                  Fluence=zero, ColdWork=coldw, OxygenConcen=zero)
        cpoira = (emodc / (2.0_r8k * (MatProperty (Material='CLAD', Property='SHEAR_MOD', Temperature=tfk(tgas0), &
          &                                        Fluence=zero, ColdWork=coldw, OxygenConcen=zero)))) - 1.0_r8k
        eph = (1.0_r8k / emodc) * (sigh - cpoira * sigz)
        epz = (1.0_r8k / emodc) * (sigz - cpoira * sigh)
        deltu = eph * (0.5_r8k * (RadialBound(ncladi) + RadialBound(nmesh)))
        deltw = epz * rl
        dum3 = RadialBound(ncladi) + deltu
        atop = pi * RadialBound(igpnod) ** 2
        ! Call plenv (vplen(1), zero , deltw, atop, vplenc)
        vplenc = vplen(1) - (0.0_r8k - deltw) * atop
        IF (vplenc < 0.005_r8k * vplen(1)) vplenc = 0.005_r8k * vplen(1)
        !
        tplenb = tgas0
        vplenb = volbp
        DO k = 1, naxn
            GapThick(k) = gpthk0 + deltu + CldPlasStrn(k,1) * (RadialBound(ncladi) + RadialBound(nmesh)) * &
              &           0.5_r8k - PelRadDeviat(k)
            GapTemp(k) = tgas0
            PelletRad(k) = RadialBound(igpnod)
            DishVperL(k) = dishv0 / pelh
            ! If central void at node k, replace dish volume per unit length with central void volume per unit length
            IF (nvoid == 1 .AND. (AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2)) THEN
                CentVoidVol(k) = pi * rvoid ** 2
            ELSE
                CentVoidVol(k) = 0.0_r8k
            END IF
            CrackVolume(k) = 0.0_r8k
            CoolPress(k) = 0.0_r8k
            FuelSurfT(k) = tgas0
            AveDishTemp(k) = tgas0
            GasPress(k) = gappr0(1)
            CrackTemp(k) = tgas0
            VolOpenPor(k) = porosf * pi * RadialBound(igpnod) ** 2
            FuelCenterT(k) = tgas0
        ENDDO
        !
        Index = 5
        CALL gapprs (TotalGasMoles, vplenc, tgas0, GapTemp, GapThick, PelletRad, naxn, dum, &
          &          CrackVolume, CoolPress, AxialNodLen, ndum1, Index, GasMolesAx0(naxn+1), &
          &          FuelSurfT, GasPress, dum2, dum3, dum4, ndum2, ndum3, DishVperL, FuelCenterT, &
          &          dum5, CrackTemp, OpenPorTemp, VolOpenPor, TotalVoidVol, VoidVolumeRatio, &
          &          vplenb, tplenb, BottomPlenumGasMoles, CentVoidVol, roughc, roughf, &
          &          AveDishTemp, TimeofGBSep)
        ! correct rod#
        GasMoles0 = TotalGasMoles
        WRITE(ounit,548) TotalGasMoles
548     FORMAT(/10x,' Number of gram-moles of gas in fuel rod = ',1pe13.6)
        !
        volgs1 = TotalVoidVol * 1728.0_r8k
        volgs2 = TotalVoidVol * 0.28317E8_r8k
        WRITE(ounit,902) volgs1, volgs2
902     FORMAT(/10x,' Cold state void volume of fuel rod      = ',1pe13.6,' inch**3',4x,1pe13.6,' mm**3' )
    ENDIF
    ! Initialize gap pressure array
    DO k = 1, naxn
        NodeSinterTemp(k) = 0
        CladEffStress(k) = 0.0_r8k
        CladYieldStress(k) = 3.0e8_r8k
        IndexPCMI(k) = 0
        IndexPCMIOnce(k) = 0
        OxStAlphaThkNearFuel(k) = 0.0_r8k
        OxStAlphaThkRemain(k) = 0.0_r8k
        OxConcenAAO(k) = coba0
        OxConcenABO(k) = coba0
        OxConcenACO(k) = coba0
        OxConcenADO(k) = coba0
        OxConcenAEO(k) = coba0
        OxConcenAFO(k) = coba0
        OxConcenAGO(k) = coba0
        OxConcenAHO(k) = coba0
        OxConcenAIO(k) = coba0
        OxConcenAAI(k) = 0.0_r8k
        OxConcenABI(k) = 0.0_r8k
        OxConcenACI(k) = 0.0_r8k
        OxConcenADI(k) = 0.0_r8k
        OxConcenAEI(k) = 0.0_r8k
        OxConcenAFI(k) = 0.0_r8k
        OxConcenAGI(k) = 0.0_r8k
        OxConcenAHI(k) = 0.0_r8k
        OxConcenAII(k) = 0.0_r8k
        OxygenConcenAve(k) = 0.0_r8k
        OxygenUptake(k) = 0.0_r8k
        OxiPowerGen(k) = 0.0_r8k
        EffFastFluStrenCoef(k) = FastFlux(k) * tflux
        EffFastFluStrnHardExp(k) = FastFlux(k) * tflux
        ! cwkf is coldwork for strength
        ! it should normally be initialized to 0.5
        EffColdWkStrenCoef(k) = coldw
        ! cwnf is coldwork for ductility
        ! it should normally be initialized to .04
        EffColdWkStrnHardExp(k) = cldwdc
        Vreloc(k) = 0.0_r8k
        HydrogenContent(k) = 0.0_r8k
        IodineContent(k) = 0.0_r8k
        CesiumContent(k) = 0.0_r8k
        ! initialize transient coolant property arrays
        CoolEnthalpy(k) = 0.0_r8k
        CoolEnthalpy0(k) = 0.0_r8k
        CoolDensity(k) = 0.0_r8k
        CoolDensity0(k) = 0.0_r8k
        CoolMassFlx(k) = 0.0_r8k
        AxialPowr(k) = 0.0_r8k
        !
        Enthl(k) = 0.0_r8k
        HFXSum(k) = 0.0_r8k
        EnergyPerL(k) = 0.0_r8k
        CladEnrgPerL(k) = 0.0_r8k
        !
        OpenPorosity(k) = porosf
        RodFailIndex(k) = 0
        RuptFailIndex(k) = 0
        GasPress(k) = gappr0(1)
        GasPress0(k) = GasPress(k)
        DishVperL(k) = dishv0
        CladCollapseIndex(k) = 0
        ! store initial thickness of oxide layer on outside cladding (inches)
        BOSOxideThick(k) = oxideod(k) / 0.0254_r8k
        EOSOxideThick(k) = oxideod(k) / 0.0254_r8k
        OxygenUptake(k) =  oxideod(k) * 5680.0_r8k * 0.2597_r8k
        AlphaThk1(k) = 0.0_r8k
        AlphaThk2(k) = 0.0_r8k
        ! store initial thickness of oxide layer on inside cladding (inches)
        OxiThk1(k) = oxideid(k) / 0.0254_r8k
        OxiThk2(k) = oxideid(k) / 0.0254_r8k
        OxUptakeID2(k) = oxideid(k) * 5680.0_r8k * 0.2597_r8k
        AlphaThk11(k) = 0.0_r8k
        AlphaThk22(k) = 0.0_r8k
        WatrMetlEnrgy(k) = 0.0_r8k
        BulkCoolTemp(k) = 0.0_r8k
        coolqual(k) = 0.0_r8k
        rmassflux(k) = 0.0_r8k
        CldPermStrn0(k) = 0.0_r8k
        HeatFlux(k) = 0.0_r8k
        CrackVolume(k) = 0.0_r8k
        SSHgap(k) = 0.0_r8k
        HeatFlux0(k) = 0.0_r8k
        RInterfacPrs(k) = 0.0_r8k
        SwellDispl(k) = 0.0_r8k
        RInterfacPrs0(k) = RInterfacPrs(k)
        PelletRad0(k) = PelletRad(k)
        CladCollIndx0(k) = 0
        l = 1
        Ihtreg(k) = 1
        Ih(k) = 1
        IF (ncool /= 7) Ichf(k) = lchf
        HtFlxFac(k) = 1.0_r8k
        SurfHtFlux(k) = 0.0_r8k
    ENDDO
    SurfHtFlux(naxn+1) = 0.0_r8k
    ! If not reading from FRAPCON restart file, then set the value for fuel relocation (ureloc) based on the burnup

    IF (NFrapconInitialization == 0) THEN
        ! Set the burnup values
        DO k = 1, naxn
            IF (ncool == 7) THEN
                AxBurnup(k) = bup
            ELSE ! axial burnup profile assumed same as first power profile
                AxBurnup(k) = polate (AxPowProfile(1:2*npaxp,1), AxNodElevat(k), npaxp) * bup
            ENDIF
            Power(k) = polate(RodAvePower, t0, npmax) * polate (AxPowProfile(1:2*npaxp,1), AxNodElevat(k), npaxp)
        ENDDO
        ! Calculate fuel relocation
        SELECT CASE (relocmodel)
        CASE ('FRAPCON-3.3')
            DO k = 1, naxn
                IF (bup > 0.0_r8k) THEN
                    ureloc(k) = 0.45_r8k * gpthk0
                ELSE
                    ureloc(k) = 0.30_r8k * gpthk0
                ENDIF
            ENDDO
        CASE ('FRAPCON-3.4')
            DO k = 1, naxn
                pk = power(k) * 3.2808_r8k ! power(k) is local power, kW/ft. Convert to kW/m
                fbu = MIN(1.0_r8k, ((AxBurnup(k) / 86400.0_r8k) / 5.0_r8k)) ! Convert burnup from MWs/kg to GWd/MTU
                pf = (pk - 20.0_r8k) * 5.0_r8k / 2000.0_r8k
                IF (pk > 40) THEN
                    ureloc(k) = gpthk0 * (0.32_r8k + 0.18_r8k * fbu)
                ELSE IF (pk > 20) THEN
                    ureloc(k) = gpthk0 * (0.28_r8k + pf + (pf + 0.12_r8k) * fbu)
                ELSE
                    ureloc(k) = gpthk0 * (0.3_r8k + 0.10_r8k * fbu)
                ENDIF
            ENDDO
        CASE ('FRAPCON-3.5')
            DO k = 1, naxn
                pk = power(k) * 3.2808_r8k ! power(k) is local power, kW/ft. Convert to kW/m
                ! Set the power dependence on fuel relocation
                IF (pk <= 20.0_r8k) THEN
                    reloc = 0.345_r8k
                ELSE IF (pk <= 40.0_r8k) THEN
                    reloc = 0.345_r8k + (pk - 20.0_r8k) / 200.0_r8k
                ELSE
                    reloc = 0.445_r8k
                ENDIF
                ! Set the burnup dependence on fuel relocation
                IF (AxBurnup(k) > 93.7_r8k) THEN
                    ureloc(k) = gpthk0 * (0.055_r8k + MIN(reloc, reloc * (0.5795_r8k + 0.2447_r8k * &
                      &         LOG(AxBurnup(k) / 86400.0_r8k))))
                ELSE
                    ureloc(k) = gpthk0 * 0.055_r8k
                ENDIF
            ENDDO
        CASE ('OFF')
            DO k = 1, naxn
                ureloc(k) = 0.0_r8k
            ENDDO
        END SELECT
    ENDIF
    ! Check to see if initial gap at all axial nodes
    DO k = 1, naxn
        gapx0 = gpthk0 + CldPlasStrn(k,1) * (0.5_r8k * (RadialBound(ncladi) + RadialBound(nmesh))) - PelRadDeviat(k)
        IF (gapx0 > 0.0_r8k) CYCLE
        WRITE(ounit,68) gapx0
68      FORMAT(' Execution stopped in Subroutine: initia. Gas gap less than zero, gap0(ft) = ',e11.4)
        ERROR STOP 'Execuion stopped in Subroutine: initia. Gas gap less than zero.'
    ENDDO
    !
    gfloa1 = 0.0_r8k
    modfal(1) = 0
    apln0(1) = 0.0_r8k
    bpln0(1) = 0.0_r8k
    bu(1) = bup
    apln(1) = 0.0_r8k
    bpln(1) = 0.0_r8k
    qpln0(1) = 0.0_r8k
    !
    DO l = 1, nodpln
        tplbot(l,1) = BOSTemp(nmesh,1)
        tplna(l,1,1) = BOSTemp(nmesh,naxn)
    ENDDO
    !
    dvdt0(1) = 0.0_r8k
    vs0(1) = 0.0_r8k
    !
    GasMolesAx0(1:naxn) = 0.0_r8k
    !
    GasMolesAx0(naxn+1) = GasMoles0
    GasMolesAx(naxn+1) = GasMoles0
    BottomPlenumGasMoles = GasMoles0
    !
    DO k = 1, naxn
        FuelSrfStrRat(k) = 0.0_r8k
        FuelSrfStrRat0(k) = 0.0_r8k
        PelSrfStrn0(k) = 0.0_r8k
        EDotFZ(k) = 0.0_r8k
        EDotFZ0(k) = 0.0_r8k
        DO l = 1, ldir
            CldResidStrn(k,l) = 0.0_r8k
            FuelResidStrn(k,l) = 0.0_r8k
            IF (l > 1) CldPlasStrn(k,l) = 0.0_r8k
            EffStrain(k) = 0.0_r8k
            CldStress(k,l) = 0.0_r8k
            CldStrn(k,l) = 0.0_r8k
            CldResidStrn0(k,l) = 0.0_r8k
            CldPlasStrn0(k,l) = CldPlasStrn(k,l)
            EffStrain0(k) = 0.0_r8k
            FuelResidStrn0(k,l) = 0.0_r8k
            OldCldAxStrn0(k) = 0.0_r8k
            OldFuelAxStrn0(k) = 0.0_r8k
            OldCladT(k) = tempcs
            OldGasPrs(k) = 0.0_r8k
            OldCoolPrs(k) = 0.0_r8k
            OldPelDis(k) = 0.0_r8k
            BOSGapIndex(k) = 0
            OldCladT0(k) = tempcs
            OldGasPrs0(k) = 0.0_r8k
            OldCoolPrs0(k) = 0.0_r8k
            OldPelDis0(k) = 0.0_r8k
            BOSGapIndex0(k) = 0
            CldStrnRat(k,l) = 0.0_r8k
            CldStrnRat0(k,l) = 0.0_r8k
        ENDDO
    ENDDO
    !
    Csum = 0.0_r8k
    Dsum = 0.0_r8k
    !
    DO k = 1, naxn
        RodOD(k) = 2.0_r8k * RadialBound(nmesh)
        RodOD0(k) = RodOD(k)
        CladMaxT(k) = tempcs
        GapThick(k) = RadialBound(ncladi) - RadialBound(igpnod)
        GapThick0(k) = GapThick(k)
        
        ! Define the 2-D burnup
        IF (ncool == 7) THEN
            AxBurnup(k) = bup
        ELSE
            ! The 2D burnup distribution defined through input variable butemp(r,z) should be used to define the
            ! axial burnup distributionaxial burnup profile assumed same as first power profile
            IF (nbuq > 0) THEN
                Asum = 0.0_r8k
                Bsum = 0.0_r8k
                !
                DO l = 2, igpnod
                    daSum = RadialBound(l) ** 2 - RadialBound(l-1) ** 2
                    Asum = Asum + daSum
                    Bsum = Bsum + daSum * (burad(k,l) + burad(k,l-1)) * 43.2_r8k
                ENDDO
                !
                AxBurnup(k) = Bsum / Asum
                Csum = Csum + AxialNodLen(k)
                Dsum = Dsum + AxialNodLen(k) * AxBurnup(k)
            ELSE
                ! Axial burnup profile assumed same as first power profile
                AxBurnup(k) = polate (AxPowProfile(1:2*npaxp,1), AxNodElevat(k), npaxp) * bup
            END IF
        END IF
        !
        SSHgap(k) = 10000.0_r8k
        CladNodes = REAL(nmesh - ncladi)
        cladthk = RadialBound(nmesh) - RadialBound(ncladi)
        
        ! Reduce cladding thickness for oxidation
        cladthk = cladthk - BOSOxideThick(k) / 12.0_r8k / 1.56_r8k
        nodethk = cladthk / CladNodes
        
        ! LOJ qt15: Modify geometry with regard to initial pellet and cladding deformations (needed for FE model)
        DO l = 1, nmesh
            
            IF (l <= igpnod) THEN
                ! Pellet nodes:
                RadialBoundO(l,k) = RadialBound(l) * (1.0_r8k + PelRadDeviat(k) / RadialBound(igpnod))
            ELSE
                ! Cladding nodes:
                RadialBoundO(l,k) = RadialBound(ncladi) * (1.0_r8k + CldPlasStrn(k,1)) + REAL(l-ncladi) * nodethk
            ENDIF 
            EOSRad(l,k) = RadialBoundO(l,k)
            BOSRad(l,k) = EOSRad(l,k)
        ENDDO

    ENDDO
    ! LOJ qt15: Rod average burnup
    IF (Dsum > 0.0_r8k) bup = Dsum / Csum
    !
    qmaxmelt(1:nmesh) = 1.0e-6_r8k
    qmaxmeltp1(1:nmesh) = 1.0e-6_r8k
    EnrgyMeltp1(1:nmesh,1:naxn) = 0.0_r8k
    EnrgyMeltZp1(1:nmesh,1:naxn) = 0.0_r8k
    EnrgyMelt(1:nmesh,1:naxn) = 0.0_r8k
    EnrgyMeltZ(1:nmesh,1:naxn) = 0.0_r8k
    !
    dcldh = 0.0_r8k
    dcldh0 = dcldh
    delth = 0.0_r8k
    delth0 = delth
    atop = pi * RadialBound(igpnod) ** 2
    !
    ! Compute total number of words in restart Data block
    ! Fracas-2 Data assumed not to be on restart tape. also water prop.
    !
    ldialb = 1
    lphypr = 1
    !
    lcombk = lsclr1 + lsclr2 + lsclr3 + lcolct + lblona + lexcb + lprntb + lflect + &
      &      ldialb + lhtcb + lresr1 + lresi2 + lresi3 + lmatpc + lcoold + 6
    !
    WRITE(ounit,620) lcombk
620 FORMAT(/,20x,'***  Total number of words in restart data block = ',i7,' *** ')
    ! lfixdb = lcombk - la1tot
    !
    ! delete length of fixed DIMENSION common blocks
    ! If calculations to be initialized from FRAPCON-3 calculations, overide prior initialization
    !
    IF (NFrapconInitialization == 1) THEN
        CALL restfs (trest, BOSOxideThick, SwellDispl, CldPlasStrn0, EffStrain0, CladMaxT, OpenPorosity, &
          &          AxBurnup, RadialBound, GasPress, igpnod, GapThick, GasFraction, TotalGasMoles, &
          &          gadolin, burad, radsrco, colddec, ureloc, FastFlux, tflux)
        !
        GasMoles0 = TotalGasMoles
        bup = 0.0_r8k
        DO k = 1, naxn
            EffFastFluStrenCoef(k) = FastFlux(k) * tflux
            EffFastFluStrnHardExp(k) = FastFlux(k) * tflux
            bup = bup + AxBurnup(k) * AxialNodLen(k) / rl
            EOSOxideThick(k) = BOSOxideThick(k)
            oxideod(k) = BOSOxideThick(k) * 0.0254_r8k
            OxygenUptake(k) = EOSOxideThick(k) * 5680.0_r8k * 0.2597_r8k * 0.0254_r8k
            cladnodes = REAL(nmesh - ncladi)
            cladthk = RadialBound(nmesh) - RadialBound(ncladi)
            ! Reduce cladding thickness for oxidation
            cladthk = cladthk - BOSOxideThick(k) / 12.0_r8k / 1.56_r8k
            nodethk = cladthk / cladnodes
            DO l = 1, nmesh
                RadialBoundO(l,k) = RadialBound(l)
                IF (l > ncladi) RadialBoundO(l,k) = RadialBoundO(ncladi,k) + REAL(l-ncladi) * nodethk
                EOSRad(l,k) = RadialBoundO(l,k)
                BOSRad(l,k) = EOSRad(l,k)
            ENDDO
        ENDDO
        bu(1) = bup
        bumtp = bup
        CALL phyprp
        !
        Index = 1
        vplenc = vplen(1) - (delth - dcldh) * atop
        CALL gapprs (TotalGasMoles, vplenc, tgas0, GapTemp, GapThick, PelletRad, naxn, dum, &
          &          CrackVolume, CoolPress, AxialNodLen, ndum1, Index, GasMolesAx0(naxn+1), &
          &          FuelSurfT, GasPress, dum2, dum3, dum4, ndum2, ndum3, DishVperL, FuelCenterT, &
          &          dum5, CrackTemp, OpenPorTemp, VolOpenPor, TotalVoidVol, VoidVolumeRatio, vplenb, &
          &          tplenb, BottomPlenumGasMoles, CentVoidVol, roughc, roughf, AveDishTemp, TimeofGBSep)
        ! Update SwellDispl array to account for colddec value to get correct gap.
        ! inward clad creep is negative, and decreases gap
        SwellDispl(1:naxn) = SwellDispl(1:naxn) - colddec(1:naxn)
        !
        nm(1) = nmesh - 1
        nm(2) = nfmesh
        nm(3) = ncmesh
        DO k = 1, naxn
            ! Initialize radial power source arrays
            ! check integrated value and normalize
            sump = (radsrco(k,nfmesh) + radsrco(k,nfmesh-1)) * rf ** 2
            DO i = 1, (nfmesh - 1)
                sump = sump + (radsrco(k,i) - radsrco(k,i+1)) * RadialBound(i) ** 2
            ENDDO
            sump = sump / (2.0_r8k * rf ** 2)
            IF (ABS(1.0_r8k - sump) >= 0.01_r8k) THEN
                WRITE(ounit,1001) sump
1001            FORMAT(//' In Subroutine: INITIA after call to gapprs, integral of radial profile sums to',e11.4,/, &
                  &      ' Sum should be 1.0. Check input.')
                ! Normalize radial profile
                DO i = 1, nfmesh
                    radsrco(k,i) = radsrco(k,i) / sump
                ENDDO
                WRITE(ounit,1002)
1002            FORMAT(/'*** Radial profile has been normalized to 1.0')
            ENDIF
            
            ! Calculate volumetric source and load into RADSRC array
            !LOJ qt15: Handle these calculations within a separate subroutine.
            CALL Radheatsource (nmesh, nfmesh, nvoid, k, CladdingPower(k), AxNodElevat(k), zvoid1, zvoid2, &
              &                 1.0_r8k, RadialBound, VolumeWeightL, VolumeWeightR, radsrco, radsrc)
        ENDDO
    ENDIF
    ! initialization for user specified fission gas release history
    ! convert rod-average burnup (bup) from MWs/kgM to MWd/kgM
    buave = bup / 86400.0_r8k
    ! fuel volume in cm3; ignore any central hole or dish volume
    fuelvol = pi * ((rl - zvoid2 + zvoid1) * 12.0_r8k * 2.54_r8k) * (RadialBound(igpnod) * 12.0_r8k * 2.54_r8k) ** 2 + &
      &       pi * ((zvoid2 - zvoid1) * 12.0_r8k * 2.54_r8k) * ((RadialBound(igpnod) * 12.0_r8k * 2.54_r8k) ** 2 - &
      &       (rvoid * 12.0_r8k * 2.54_r8k) ** 2)
    ! burnup in MWd
    bumwd = buave * 10.97_r8k * frden * (238.0_r8k / 270.0_r8k) * fuelvol / 1000.0_r8k
    ! produced fission gas in cm3, assume 31cm3/MWd of burnup
    prodXe = 27.1_r8k * bumwd
    prodKr = 3.9_r8k * bumwd
    ! calculate moles of originally specified gas
    DO i = 1, ngases
        gsmol0(i) = GasFraction(i) * GasMoles0
    ENDDO
    !
    GasPress0(1:naxn) = GasPress(1:naxn)
    !
    GasMolesAx0(naxn+1) = GasMoles0
    GasMolesAx(naxn+1) = GasMoles0
    ! If (ithymx > 0) THEN
    ! set azimuthal coolant factor portion of a1
    !
    ! lmax = 6 * ithymx * nvol + ithymx + 1
    !
    ! DO l = 1, lmax
    ! a1(ixazmn+l-1) = afrap(ixazim+l-1)
    ! ENDDO
    !
    ! WRITE(ounit,904) ixazmn, ixazim, lmax
    ! WRITE(ounit,907)
    ! 904 FORMAT(' ixazmn = ',i5,' ixazim = ',i5,' lmax = ',i6)
    ! 907 FORMAT(' azimuthal coolant factors in a1 array - initia ' )
    ! lmax = lmax + ixazmn - 1
    ! WRITE(ounit,906) (a1(l),l = ixazmn,lmax)
    ! 906 FORMAT(8(2x,e11.4))
    !
    ! ENDIF
    ! **************************************************************
    ! * Create an integrated power table for flt-c-set correlation *
    ! **************************************************************
    qaxzq(1) = 0.0_r8k
    qaxzq(2) = 0.0_r8k
    zpkfc = 0.0_r8k
    xsave = AxPowProfile(1,1)
    iEnd = 2 * npaxpf - 1
    !
    DO i = 3, iEnd, 2
        ip1 = i + 1
        in1 = i - 1
        in2 = i - 2
        qaxzq(i) = qaxzq(in2) + ((AxPowProfile(i,1) + AxPowProfile(in2,1)) / 2.0_r8k) * (AxPowProfile(ip1,1) - AxPowProfile(in1,1))
        qaxzq(ip1) = AxPowProfile(ip1,1)
        IF (AxPowProfile(i,1) >= xsave) THEN
            zpkfc = AxPowProfile(ip1,1)
            xsave = AxPowProfile(i,1)
        ENDIF
    ENDDO
    !
    DO i = 1, iEnd, 2
        ip1 = i + 1
        faxzq(i) = AxPowProfile(i,1)
        faxzq(ip1) = AxPowProfile(ip1,1)
    ENDDO
    !
    END SUBROUTINE initia
    !
    !
    !
    SUBROUTINE powrmp (np2, pt, t0, dppowi, powimx, powict, nprsw)
    USE Kinds_fraptran
    USE functions_fraptran, ONLY : polate
    IMPLICIT NONE
    !>@brief
    !> Subroutine powrmp calculates parameters for internally programming power ramp to gradually close gap 
    !> between fuel and cladding on first time step
    !
    ! Input
    !
    ! np     - Number of power-time pairs * 2
    ! pt(l)  - Array of power-time pairs (kW/ft-sec)
    ! t0     - Problem start time (sec)
    !
    ! Output
    !
    ! dppowi
    ! powimx
    ! powict
    !
    INTEGER(ipk), INTENT(IN) :: np2, nprsw
    REAL(r8k), INTENT(IN) :: t0
    REAL(r8k), INTENT(OUT) :: dppowi, powimx, powict
    REAL(r8k), DIMENSION(:), INTENT(IN) :: pt
    !
    dppowi = 0.01_r8k
    powimx = polate(pt, t0, np2/2)
    powict = 1.0e-5_r8k
    IF (dppowi > powimx) dppowi = powimx
    IF (nprsw == 1) dppowi = powimx
    !
    END SUBROUTINE powrmp
    !
    !
    !
    SUBROUTINE thmprp (a, k2, k3, isc, isf)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk, tkf
    USE variables_fraptran, ONLY : ounit, nomat, imatflag, imatflag1, idatapairs, iheattablen, idebug, nsf, nsc, nkf, nkc, &
      &                   fotmtl, ftmelt, fhefus, compmt
    USE scalr_h_fraptran
    USE Material_Properties_fraptran, ONLY : MatProperty
    !>@brief
    !> Subroutine stores input thermal properties in array a. unit is .TRUE. if input units are same as internal units
    !> Original routine on oldpl-t6u67 was replaced with version to be compatiable with new input logic -7/9/80
    !
    ! Input
    !
    ! rhof      - fuel cold-state density (lbf/ft**3)
    ! rhoc      - cladding cold-state density (lbf/ft**3)
    ! bup       - fuel burnup (mw-sec/kg)
    ! frpo2     - Fraction by weight of puo2 in fuel
    ! cnfsol    - thermal conductivity of solid phase of fuel at melting  (btu/s-F-ft)
    ! cnfliq    - liquid phase (btu/s-F-ft)
    ! t0f       - minimum temperature (F) in fuel thermal property table
    ! tmaxf     - minimum temperature (F) in fuel thermal property table
    ! t0c       - minimum temperature (F) in cladding thermal property table
    ! tmaxc     - minimum temperature (F) in cladding thermal property table
    !
    INTEGER(ipk) :: l, npdx, k1, nkf1, naaa, l1, lft1, nkf1p1, l2
    INTEGER(ipk), INTENT(IN) :: isc, isf, k2, k3
    REAL(r8k) :: aaa, bbb, burnup, tktab, ck, tmeltf, dt, t, t1f, f
    REAL(r8k), PARAMETER :: spen = 2326.284_r8k
    REAL(r8k), PARAMETER :: gapcv = 0.012_r8k
    !                   (kg/m3)       (J/kg)/(btu/lbm)
    REAL(r8k), PARAMETER :: dens = 16.018463373959_r8k
    REAL(r8k), PARAMETER :: htcap = 4.1868e3_r8k
    REAL(r8k), PARAMETER :: spenv = 3.7258945807819e4_r8k
    REAL(r8k), PARAMETER :: thcond = 1.6056e-4_r8k
    ! Arrays
    REAL(r8k), DIMENSION(:), INTENT(INOUT) :: a
    REAL(r8k), DIMENSION(2) :: rho
    CHARACTER(LEN=4), DIMENSION(3), PARAMETER :: mat = [ 'Fuel' , 'Clad' , 'Gap ' ]
    !
    aaa = tfk(t0f)
    bbb = tfk(tmaxf)
    idebug = 0
    IF (idebug == 1) THEN
        WRITE(ounit,30)
        WRITE(ounit,32) mat(1), t0f, tmaxf, aaa, bbb, nkf, isf
    ENDIF
    aaa = tfk(t0c)
    bbb = tfk(tmaxc)
    IF (idebug == 1) WRITE(ounit,32) mat(2), t0c, tmaxc, aaa, bbb, nkc, isc
    ! rho(1)=fuel density, rho(2)=cladding density
    rho(1) = rhof
    rho(2) = rhoc
    !
    DO l = 1, 2
        aaa = rho(l) * dens
        IF (idebug == 1) WRITE(ounit,34) mat(l), rho(l), aaa
        IF (l <= 1) THEN
            ! compute fraction of theoretical density of fuel
            aaa = 100.0_r8k * frden
            IF (idebug == 1) WRITE(ounit,36) aaa
        ENDIF
    ENDDO
    !
    rhof = rho(1)
    rhoc = rho(2)
    ! calculate and set pointers for 3 materials
    npdx = 1
    nomat = 3
    k1 = 4 + npdx
    ! nkg = nsg = 1
    ! read volumetric heat capacity of gas gap (btu/ft3.F)
    aaa = gapcv * (htcap * dens)
    IF (idebug == 1) WRITE(ounit,48) gapcv, aaa
    ! specify gap as material 3, preclude melting for gap
    tmelt(3) = 1.0e20_r8k
    hfusn(3) = 1.0_r8k
    ! get: fuel melting temperature, fuel heat of fusion, cladding melting temperature, clad heat of fusion
    ! units       (K)   (J/kg)
    ! convert to    (F)   (btu/ft**3)
    tmelt(1) = ftmelt
    hfusn(1) = fhefus
    ! Cladding properties
    tmelt(2) = MatProperty (Material='CLAD', Property='TMELT', Temperature=0.0_r8k)
    hfusn(2) = MatProperty (Material='CLAD', Property='HEAT_FUSION', Temperature=0.0_r8k)
    ! tmeltk   = tmelt(1)
    DO l = 1, 2
        aaa = tmelt(l)
        tmelt(l) = tkf(tmelt(l))
        hfusn(l) = (hfusn(l) / spen) * rho(l)
        bbb = hfusn(l) * spenv
        WRITE(ounit,40) mat(l), tmelt(l), aaa
        WRITE(ounit,42) mat(l), hfusn(l), bbb
    ENDDO
    WRITE(ounit,43)
43  FORMAT(/)
    ! set flag for all materials to be tabular Data
    imatflag(1)  = 0
    imatflag(2)  = 0
    imatflag(3)  = 0
    imatflag1(1) = 0
    imatflag1(2) = 0
    imatflag1(3) = 0
    ! specify number of Data pairs of k and cp for fuel and cladding
    iDatapairs(1)  = nkf
    iDatapairs(2)  = nkc
    iDatapairs(3)  = 1
    iheattablen(1) = nsf
    iheattablen(2) = nsc
    iheattablen(3) = 1
    ! FUEL THERMAL CONDUCTIVITY AND SPECIFIC HEAT VALUES ARE NOW DIRECTLY CALCULATED 
    ! AT THE POINT OF NEED BY CALLS TO FTHCON AND FCP (e.g., in MAData); 
    ! Therefore, now DO NOT Call FTHCON and FCP in this routine; however, still need temperature values in the array (PNNL, 6)
    ! Find thermal conductivity of solid phase at melting
    burnup = 0.01_r8k
    tktab = ftmelt - 1.0_r8k
    ! use Lucuta model with zero values for burnup and Gd_fraptran
    ck = MatProperty (Material='FUEL', Property='THERMCOND', Temperature=tktab, Burnup=burnup, OMRatio=fotmtl, &
      &               Fraction_TD=frden, Pu=compmt, Gadolinia=0.0_r8k)
    !
    cnfsol = ck * thcond
    !
    tktab = ftmelt + 1.0_r8k
    ! use Lucuta model with zero values for burnup and Gd_fraptran
    ck = MatProperty (Material='FUEL', Property='THERMCOND', Temperature=tktab, Burnup=burnup, OMRatio=fotmtl, &
      &               Fraction_TD=frden, Pu=compmt, Gadolinia=0.0_r8k)
    !
    cnfliq = ck * thcond
    ! generate fuel thermal conductivity table for solid phase
    tmeltf = tkf(ftmelt)
    nkf1 = nkf
    IF (tfk(tmaxf) < ftmelt .OR. nkf <= 10) THEN
        naaa = nkf1 - 1
        dt = (tmaxf - t0f) / naaa
    ELSE
        nkf1 = nkf - 5
        naaa = nkf1 - 1
        dt = (tmeltf - t0f) / naaa
    ENDIF
    l1 = k1 + 2
    ! lft1 + 1 = starting index for temperature table
    lft1 = l1
    ! generate and store fuel conductivity-temperature pairs
    l2 = l1 + nkf
    t = t0f
    !
    DO l = 1, nkf1
        tktab = tfk(t)
        ! convert temp. from F to K. Convert cond. from watts/m.K to btu/sec.ft.F
        ! use Lucuta model with zero values for burnup and Gd_fraptran
        ! Call fthcon (tktab ,frden ,fotmtl ,ck ,dkdt, burnup, gadolin)
        ! f = ck * thcond
        a(l1+l) = t
        a(l2+l) = 0.0_r8k
        t = t + dt
    ENDDO
    IF (nkf1 /= nkf) THEN
        ! generate fuel thermal conductivity table for liquid phase
        nkf1p1 = nkf1 + 1
        naaa = nkf - nkf1p1
        t1f = tmeltf + 1.0_r8k
        dt = (tmaxf - t1f) / naaa
        t = t1f
        !
        DO l = nkf1p1, nkf
            tktab = tfk(t)
            ! use Lucuta model with zero values for burnup and Gd_fraptran
            ! Call fthcon (tktab, frden, fotmtl, ck, dkdt, burnup, gadolin)
            ! f = ck * thcond
            a(l1+l) = t
            a(l2+l) = 0.0_r8k
            t = t + dt
        ENDDO
    ENDIF
    IF (idebug == 1) THEN
        WRITE(ounit,300) nkf, (a(l1+l), l=1,nkf)
        WRITE(ounit,301) mat(1), (a(l2+l), l=1,nkf)
    ENDIF
    !
    l1 = k1 + 3
    l2 = l1 + nkf - 1
    ! generate and store fuel specific heat versus temperature pairs
    l2 = k1 + 2 * nkf + 3
    IF (nsf > 1) THEN
        naaa = nsf - 1
        dt = (tmaxf - t0f)/naaa
        l1 = l2
        l2 = l1 + nsf
    ENDIF
    t = t0f
    !
    DO l = 1,nkf
        ! generate specific heat table with same temperature points as thermal conductivity table
        t = a(lft1+l)
        ! convert temperatures from F to K, and specific heat from (joules/kg-K) to (btu/lbm-F)
        ! frmelt = 0.0_r8k
        ! If (tfk(t) > tmeltk)frmelt=1.
        ! f = fcp (tfk(t), frmelt, fotmtl, frden)/htcap
        ! a(l2+l) = f * rho(1)
        a(l2+l) = 0.0_r8k
    ENDDO
    !
    IF (nsf > 2) THEN
        WRITE(ounit,300) nsf, (a(l1+l), l=1,nsf)
        WRITE(ounit,302) mat(1), (a(l2+l), l=1,isf)
    END IF
    ! generate and store cladding conductivity versus temperature pairs
    aaa = nkc - 1
    dt = (tmaxc - t0c) / aaa
    l1 = k2 + 2
    l2 = l1 + nkc
    t  = t0c
    !
    DO l = 1,nkc
        ! temp (K), flux (n/m**2-sec), time (hours), convert cond. from watts/m.K  to  btu/sec.ft.F
        f = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=tfk(t), Flux=0.0_r8k, ColdWork=coldw) * thcond
        a(l1+l) = t
        a(l2+l) = f
        t = t + dt
    ENDDO
    !
    IF (idebug == 1) THEN
        WRITE(ounit,300) nkc, (a(l1+l), l=1,nkc)
        WRITE(ounit,301) mat(2), (a(l2+l), l=1,nkc)
    ENDIF
    ! generate and store cladding specific heat versus temperature pairs
    l2 = k2 + 2 * nkc + 3
    IF (nsc > 1) THEN
        aaa = nsc - 1
        dt = (tmaxc - t0c) / aaa
        l1 = l2
        l2 = l1 + nsc
    ENDIF
    t = t0c
    !
    DO l = 1, isc
        ! temp (K), specific heat (joules/kg-K)
        f = MatProperty (Material='CLAD', Property='SPECHEAT', Temperature=tfk(t)) / htcap
        IF (nsc > 1) a(l1+l) = t
        a(l2+l) = f * rho(2)
        t = t + dt
    ENDDO
    !
    IF (nsc > 2) THEN
        WRITE(ounit,300) nsc, (a(l1+l), l=1,nsc)
        WRITE(ounit,302) mat(2), (a(l2+l), l=1,isc)
    ENDIF
    !
30  FORMAT(20x, '/ Thermal property data /' )
32  FORMAT(a4,' Property tables in the temperature range ',f7.1,' to ',f7.1,' F (', f17.1,' to ',f7.1,' K)'/' will use ',i4, &
      &       ' points for thermal conductivity, ', i4,' for heat capacity.')
34  FORMAT(/10x,a4,' density                     =',1pe13.4,' lbm/ft**3   ',1pe13.4,' kg/m**3')
36  FORMAT(/10x,'fraction of theoretical density =',1pe13.4)
40  FORMAT(/10x,a4,' melting temperature         =',1pe13.4,' F           ',1pe13.4,' K')
42  FORMAT(/10x,a4,' heat of fusion              =',1pe13.4,' btu/ft**3   ',1pe13.4,' J/m**3')
48  FORMAT(/10x,' gas gap heat capacity          =',1pe13.4,' btu/ft**3.F ',1pe13.4,' J/m**3.K')
300 FORMAT(/ i4, ' points', 41x, 'temperature'/(4e25.16) )
301 FORMAT( 50x, a5,' conductivity' /(4e25.16) )
302 FORMAT( 50x, a5,' heat capacity'/(4e25.16) )
    !
    END SUBROUTINE thmprp
    !
    !
    !
    SUBROUTINE phyprp
    USE Kinds_fraptran
    USE phypro_h_fraptran
    USE variables_fraptran, ONLY : CladType, ounit
    IMPLICIT NONE
    !>@brief
    !> phyprp returns uo2, (u,pu)o2, and zircaloy melting points and heats of fusion, and zirconium
    !> and zircaloy alpha to beta transition temperatures.
    !> This is a MATPRO-11, Rev. 2 routine modified by PNNL for use in FRAPT_fraptran
    !>@author
    !> phypro was coded by v.f. baston in may 1974
    !> modified by c.s. olsen in feb. 1975
    !> modified by b.w. burnham in nov. 1977
    !> modified by d.l. hagrman in june 1979
    !> Modified by PNNL, January 1997, to clean up coding and delete sensitivity uncertainty analysis coding.
    !> Modified by K.J. Geelhood, June 2004 to update the burnup dependance for fuel melting temperature
    !
    ! Input
    !
    ! bumtp    - burnup (mw-s/kg-u)
    ! compmt   - puo2 content (wt%)
    ! deloxy   - oxygen concentration - oxygen concentration of as received cladding (kg oxygen/kg zircaloy)
    ! CladType - Cladding type indicator
    !
    ! Output
    !
    ! ftmelt   - uo2 or mixed oxide fuel melting points (k)
    ! fhefus   - uo2 or mixed oxide fuel heat of fusion (J/kg)
    ! ctmelt   - zr clad melting point (k)
    ! chefus   - zr clad heat of fusion (J/kg).
    ! fdelta   - liquid-solid coexistence temperature range (k)
    !
    ! Reference:
    !
    ! (1) uo2 fuel melting point of 3113k from h.c.brassfield et al gemp-482
    ! (2) uo2 heat of fusion of 17.7 kcal/mole from l.leibowitz et al, j.nuc.mat. 39 p 115 (1971).
    ! (3) Cladding melting point of 2098 k from m.l. picklesimer private communication. 
    ! (4) Cladding heat of fusion of 4.9 kcal/mol from brassfield et al, gemp-482.
    ! (5) beginning and end of alpha-beta transus are from Data in figure iii.33 of anl-76-49
    ! (6) isothermal zirconium alpha-beta transus temperature is 1135 k taken from b.lustman & f.kerze 
    !     "The metallurgy of zirconium" mcgraw-hill book co., new york, 1955
    ! (7) mixed oxide melting point  was obtained from lyon et al, j. nuc. mat., 22 (1967) p 332
    !
    ! Notes:
    !
    ! Burnup dependance for fuel melting temperature changed from 3.2 K/GWd/tHM to 0.5 K/GWd/tHM by K.J.Geelhood in 
    ! June 2004 based on S.G. Popov, et al., "Thermophysical Properties of MOX and UO2 Fuels including the Effects 
    ! of Irradiation," ORNL/TM-200/351
    !
    REAL(r8k) :: c, fbu, wfox
    
    ! Fuel Burnup
    fbu = bumtp / 86.4_r8k
    
    ! Fuel Heat of fusion
    fhefus = 27.4e4_r8k

    ! Fuel melting temperature and fdelta
    SELECT CASE (imox)
    CASE (0) ! UO2 fuel properties
        ftmelt = 3113.15_r8k - 5.0_r8k * fbu / 10000.0_r8k
        fdelta = 1.0e-10_r8k
    CASE (1, 2) ! MOX fuel properties
        ftmelt = sldus(compmt) + 273.15_r8k - 5.0_r8k * fbu / 10000.0_r8k
        fdelta = liqdus(compmt) - sldus(compmt) - 5.0_r8k * fbu / 10000.0_r8k
    CASE DEFAULT
        WRITE (ounit,*) 'Execution terminated in Subroutine: thmprp. Wrong value for Fuel Type. imox =',imox
        ERROR STOP 'Execution terminated in Subroutine: thmprp. Wrong value for Fuel Type'
    END SELECT
    
    ! Cladding melting temperature and heat of fusion
    SELECT CASE (CladType)
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry properties
        ctmelt = 2098.15_r8k
        chefus = 22.5e4_r8k
    CASE (6, 8) ! Zr-1%Nb properties from RRC-KI
        ctmelt = 2133.0_r8k
        chefus = 21.0e4_r8k
    CASE DEFAULT
        WRITE(ounit,*) 'Execution terminated in Subroutine: thmprp. Wrong value for Cladding Type. CladType =',CladType
        ERROR STOP 'Execution terminated in Subroutine: thmprp. Wrong value for Cladding Type'
    END SELECT
    !
    CONTAINS
        !
        PURE REAL(r8k) FUNCTION sldus (PuContent)
        IMPLICIT NONE
        !
        ! Input
        !
        ! PuContent   - Weight % Pu
        !
        REAL(r8k), INTENT(IN) :: PuContent
        !
        sldus = 2840.0_r8k - 5.41395_r8k * PuContent + 7.468390e-3_r8k * PuContent ** 2
        !
        END FUNCTION sldus
        !
        PURE REAL(r8k) FUNCTION liqdus (PuContent)
        IMPLICIT NONE
        !
        ! Input
        !
        ! PuContent   - Weight % Pu
        !
        REAL(r8k), INTENT(IN) :: PuContent
        !
        liqdus = 2840.0_r8k - 3.21860_r8k * PuContent - 1.448518e-2_r8k * PuContent ** 2
        !
        END FUNCTION liqdus
    !
    END SUBROUTINE phyprp
    !
    !
    !
    SUBROUTINE porcor (frden, porosf)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes fuel Open porosity by correlation
    !>@author
    !>
    !
    ! Input
    !
    ! frden  - fuel density (fraction of theoretical maximum density)
    !
    ! Output
    !
    ! porosf - Open porosity of fuel (fraction of theoretical volume)
    !
    REAL(r8k), INTENT(IN) :: frden
    REAL(r8k), INTENT(OUT) :: porosf
    REAL(r8k) :: d
    !
    d = frden * 100.0_r8k
    !
    IF (d <= 92.5_r8k) THEN
        porosf = 16.9297_r8k - 0.232855_r8k * (d - 1.25_r8k) - 8.71836e-4_r8k * (d - 1.25_r8k) ** 2 + &
          &      1.52442e-5_r8k * (d - 1.25_r8k) ** 3
    ELSE IF (d <= 95.25_r8k) THEN
        porosf = 1.20196e-3_r8k * (95.25_r8k - d)
    ELSE
        porosf = 0.0_r8k
    ENDIF
    !
    END SUBROUTINE porcor
    !
END MODULE Initialization_fraptran













