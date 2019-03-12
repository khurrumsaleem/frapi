MODULE Restart_fraptran
    USE Kinds_fraptran
    USE variables_fraptran
    USE rlpst_fraptran
    IMPLICIT NONE
    !>@brief
    !> This module performs FRAPCON_fraptran-to-FRAPTRAN reads and FRAPTRAN-to-FRAPTRAN restart file read/write (Not yet implemented)
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/24/2014
    !
    CONTAINS
    !
    SUBROUTINE Write_Restart
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> This writes the restart file
    INTEGER(ipk) :: runit
    WRITE(runit) 
    !
    END SUBROUTINE Write_Restart
    
    SUBROUTINE Read_Restart
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> This reads the restart file
    END SUBROUTINE Read_Restart
    !
    !
    !
    SUBROUTINE restfs (trest, BOSOxideThick, SwellDispl, CldPlasStrn, EffStrain, CladMaxT, OpenPorosity, &
      &                AxBurnup, RadialBound, GasPress, igpnod, GapThick, GasFraction, TotalGasMoles, &
      &                gadolin, burad, radsrco, colddec, ureloc, FastFlux, tflux)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ngasr, ansr, fmgp, gasavail1, gasavail2, ounit, fcunit
    USE resti_h_fraptran, ONLY : nmesh, ncladi, naxn
    USE NCGases_fraptran, ONLY : ngases
    IMPLICIT NONE
    !>@brief
    !> Read a FRAPCON restart file to initialize history dependent variables that are not specified by card input. 
    !> This subroutine works only with Fracas-1 and will not read any information for Fracas-2
    !>@author
    !> M.E. Cunningham, PNNL, 8/6/1997
    !> Updated by Ian Porter, NRC, 5/4/2016
    !
    ! Input
    !
    ! SwellDispl(k) - Radial displacement of fuel due to densification and fission gas swelling  (ft)
    !
    ! Internal
    !
    ! ldir    - Cladding strain directions. (1 = hoop, 2 = axial, 3 = radial)
    !
    INTEGER(ipk) :: ngasmod, naxfsn, k, nfofs, ncifs, ncofs, j, l, naxnfs, ldir, ns1, ns2, m
    INTEGER(ipk), INTENT(IN) :: igpnod
    INTEGER(ipk), PARAMETER :: FRAPFGR = 3_ipk
    REAL(r8k) :: trecrd, radnorm, rft, rs1, rs2, ts1, ts2, bu1, bu2, qr1, qr2
    REAL(r8k), INTENT(INOUT) :: trest
    REAL(r8k), INTENT(OUT) :: totalgasmoles, tflux
    REAL(r8k), PARAMETER :: em03 = 1.0E-3_r8k
    LOGICAL :: Write_to_Output
    REAL(r8k), DIMENSION(:) :: BOSOxideThick, SwellDispl, EffStrain, CladMaxT, OpenPorosity, AxBurnup, RadialBound, &
      &                        GasPress, GapThick, GasFraction, colddec, ureloc, FastFlux, gadolin
    REAL(r8k), DIMENSION(:,:) :: CldPlasStrn, burad, radsrco
    REAL(ipk), DIMENSION(naxn) :: restfluence
    REAL(r8k), DIMENSION(naxn,3) :: CldPlasStrnFrapcon
    REAL(r8K), DIMENSION(:,:), ALLOCATABLE :: FrapconTemp, radpowo, burado
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: radfs, radfsn, tempfs
    !
    Write_to_Output = .FALSE.
    trecrd = 0.0_r8k

    ! Read through the FRAPCON-to-FRAPTRAN restart file for every FRAPTRAN problem time until the correct timestep 
    ! to read the restart data is reached
    Read_FRAPCON_Restart: DO
        
        ! Restart time (sec)
        READ(fcunit,*) trecrd
        WRITE(ounit,12) trecrd
12      FORMAT('Frapcon initialization file read for time(sec) = ',e13.6)
        
        ! If correct FRAPCON timestep, then write data to output file
        IF (trecrd >= (trest - em03)) Write_to_Output = .TRUE.
        
        ! FRAPCON number of axial nodes
        READ(fcunit,201) naxnfs
        IF (Write_to_Output) WRITE(ounit,905) naxnfs
        IF (naxnfs /= naxn) THEN
            WRITE(ounit,900) naxnfs, naxn
900         FORMAT('FRAPCON initialization file not compatible with FRAPTRAN nodalization.',/,'FRAPCON has ',i3, &
              &    ' axial nodes, while FRAPTRAN has ',i3)
            ERROR STOP 'Number of FRAPCON axial nodes not consistent with number of FRAPTRAN axial nodes'
        ENDIF
        
        ! Oxidation layer thickness at cladding outside surface, inches
        READ(fcunit,*) (BOSOxideThick(k),k = 1,naxn)
        IF (Write_to_Output) WRITE(ounit,910) (BOSOxideThick(k),k = 1,naxn)
        
        ! Excess H2 concenctration in cladding (ppm)
        READ(fcunit,*) (cexh2a(k),k = 1,naxn)
        IF (Write_to_Output) WRITE(ounit,911) (cexh2a(k),k = 1,naxn)
        
        ! Peak historic cladding temp. (K)
        READ(fcunit,*) (CladMaxT(k),k = 1,naxn)
        IF (Write_to_Output) WRITE(ounit,930) (CladMaxT(k),k = 1,naxn)
        
        ! Open porosity at each axial node (fraction of pellet volume)
        READ(fcunit,*) (OpenPorosity(k),k = 1,naxn)
        IF (Write_to_Output) WRITE(ounit,940) (OpenPorosity(k),k = 1,naxn)
        
        ! Fuel burnup  (MW-sec/kg)
        READ(fcunit,*) (AxBurnup(k),k = 1,naxn)
        IF (Write_to_Output) WRITE(ounit,935) (AxBurnup(k),k = 1,naxn)
        
        ! Fast neutron fluence, n/m^2
        READ(fcunit,*) (restfluence(k), k=1,naxn)
        IF (Write_to_Output) WRITE(ounit,936) (restfluence(k), k=1,naxn)
        DO k = 1, naxn
            FastFlux(k) = restfluence(k) / trecrd
        END DO
        tflux = trecrd
        
        ! FRAPCON radial nodes at fuel pellet surface, cladding inside surface, cladding outside surface
        READ(fcunit,*) nfofs, ncifs, ncofs
        IF (Write_to_Output) WRITE(ounit,945) nfofs, ncifs, ncofs
        
        ! Allocate any internal variables based on # of FRAPCON nodes
        ALLOCATE (tempfs(1:ncofs), radfs(1:ncofs), radfsn(1:MAX(igpnod,nfofs)), FrapconTemp(1:nmesh,1:naxn), &
          &       burado(1:naxn,1:nfofs), radpowo(1:naxn,1:nfofs))
        
        ! Gram-moles of gas in fuel rod
        READ(fcunit,*) TotalGasMoles
        IF (Write_to_Output) WRITE(ounit,970) TotalGasMoles
        
        ! Fraction of each component of gas
        ! Assume that nitrogen is set to 0.0 as this was not part of the original restart
        GasFraction(6) = 0.0_r8k
        READ(fcunit,*) (GasFraction(j),j=1,5), GasFraction(7), GasFraction(8)
        IF (Write_to_Output) WRITE(ounit,975) (GasFraction(j),j=1,ngases)
        
        ! Radius to each FRAPCON radial node (ft)
        READ(fcunit,*) (radfs(l),l = 1,ncofs)
        IF (Write_to_Output) WRITE(ounit,950) (radfs(l),l = 1,ncofs)
        
        ! Normalize radii to match FrapTran value
        radnorm = RadialBound(igpnod) / radfs(nfofs)
        DO l = 1, nfofs
            radfsn(l) = radfs(l) * radnorm
        END DO
        
        ! FRACAS-1 initialization info
        
        ! Cladding plastic strains
        DO ldir = 1, 3
            DO k = 1, naxn
                READ(fcunit,*) CldPlasStrnFrapcon(k,ldir)
            END DO
            IF (Write_to_Output) WRITE(ounit,920) (CldPlasStrnFrapcon(k,ldir),k = 1,naxn)
        END DO
        
        ! Cladding effective plastic strain
        DO k = 1, naxn
            READ(fcunit,*) EffStrain(k)
        END DO
        IF (Write_to_Output) WRITE(ounit,925) (EffStrain(k),k = 1,naxn)
        
        ! Radial temperature distribution at each axial node (F)
        DO k = 1,naxn
            
            READ(fcunit,*) (tempfs(l),l = 1,ncofs)
            
            ! By interpolation find temperatures at FRAPTRAN radial nodes
            ns1 = 1
            ns2 = 2
            FrapconTemp(1,k) = tempfs(1)
            !
            DO l = 2, (igpnod - 1)
                
                rft = RadialBound(l)
                
                Temp_TerpLoop: DO
                    rs1 = radfsn(ns1)
                    rs2 = radfsn(ns2)
                    IF (rs2 >= rft) EXIT Temp_TerpLoop
                    ns1 = ns2
                    ns2 = ns2 + 1
                END DO Temp_TerpLoop
                
                ts1 = tempfs(ns1)
                ts2 = tempfs(ns2)
                FrapconTemp(l,k) = ts1 + ((rft - rs1) / (rs2 - rs1)) * (ts2 - ts1)
                
            END DO
            
            FrapconTemp(igpnod,k) = tempfs(nfofs)
            FrapconTemp(ncladi,k) = tempfs(ncifs)
            FrapconTemp(nmesh,k)  = tempfs(ncofs)
            IF (nmesh /= (ncladi+1)) THEN
                DO l = (ncladi + 1), (nmesh - 1)
                    rft = RadialBound(l)
                    FrapconTemp(l,k) = FrapconTemp(ncladi,k) + ((rft - RadialBound(ncladi)) / (RadialBound(nmesh) - &
                      &                RadialBound(ncladi))) * (FrapconTemp(nmesh,k) - FrapconTemp(ncladi,k))
                END DO
            ENDIF
            IF (Write_to_Output) WRITE(ounit,955) (FrapconTemp(m,k),m = 1,nmesh)
            
        END DO
        
        ! Net permanent fuel deformation due to fuel swelling and densification (no relocation); inches, convert to feet
        READ(fcunit,*) (SwellDispl(k),k = 1,naxn)
        ! Convert to inches
        SwellDispl = SwellDispl / 12.0_r8k
        ! Write to output file
        IF (Write_to_Output) WRITE(ounit,915) (SwellDispl(k),k = 1,naxn)
        
        ! Net permanent cladding deformation; inches, convert to feet
        READ(fcunit,*) (colddec(k),k = 1,naxn)
        ! Convert to inches
        colddec = colddec / 12.0_r8k
        ! Write to output file
        IF (Write_to_Output) WRITE(ounit,916) (colddec(k),k = 1,naxn)
        
        ! Permanent fuel relocation displacement; inches, convert to feet
        READ(fcunit,*) (ureloc(k),k = 1,naxn)
        ! Convert to inches
        ureloc = ureloc / 12.0_r8k
        ! Write to output file
        IF (Write_to_Output) WRITE(ounit,917) (ureloc(k),k = 1,naxn)
        
        ! Gadolinia content of fuel (currently only reads for 1 axial node)
        READ(fcunit,*) gadolin(1)
        IF (Write_to_Output) WRITE(ounit,980) gadolin(1)
        ! Apply value to all axial nodes
        gadolin = gadolin(1)
        
        ! Radial burnup profile at each axial node
        IF (Write_to_Output) WRITE(ounit,*) 'Radial burnup profiles from FRAPCON'
        DO k = 1, naxn
            DO l = 1, nfofs
                READ(fcunit,*) burado(k,l)
            END DO
            IF (Write_to_Output) WRITE(ounit,*) 'Axial node ',k,(burado(k,l),l = 1,nfofs)
        END DO
        
        ! Interpolate to define burnup profile at FrapTran nodes
        IF (Write_to_Output) WRITE(ounit,*) 'Radial burnup profiles corrected for FRAPTRAN'
        DO k = 1, naxn
            
            ns1 = 1
            ns2 = 2
            burad(k,1) = burado(k,1)
            burad(k,igpnod) = burado(k,nfofs)

            DO l = 2, (igpnod - 1)
                
                rft = RadialBound(l)
                
                Burnup_TerpLoop: DO
                    rs1 = radfsn(ns1)
                    rs2 = radfsn(ns2)
                    IF (rs2 >= rft) EXIT Burnup_TerpLoop
                    ns1 = ns2
                    ns2 = ns2 + 1
                END DO Burnup_TerpLoop
                
                bu1 = burado(k,ns1)
                bu2 = burado(k,ns2)
                burad(k,l) = bu1 + (bu2 - bu1) * ((rft - rs1) / (rs2 - rs1))
                
            END DO
            
        END DO
        
        IF (Write_to_Output) THEN
            DO k = 1, naxn
                WRITE(ounit,*) 'Axial node ',k,(burad(k,l),l = 1,igpnod)
            END DO
        END IF
        
        ! Radial power profile at each axial node
        IF (Write_to_Output) WRITE(ounit,*) 'Radial power profiles from FRAPCON'
        DO k = 1, naxn
            DO l = 1, nfofs
                READ(fcunit,*) radpowo(k,l)
            END DO
            IF (Write_to_Output) WRITE(ounit,*) 'Axial node ',k,(radpowo(k,l),l = 1,nfofs)
        END DO
        
        ! Interpolate to define radial power profile at FrapTran nodes
        IF (Write_to_Output) WRITE(ounit,*) 'Radial power profile corrected for FRAPTRAN'
        DO k = 1, naxn
            ns1 = 1
            ns2 = 2
            radsrco(k,1) = radpowo(k,1)
            radsrco(k,igpnod) = radpowo(k,nfofs)
            
            DO l = 2, (igpnod - 1)
                
                rft = RadialBound(l)
                
                Power_TerpLoop: DO
                    rs1 = radfsn(ns1)
                    rs2 = radfsn(ns2)
                    IF (rs2 >= rft) EXIT Power_TerpLoop
                    ns1 = ns2
                    ns2 = ns2 + 1
                END DO Power_TerpLoop
                
                qr1 = radpowo(k,ns1)
                qr2 = radpowo(k,ns2)
                radsrco(k,l) = qr1 + (qr2 - qr1) * ((rft - rs1) / (rs2 - rs1))
                
            END DO
            
        END DO
        !
        IF (Write_to_Output) THEN
            DO k = 1, naxn
                WRITE(ounit,*) 'Axial node  ',k,(radsrco(k,l),l = 1,igpnod)
            END DO
        END IF
        
        ! FRAPCON fission gas release model
        READ(fcunit,*) ngasmod
        
        ! Read data only if FRAPCON was run using the FRAPFGR model
        IF (ngasmod == FRAPFGR) THEN
            DEALLOCATE (ansr, gasavail1, gasavail2)
            READ(fcunit,*) ngasr
            ALLOCATE (ansr(1:ngasr))
            ALLOCATE (gasavail1(1:naxn,1:ngasr))
            ALLOCATE (gasavail2(1:naxn,1:ngasr))
            READ(fcunit,*) (ansr(k), k=1,ngasr)
            DO j = 1, naxn
                READ(fcunit,*) (gasavail1(j,k), k=1,ngasr)
                READ(fcunit,*) (gasavail2(j,k), k=1,ngasr)
                READ(fcunit,*) fmgp(j)
            END DO
        ENDIF
        
        ! IP 12/18/15 - The reason I chose to deallocate here instead of have the code skip the allocation after reading the first 
        ! iteration is in case the restart file is ever modified to only print out the refabricated area, and the number of
        ! nodes possibly switching.
        DEALLOCATE (tempfs, radfs, radfsn, FrapconTemp, burado, radpowo)
        
        ! Exit only if the correct time has been read from the FRAPCON restart information
        IF (trecrd >= (trest - em03) ) EXIT Read_FRAPCON_Restart

    END DO Read_FRAPCON_Restart
    !
101 FORMAT (2x,30(e11.4,2x))
201 FORMAT (2x,30(i5,2x))
905 FORMAT (' Number of axial nodes            ',i3)
910 FORMAT (' Zircaloy-oxide thickness (in)    ',/,8(1x,e11.4))
911 FORMAT (' Total cladding H2 (ppm)          ',/,8(1x,e11.4))
915 FORMAT (' Fuel permanent surface disp (ft) ',/,8(1x,e11.4))
916 FORMAT (' Clad permanent surface disp (ft) ',/,8(1x,e11.4))
917 FORMAT (' Fuel permanent reloc disp (ft)   ',/,8(1x,e11.4))
920 FORMAT (" Cladding CldPlasStrn's           ",/,8(1x,e11.4))
925 FORMAT (" Cladding ep's      ",/,8(1x,e11.4))
930 FORMAT (' Maximum cladding average temp (K)',/,8(1x,e11.4))
935 FORMAT (' Burnup (MWs/kg)    ',/,8(1x,e11.4))
936 FORMAT (' Fast fluence (n/m^2',/,8(1x,e11.4))
940 FORMAT (' Open porosity fraction           ',/,8(1x,e11.4))
945 FORMAT (' nfofs,ncifs,ncofs  ',/,3i5)
950 FORMAT (' Radius to each node (ft)         ',/,8(1x,e11.4))
955 FORMAT (' Radial temp. dist (F)            ',/,8(1x,e11.4))
970 FORMAT (' Gm-moles of gas    ',/,1(1x,e11.4))
975 FORMAT (' Mole fractions of gas            ',/,8(1x,e11.4))
980 FORMAT (' Gadolin            ',/,8(1x,e11.4))
    !
    END SUBROUTINE restfs
    !
    END MODULE Restart_fraptran


