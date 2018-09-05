MODULE void
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to calculate void volume and gas pressure.
    !> Subroutines include gspres, volume
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 05/21/2015
    !
    CONTAINS
    !
    SUBROUTINE gspres
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon
    USE Refabrication, ONLY : irefab, fgpavrefab
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called from frpcon and computes the rod internal gas pressure
    !>@author
    !> gspres was coded by g a berna in january 1978.
    !> Modified by Ian Porter, NRC, April 2014
    !
    ! Input
    !
    ! CladIrradGrowStrn - axial strain due to cladding irradiation growth
    ! HotNodLength      - hot node length (in)
    ! hpv               - hot plenum volume (in**3)
    ! ir1               - number of axial nodes plus one
    ! it                - power-time step index
    ! j                 - axial node index
    ! ir1               - number of axial nodes plus one
    ! nt                - number of axial increments
    ! rfnvff            - fuel roughness volume (%)
    ! taf               - average pellet temperature (F)
    ! tag               - average gap temperature (F)
    ! CladAveTemp       - average cladding temperature at level j (F)
    ! PelCentTemp       - centerline temp axial array (F)
    ! tplen             - plenum temperature (F)
    ! totl              - fuel stack height (ft)
    ! tps               - temperature of pellet surface (F)
    ! FuelTempRestruRad - temperature at the restructured radius, rtran (F)
    ! CladVolume        - hot cladding volume per node (cu in)
    ! CrackVolume       - crack volume at each axial node (cu in)
    ! RinterfacVolume   - hot dish plus interface volume per node (cu in) or just interface volume if rc > 0.0 or hdish = 0.0
    ! FuelVolume        - hot fuel volume per node  (cu in)
    ! GapVolume         - hot gap volume per node (cu in)
    ! PorosityVolume    - hot open porosity volume (cu in)
    ! AnnulusVolume     - hot annulus volume (cu in)
    !
    ! Output
    !
    ! gasmo  - moles of gas in the rod (moles)
    ! hcrv   - total hot crack volume (in**3)
    ! hcv    - total hot clad volume (in**3)
    ! hdshv  - total hot dish + interface volume (in**3) or just interface vol. if rc > 0.0 or hdish=0.0
    ! hva    - total annulus volume (in**3)
    ! hfll   - maximum hot fuel stack height (in)
    ! hfper  - increase of fuel stack height (%)
    ! hfv    - total hot fuel volume (in**3)
    ! hgv    - total hot gap volume (in**3)
    ! hporv  - total hot porosity volume (in**3)
    ! pit    - rod internal gas pressure (psia)
    ! press  - rod internal gas pressure (psia)
    ! sumck  - sum of axial crack volumes / crack temperatures (in**3/F)
    ! sumdh  - sum of axial dish volumes / fuel avg. temp. (in**3/F)
    ! sumva  - sum of annulus volumes / center temperature (in**3/F)
    ! sumgp  - sum of axial gap volumes / gap temperatures (in**3/F)
    ! sumpor - sum of axial porosity volumes / porosity temps (in**3/F)
    ! sumrg  - sum of axial roughness volumes / roughness temperature
    ! taca   - radial and axial averaged cladding temperature
    ! tafa   - radial and axial fuel averaged temperature (F)
    ! taga   - radial and axial gap averaged temperature (F)
    ! tsfa   - axial averaged fuel surface temperature (F)
    ! tpca   - axial average centerline temp (F)
    ! tntera - Axial averaged interface temp (F)
    !
    INTEGER(ipk) :: i
    REAL(r8k) :: NodalTotal, sumva, tinter, sumpln, plenumgmles
    !
    NodalTotal = 0.0_r8k
    hfv = 0.0_r8k
    hcv = 0.0_r8k
    hgv = 0.0_r8k
    hdshv = 0.0_r8k
    hva = 0.0_r8k
    hcrv = 0.0_r8k
    hporv = 0.0_r8k
    hfll = 0.0_r8k
    taca(it) = 0.0_r8k
    tsfa(it) = 0.0_r8k
    tafa(it) = 0.0_r8k
    taga(it) = 0.0_r8k
    tntera = 0.0_r8k
    sumdh = 0.0_r8k
    sumgp = 0.0_r8k
    tpca = 0.0_r8k
    sumck = 0.0_r8k
    sumrg = 0.0_r8k
    sumpor = 0.0_r8k
    sumva = 0.0_r8k
    DO i = jmin, jmax
        hfv = hfv + FuelVolume(i-1)
        hcv = hcv + CladVolume(i-1) * (1.0_r8k + CladIrradGrowStrn(i-1))
        hgv = hgv + GapVolume(i-1)
        hdshv = hdshv + RinterfacVolume(i-1)
        hva = hva + AnnulusVolume(i-1)
        hcrv = hcrv + CrackVolume(i-1)
        hporv = hporv + PorosityVolume(i-1)
        hfll = hfll + HotNodLength(i-1)
        taca(it) = taca(it) + CladAveTemp(i-1) * deltaz(i-1) / totl
        tsfa(it) = tsfa(it) + PelSurfTemp(i-1) * deltaz(i-1) / totl
        tafa(it) = tafa(it) + PelAveTemp(i-1) * deltaz(i-1) / totl
        taga(it) = taga(it) + GapAveTemp(i-1) * deltaz(i-1) / totl
        tpca = tpca + PelCentTemp(i-1) * deltaz(i-1) / totl
        tinter = (PelAveTemp(i-1) + GapAveTemp(i-1)) / 2.0_r8k
        tntera = tntera + tinter * deltaz(i-1) / totl
        sumdh = sumdh + RinterfacVolume(i-1) / tfr(PelAveTemp(i-1))
        ! For flat-ended pellets, assign lower temperature to the interface volume between pellets.
        IF (hdish == 0.0_r8k) sumdh = sumdh + RinterfacVolume(i-1) / tfr(tinter)
        IF (rc(i-1) > 0.0_r8k) sumdh = sumdh + RinterfacVolume(i-1) / tfr(tinter)
        !
        sumva = sumva + AnnulusVolume(i-1) / tfr(PelCentTemp(i-1))
        sumgp = sumgp + GapVolume(i-1) / tfr(GapAveTemp(i-1))
        sumck = sumck + CrackVolume(i-1) / tfr((PelSurfTemp(i-1) + FuelTempRestruRad(i-1)) / 2.0_r8k)
        sumrg = sumrg + rfnvff * FuelVolume(i-1) / tfr(GapAveTemp(i-1))
        sumpor = sumpor + PorosityVolume(i-1) / tfr(PelAveTemp(i-1))
        ! Calculate the # of moles at each node
        NodalMoles(i-1) = sumva + sumgp + sumck + sumrg + sumpor + sumdh - NodalTotal
        NodalTotal = NodalTotal + NodalMoles(i-1)
    END DO
    hfper = (hfll / (totl * fttoin) - 1.0_r8k) * 100.0_r8k
    ! Calculation of rod internal pressure
    sumpln = hpv / tfr(tplen)
    NodalTotal = NodalTotal + sumpln
    DO i = jmin, jmax
        NodalGMLES(i-1) = (NodalMoles(i-1) / NodalTotal) * gasmo(it-1)
    END DO
    plenumGMLES = sumpln / NodalTotal * gasmo(it-1)
    NodalGMLES(jmax) = plenumGMLES
    !
    IF (it == irefab + 1) THEN ! Refabrication time step
        pit(it-1) = fgpavrefab
        gasmo(it-1) = pit(it-1) * (sumpln + sumgp + sumdh + sumck + sumpor + sumrg + sumva) / 40.8_r8k
    ELSE
        pit(it-1) = (gasmo(it-1) * 40.8_r8k) / (sumpln + sumgp + sumdh + sumck + sumpor + sumrg + sumva)
    END IF
    !
    press = pit(it-1)
    ! This needs to be checked and possibly removed - IP
    IF (j-1 == nt) j = ir1 + 1
    !
    END SUBROUTINE gspres
    !
    !
    !
    SUBROUTINE volume
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : den, AnnulusVolume, PorosityVolume, GapVolume, FuelVolume, &
      &                   RinterfacVolume, CrackVolume, CladVolume, RinternalVolume, &
      &                   HotNodLength, FuelCladGap, HotThermalGap, ringvol, &
      &                   coldringl, tfuelr, hrad, uo2exp, dpw, densf, hotringl, &
      &                   CladDiamHot, nr, rc, it, j, nrm1, hdish, dphf, fpor1, ftmelt, tcc
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called from frpcon and calculates the void volumes including crack, dish, gap, and open porosity
    !>@author
    !> Changes made to explicitly calculate annulus volumes and pellet inteface volumes by DD Lanning and KJ Geelhood July, 1997.
    !
    ! Input
    !
    ! coldringl       - cold ring length (in)
    ! CladDiamhot     - cladding inside diameter (in)
    ! den             - true density of the fuel ((of theoretical)
    ! densf           - total densification by ring + increment (in/in)
    ! dphf            - diameter of the pellet hot + swelled (in)
    ! dpw             - total outward swelling by ring + increment (in/in)
    ! fpor1           - cold fuel open porosity fraction
    ! ftmelt          - fuel melt temperature (K)
    ! FuelCladGap     - hot radial structural gap (in)
    ! HotThermalGap   - hot thermal gap (in) , diametral
    ! it              - power step index
    ! j               - axial node index (level plus one)
    ! l               - fuel ring index
    ! nr              - maximum number of radial nodes
    ! nrm1            - maximum number of radial nodes minus one
    ! pi              - the constant 3.1415926
    ! hrad            - hot fuel ring radius (in)
    ! tcc             - cold state cladding thickness (in)
    ! tfuelr          - temperature of fuel ring boundary (F)
    ! uo2exp          - fuel thermal expansion per ring (in/in)
    ! hdish           - input dish depth (inches)
    ! rc              - input pellet inner annulus radius (inches)
    !
    ! Output
    !
    ! HotNodLength    - maximum ring length of an increment (in)
    ! hotringl        - hot ring length (in)
    ! RinternalVolume - volume within the cladding for axial node j (cu in)
    ! CladVolume      - hot cladding volume per increment (cu in)
    ! CrackVolume     - hot crack volume per increment (cu in)
    ! RinterfacVolume - hot dish volume  per increment (cu in). For flat-ended pellets, RinterfacVolume is the interface volume.
    ! va              - hot anuulus volume per increment (cu in)
    ! FuelVolume      - hot fuel volume per increment (cu in)
    ! GapVolume       - hot gap volume per increment (cu in)
    ! ringvol         - fuel volume per increment + ring (cu in)
    ! PorosityVolume  - hot open porosity volume (cu in)
    !
    INTEGER(ipk) :: l
    REAL(r8k) :: tmelt, asum, hrlav, vmax, r1, gv, gp, cv, fpor, dcih
    
    ! Convert kelvin to fahrenheit
    tmelt = tkf(ftmelt)
    !
    HotNodLength(j-1) = 0.0_r8k
    FuelVolume(j-1) = 0.0_r8k
    asum = 0.0_r8k
    dcih = CladDiamHot(j-1)
    ! Calculate the nodal hot fuel pellet hot volume, FuelVolume(j-1). 
    ! Note that the coldringl's are already adjusted for the presence of a dish, If any.
    DO l = 1, nrm1
        hotringl(l,j-1) = coldringl(l,j-1) * (1.0_r8k + uo2exp(l,j-1) + dpw(l,j-1) + densf(l,j-1))
        IF (tfuelr(l) >= tmelt .AND. l > 1) hotringl(l,j-1) = hotringl(l-1,j-1)
        HotNodLength(j-1) = MAX(HotNodLength(j-1), hotringl(l,j-1))
        FuelVolume(j-1) = FuelVolume(j-1) + hotringl(l,j-1) * (hrad(l,j-1) ** 2 - hrad(l+1,j-1) ** 2)
        asum = asum + (hrad(l,j-1) ** 2 - hrad(l+1,j-1) ** 2)
    END DO
    ! hrlav is the area-average hot ring length
    hrlav = FuelVolume(j-1) / asum
    FuelVolume(j-1) = FuelVolume(j-1) * pi
    ! FOR DISHED PELLETS (WITH/WITHOUT ANNULUS)
    ! Calculate RinterfacVolume(j-1) as the difference between:
    ! 1) a theoretical cylinder (flat-ended and solid) with length equal to the max. hot nodal ring length
    ! 2) the actual nodal fuel volume FuelVolume(j-1).
    ! Thus RinterfacVolume includes dish volume plus small interface volume.
    ! If there is an annulus, RinterfacVolume at this point also Include the annulus volume.
    ! The annulus volume, va is subtracted off below.
    IF (hdish > 0.0_r8k) THEN
        vmax = pi * hrad(1,j-1) * hrad(1,j-1) * HotNodLength(j-1)
        RinterfacVolume(j-1) = vmax - FuelVolume(j-1)
    END IF
    ! FOR FLAT-ENDED PELLETS (WITH/WITHOUT ANNULUS)
    ! The pellet column is assumed to expand axially by the volume-average strain.  
    ! RinterfacVolume(j-1) is calculated as the interface volume.
    ! If there is an annulus, its volume is calculated explicitly as "va".
    IF (hdish == 0.0_r8k) THEN
        HotNodLength(j-1) = hrlav
        RinterfacVolume(j-1) = 0.0_r8k
        DO l = 1, nrm1
            IF (hotringl(l,j-1) < HotNodLength(j-1)) RinterfacVolume(j-1) = &
              & RinterfacVolume(j-1) + pi * (HotNodLength(j-1) - hotringl(l,j-1)) * (hrad(l,j-1) ** 2 - hrad(l+1,j-1) ** 2)
        END DO
        ! Interface volume = 2 * RinterfacVolume as calculated above.
        RinterfacVolume(j-1) = 2.0_r8k * RinterfacVolume(j-1)
    END IF
    ! Calculate the annulus volume if rc > 0.0
    ! For dished pellets, subtract it from RinterfacVolume so that then RinterfacVolume becomes the interface volume.
    AnnulusVolume(j-1) = 0.0_r8k
    IF (rc(j-1) > 0.0_r8k) AnnulusVolume(j-1) = pi * hrad(nrm1+1,j-1) ** 2 * HotNodLength(j-1)
    IF (hdish > 0.0_r8k) RinterfacVolume(j-1) = RinterfacVolume(j-1) - AnnulusVolume(j-1)
    ! calculation of hot clad and gap volumes
    r1 = MAX(dphf, dcih) * 0.5_r8k
    RinternalVolume(j-1) = pi * r1 * r1 * HotNodLength(j-1)
    gv = pi * HotNodLength(j-1) * (2.0_r8k * dcih * HotThermalGap(j-1) - HotThermalGap(j-1) ** 2) / 4.0_r8k
    gp = (dcih - dphf) * 0.5_r8k
    GapVolume(j-1) = MAX(0.0_r8k, gv)
    cv = pi * HotNodLength(j-1) * (dcih * FuelCladGap(j-1) - FuelCladGap(j-1) ** 2) - GapVolume(j-1)
    CrackVolume(j-1) = MAX(0.0_r8k, cv)
    CladVolume (j-1) = 2.0_r8k * pi * r1 * tcc
    ! calculation of open porosity volume
    PorosityVolume(j-1) = 0.0_r8k
    DO l = 1, nrm1
        IF (tfuelr(l+1) <= 1472.0_r8k) fpor = fpor1
        IF (tfuelr(l) < 1472.0_r8k .AND. tfuelr(l+1) > 1472.0_r8k) fpor = fpor1 * &
          &  (1472.0_r8k - tfuelr(l)) / (tfuelr(l+1) - tfuelr(l)) + 0.5_r8k * fpor1 * &
          &  ((tfuelr(l+1) - 1472.0_r8k) / (tfuelr(l+1) - tfuelr(l)))
        IF (tfuelr(l+1) <= 2012.0_r8k .AND. tfuelr(l) >= 1472.0_r8k) fpor = 0.5_r8k * fpor1
        IF (tfuelr(l) < 2012.0_r8k .AND. tfuelr(l+1) > 2012.0_r8k) fpor = 0.5_r8k * fpor1 * &
          &  ((2012.0_r8k - tfuelr(l)) / (tfuelr(l+1) - tfuelr(l)))
        IF (tfuelr(l+1) <= 2912.0_r8k .AND. tfuelr(l) >= 2012.0_r8k) fpor = 0.0_r8k
        IF (tfuelr(l) < 2912.0_r8k .AND. tfuelr(l+1) > 2912.0_r8k) fpor = 0.0_r8k
        IF (tfuelr(l) >= 2912.0_r8k) fpor = 0.0_r8k
        PorosityVolume(j-1) = PorosityVolume(j-1) + ringvol(l,j-1) * fpor
    END DO
    PorosityVolume(j-1) = MAX(0.0_r8k, PorosityVolume(j-1))
    !
   END SUBROUTINE volume
    !
END MODULE void

