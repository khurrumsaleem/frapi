MODULE FEModel
    !>@brief
    !> This module contains the subroutines used to run the Cladding FEM Model.
    !> Subroutines include mech, create_gascav, del_node, mesh1dto2d,
    !> read_output, remesh_fuel rod,temp_storage
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/10/2016
    USE Kinds
    USE StaticFEA
    USE FEA_IO
    USE FEA_Mesh
    USE FEA_Node
    USE ZrModels, ONLY : ckmn
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE mech(Timefr,TimeIncrement,nce,naxn,ncladi,nmesh,frcoef, &
     RadialBoundO,RadialBound,AxialNodLen,vplen,CladAveTemp,EOSTemp, &
     BulkCoolTemp,GasPress,CoolPress,PelSrfDispl,PelSrfStrn,rpp,rc,GapIndex, &
     GapThick,RInterfacGap,RInterfacPrs,CldStrn,CldPlasStrn,dcldh,dlrel, &
     CldElStrn,PelRadDeviat,pitch,OxygenConcenAve,EffFastFluStrenCoef, &
     EffFastFluStrnHardExp,EffColdWkStrenCoef,EffColdWkStrnHardExp, &
     CldStress,TotalGasMoles,CrackVolume,CrackTemp,VolOpenPor,OpenPorTemp, &
     volbp,tplenb,roughc,roughf,DishVperL,AveDishTemp,RuptFailIndex,EOSRad, &
     CladEffStress,DeformedRadiusOfMesh,CldThermStrn,CldPermStrn,irupt, &
     ruptstrain,nodpln,tplna,irefine,refine,Ifaila,Ifail,RodFailIndex,coefk, &
     coefn,coefm,Emodulus,CladYieldStress,NFrapconInitialization,ntstep, &
     EffStrain)
    USE Kinds
    USE Conversions, ONLY : tfk
    USE common_parameters
    USE fraptran_variables
    USE geometry
    USE pressure1d
    USE pressure2d
    USE Data_grid
    USE cladding
    USE nuclear_fuel
    USE quad4
    USE solid1d
    USE cont1d
    Use Variables, ONLY : CladType
    USE Uncertainty_Vals
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> Driver Subroutine for the finite element model of FRAPTRAN
  INTEGER(ipk) :: &
       nce, &     ! Number of radial cladding elements
       naxn, &    ! Number of axial nodes
       ncladi,nmesh, & ! Indices for RadiaBound arrays
       irupt, &   ! Flag for rupture model
       nodpln, &  ! Number of nodes in upper plenum
       irefine, & ! Use mesh refinement in case of ballooning
       Ifaila, &  ! Ruptured axial cladding node number
       Ifail, &   ! Flag for rupture
       NFrapconInitialization, & ! Flag for FRAPCON3 initialization
       ntstep     ! Number of time step
  INTEGER(ipk), DIMENSION(:) :: & ! (naxn)
       GapIndex, &      ! Gap closure index ( 0=Open / 1=closed )
       RuptFailIndex, & ! Cladding failure index ( 0=not failed )
       RodFailIndex     ! Cladding failure index ( 0=not failed )      
  REAL(r8k) :: &
       Timefr, & ! Analysis time (s)
       TimeIncrement, & ! Time step (s)
       frcoef, & ! Friction coefficient (-)
       vplen, &  ! Plenum volume (ft3)
       cpl, &    ! Cold plenum length (in)
       rpp, &    ! Pellet outer radius (in)
       rc, &     ! Pellet inner radius (in)
       dcldh, &  ! change in length of the cladding (in)
       dlrel, &  ! relative change of the fuel and cladding (in)
       pitch, &  ! Rod pitch in the assembly
       TotalGasMoles, &
       volbp, tplenb, &
       roughc, roughf, & ! Cladding and pellet surface roughness
       ruptstrain, &     ! Rupture hoop strain
       refine            ! Parameter to control mesh refinement
  REAL(r8k), DIMENSION(:) :: & ! (naxn)
       CladYieldStress, &
       EffStrain, & ! Cladding effective plastic strain (in/in)
       Emodulus, &  !
       coefk, &
       coefn, &
       coefm, &
       CldPermStrn, & ! Cladding permanent strain (in/in)
       CrackVolume, &
       CrackTemp, &
       VolOpenPor, &
       OpenPorTemp, &
       DishVperL, &       ! Pellet dish volume
       AveDishTemp, &     ! Pellet dish temperature
       CladEffStress, &   ! Clad effective stress (Psi)
       PelRadDeviat, &    ! Pellet initial 
       OxygenConcenAve, & ! input average oxygen concentration excluding oxide layer
                                ! average oxygen concentration of as received cladding  (kg oxygen/kg zircaloy)
       EffFastFluStrenCoef, &   ! input effective fast fluence for strength coefficient (neutrons/(m**2))
       EffFastFluStrnHardExp, & ! input effective fast fluence for strain hardening exponent (neutrons/(m**2))
       EffColdWkStrenCoef, &    ! input effective cold work for strength coefficient (unitless ratio of areas)
       EffColdWkStrnHardExp, &  ! input effective cold work for strain hardening exponent(unitless ratio of areas)
       RadialBound, &  ! Radial meshing (ft) This array is dimensioned nmesh
       AxialNodLen, &  ! Axial node length (in)  
       CladAveTemp, &  ! Cladding average temperature (F)
       BulkCoolTemp, & ! Coolant temperatures (F)
       GasPress, &     ! Rod inner pressure (Psi)
       CoolPress, &    ! Coolant pressure (Psi)
       PelSrfDispl, &  ! Fuel surface displacement (ft)
       GapThick, &     ! FRAPTRAN array for gap width (ft)
       RInterfacGap, & ! FRAPTRAN array for gap width (ft)
       RInterfacPrs    ! FRAPTRAN array for contact pressure
  REAL(r8k), DIMENSION(:,:) :: & ! (naxn,3)
       CldThermStrn, & ! Clad thermal strains (in/in
       CldStress, &    ! Cladding stresses (Psi)
       PelSrfStrn, &   ! FRAPTRAN array for total fuel strains (in/in)
       CldStrn, &      ! FRAPTRAN array for total cladding strains
       CldPlasStrn, &  ! FRAPTRAN array for plastic strains
       CldElStrn       ! FRAPTRAN array for elastic strains
  REAL(r8k), DIMENSION(:,:) :: & ! (nmesh,naxn)
       EOSRad, &       ! Mesh radii (ft)
       DeformedRadiusOfMesh, & ! Mesh radii (ft)
       RadialBoundO, & ! Radial meshing (ft)
       EOSTemp         ! Nodal temperatures (F)
  REAL(r8k), DIMENSION(6,2,1) :: tplna ! Upper plenum temperature
  INTEGER(ipk) :: i,j,k,label,node,nfound,na,ig
  REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: rad
  REAL(r8k), DIMENSION(3) :: x_node
  REAL(r8k):: u_forced_r,u_forced_ax,mgas,vpT0, &
       V0,Vrc,Vrf,dr,avgtemp,totl,emod,srate,Kpl,npl,mpl
  TYPE(solid1d_type), POINTER :: current_solid1d
  TYPE(cont1d_type), POINTER :: current_cont1d
  TYPE(quad4_type), POINTER :: current_quad4
  TYPE(line_type), POINTER :: current_line

  ! Initialize variables
  na = naxn + 1
  nax = naxn
  quiet = .TRUE.
  max_rn = 1.0e-5_r8k

  ! Time step
  time_End = Timefr
  time0 = Timefr - TimeIncrement
  dtime = TimeIncrement
  IF ( time_End - time0 < 1.0e-10_r8k ) THEN
     time_End = time0 + 1.0e-6_r8k
     dtime = 1.0e-6_r8k
  ENDIF
  dtime0 = dtime
  dtime_output = 1.0e10

  ! Initialize finite element model
  IF ( nnodes == 0 ) THEN
     ! Reference temperature
     tref = 293.15_r8k

     ! use British units
     units = 0

     ! Upper plenum length
     cpl = vplen / (pi * RadialBound(ncladi) ** 2) * 12.0_r8k

     ! Initialize rupture modelling
     failed_cladding = .FALSE.
     rupture_model = irupt
     epsrupt = ruptstrain

     ! Open output files
     CALL fileo()

     ! mesh fuel rod
     IF ( NFrapconInitialization == 0 ) THEN
        CALL mesh_fuel_rod(na,nce,ncladi,nmesh,rc,rpp,RadialBoundO, &
             AxialNodLen,cpl,pitch)
     ELSE
        CALL remesh_fuel_rod(na,nmesh,RadialBoundO,AxialNodLen,cpl,pitch)
        time0 = Timefr - TimeIncrement
     ENDIF

     ! Create temporary storage
     ALLOCATE( elev(na), ut(2,nnodes), tempt(nnodes), epsplt(3,nsolid1d), &
          epsefft(nsolid1d), closedt(ncont1d), stickt(ncont1d), &
          dz0t(ncont1d), prt(npressure1d), rad(nmesh,na), flag2d(naxn), &
          clad_width(naxn),axsliceheight(naxn), clad_OxygenConcenAve(naxn), &
          clad_EffFastFluStrenCoef(naxn), clad_EffFastFluStrnHardExp(naxn), &
          clad_EffColdWkStrenCoef(naxn), clad_EffColdWkStrnHardExp(naxn) )

     ! Initialize flags for mesh refinement
     flag2d = 0

     ! Initialize temporary storage variables
     CALL temp_storage('U')

     ! output initial geometry
     CALL Write_output()

     ! Set input Data grid for the temperature input
     elev(1) = AxialNodLen(1)/2.0_r8k
     rad(1:nmesh,1) = RadialBoundO(1:nmesh,1)*12.0_r8k
     DO i=2,naxn
        elev(i) = elev(i-1) + (AxialNodLen(i-1) + AxialNodLen(i))/2.0_r8k
        rad(1:nmesh,i) = RadialBoundO(1:nmesh,i)*12.0_r8k
     ENDDO
     elev(na) = elev(na-1) + (AxialNodLen(na-1) + cpl)/2.0_r8k
     rad(1:nmesh,na) = RadialBoundO(1:nmesh,naxn)*12.0_r8k
     CALL grid_1d_create(1, na, nmesh, elev, rad)

     ! Save cladding Dimensions
     DO i = 1, naxn
        clad_width(i) = (RadialBoundO(nmesh,i) - RadialBoundO(ncladi,i)) * 12.0_r8k
        axsliceheight(i) = AxialNodLen(i)
     ENDDO
     
  END IF

  ! Set cladding parameters
  lrupture = .TRUE.
  clad_frcoef = frcoef
  clad_OxygenConcenAve(1:naxn) = OxygenConcenAve(1:naxn)
  clad_EffFastFluStrenCoef(1:naxn) = EffFastFluStrenCoef(1:naxn)
  clad_EffFastFluStrnHardExp(1:naxn) = EffFastFluStrnHardExp(1:naxn)
  clad_EffColdWkStrenCoef(1:naxn) = EffColdWkStrenCoef(1:naxn)
  clad_EffColdWkStrnHardExp(1:naxn) = EffColdWkStrnHardExp(1:naxn)
  clad_CladType = CladType
  
  ! Initialize values for steady-state iteration
  IF ( ntstep <= 1 ) Call temp_storage('I')

  ! Create gas cavity entry for the free gas volume
  mgas = TotalGasMoles*73.5632469715213_r8k
  CALL create_gascav(1,mgas)

  ! Gas in the other free volumes
  V0 = 1728.0_r8k*volbp
  vpT0 = V0/tfk(tplenb)
  DO i=1,naxn
     label = i*1000 + 3
     x_node = node_coords(label,1)
     Vrc = 2.0_r8k*pi*x_node(1)*roughc/2.54_r8k*AxialNodLen(i)
     label = i*1000 + 2
     x_node = node_coords(label,1)
     Vrf = 2.0_r8k*pi*x_node(1)*roughf/2.54_r8k*0.01_r8k*AxialNodLen(i)
     vpT0 = vpT0 + AxialNodLen(i)*144.0_r8k* &
          ( CrackVolume(i)/tfk(CrackTemp(i)) + &
          VolOpenPor(i)/tfk(OpenPorTemp(i)) + &
          DishVperL(i)/tfk(AveDishTemp(i)) ) + &
          Vrf/tfk(EOSTemp(ncladi-1,i)) + &
          Vrc/tfk(EOSTemp(ncladi,i))
     V0 = V0 + AxialNodLen(i)*144.0_r8k*( CrackVolume(i) + VolOpenPor(i) + &
          DishVperL(i)) + Vrc + Vrf
  ENDDO
  CALL set_vpt(1,vpT0,V0)

  ! Check for possible ballooning
  IF ( ( irefine == 1 ).AND.( ntstep > 1 ) ) THEN

     ! Refine mesh in hottest slice If the rod is overpressurized
     i = 1
     DO j=2,naxn
        IF ( CladAveTemp(j) > CladAveTemp(i) ) THEN
           i = j
        ENDIF
     ENDDO
     IF ( ( GasPress(i) > CoolPress(i) ).AND.( flag2d(i) == 0 ) ) THEN
        flag2d(i) = 1
        CALL mesh1dto2d(refine,nce,i)
     ENDIF

     ! Refine mesh If the rod is overpressurized and there are large inelastic
     ! deformations nearby
     DO i=1,naxn
        IF ( ( GasPress(i) > CoolPress(i) ).AND.( flag2d(i) == 0 ) ) THEN
           j = MAX(1,i-1)
           k = MIN(naxn,i+1)
           IF ( MAX(CldPlasStrn(i,1),CldPlasStrn(j,1),CldPlasStrn(k,1)) &
                >= 0.02_r8k ) THEN
              flag2d(i) = 1
              CALL mesh1dto2d(refine,nce,i)
           ENDIF
        ENDIF
     ENDDO
  ENDIF

  ! Check for rupture
  Prupt = CoolPress(1)
  DO i=1,naxn
     IF ( RuptFailIndex(i) > 0 ) THEN
        Prupt = CoolPress(i)
        failed_cladding = .TRUE.
        CALL gascav_failure(1,Prupt)
     ENDIF
  ENDDO

  ! Pressure boundary conditions at the rod outer surface
  DO i=1,naxn
     label = i*1000
     CALL pressure1d_setpress(label,CoolPress(i))
     CALL pressure2d_setpress(label,CoolPress(i))
  ENDDO

  ! Pressure at plenum
  DO i=na,na+1
     label = i*1000
     CALL pressure1d_setpress(label,CoolPress(na-1))
     CALL pressure2d_setpress(label,CoolPress(na-1))
  ENDDO

  ! Temperature
  DO i=1,naxn
     DO j=1,nmesh
        first_grid_1d%Data(j,i) = (EOSTemp(j,i) + 459.67_r8k)/1.8_r8k
     ENDDO
  ENDDO

  ! Plenum temperature
  avgtemp = 0.0_r8k
  DO i=1,nodpln
     avgtemp = avgtemp + tplna(i,2,1)
  ENDDO
  avgtemp = avgtemp/nodpln
  first_grid_1d%Data(1:ncladi,na) = (avgtemp + 459.67_r8k)/1.8_r8k
  first_grid_1d%Data(ncladi+1:nmesh,na)=(BulkCoolTemp(naxn)+459.67_r8k)/1.8_r8k

  CALL grid_1d_temp(1)

  ! Forced fuel displacements
  u_forced_ax = 0.0_r8k
  DO i=1,na-1
     node = i*1000+1
     CALL create_displ(node,0.0_r8k)
     node = i*1000+2
     u_forced_r = PelSrfDispl(i)*12.0_r8k
     CALL create_displ(node,u_forced_r)
     node = i*1000+5
     u_forced_ax = u_forced_ax + PelSrfStrn(i,2)*AxialNodLen(i)
     CALL create_displ(node,u_forced_ax)
  ENDDO

  ! Solution for the current time step
  CALL comput_static()

  ! Output gap condition
  current_cont1d => first_cont1d
  DO WHILE ( ASSOCIATED(current_cont1d) )
     i = current_cont1d%label/1000
     IF ( current_cont1d%closed ) THEN
        GapIndex(i) = 1
        GapThick(i) = 0.0_r8k
        RInterfacPrs(i) = Fint(1,current_cont1d%node_numbers(2)) / &
             (2.0_r8k*pi*x(1,current_cont1d%node_numbers(2))*AxialNodLen(i))
     ELSE
        GapIndex(i) = 0
        GapThick(i) = (x(1,current_cont1d%node_numbers(2)) - &
             x(1,current_cont1d%node_numbers(1)))/12.0_r8k
        RInterfacPrs(i) = 0.0_r8k
     ENDIF
     RinterFacGap(i) = GapThick(i)
     current_cont1d => current_cont1d%next
  ENDDO

  ! Total and relative rod length change
  label = na*1000 + 6
  x_node = node_coords(label,2)
  dcldh = x_node(2)/12.0_r8k
  label = (na - 1)*1000 + 5
  x_node = node_coords(label,2)
  dlrel = dcldh - x_node(2)/12.0_r8k

  ! Cladding rupture
  IF ( failed_cladding ) THEN
     totl = 0.0_r8k
     Ifaila = 1
     DO i=1,naxn
        IF ( h_rupt > totl ) THEN
           Ifaila = i
        ELSE
           EXIT
        ENDIF
        totl = totl + AxialNodLen(i)
     ENDDO
     Ifail = 1
     RuptFailIndex(Ifaila) = 1
     RodFailIndex(Ifaila) = 1
  ENDIF

  ! Compute radially averaged values
  DO i=1,na-1
     IF ( flag2d(i) == 1 ) THEN
        ! Average cladding inside diameter at 2D mesh
        label = i*1000 + 3
        current_line => first_line
        DO WHILE ( ASSOCIATED(current_line) )
           IF ( current_line%number == label ) EXIT
           current_line => current_line%next
        ENDDO
        EOSRad(ncladi,i) = 0.0_r8k
        DO j=1,current_line%nelems+1
           label = current_line%nodes(j)
           x_node = node_coords(label,1)
           EOSRad(ncladi,i) = EOSRad(ncladi,i) + x_node(1)
        ENDDO
        EOSRad(ncladi,i) = EOSRad(ncladi,i)/(current_line%nelems + 1)/12.0_r8k

        ! Average cladding outside diameter at 2D mesh
        label = i*1000 + 4
        current_line => first_line
        DO WHILE ( ASSOCIATED(current_line) )
           IF ( current_line%number == label ) EXIT
           current_line => current_line%next
        ENDDO
        EOSRad(nmesh,i) = 0.0_r8k
        DO j=1,current_line%nelems+1
           label = current_line%nodes(j)
           x_node = node_coords(label,1)
           EOSRad(nmesh,i) = EOSRad(nmesh,i) + x_node(1)
        ENDDO
        EOSRad(nmesh,i) = EOSRad(nmesh,i)/(current_line%nelems + 1)/12.0_r8k

        ! Gap width
        label = i*1000 + 2
        x_node = node_coords(label,1)
        GapThick(i) = EOSRad(ncladi,i) - x_node(1)/12.0_r8k
        IF ( GapThick(i) < 1.0e-10_r8k ) THEN
           GapThick(i) = 0.0_r8k
           GapIndex(i) = 1
           RInterfacPrs(i) = 0.0_r8k
        ENDIF
        RinterFacGap(i) = GapThick(i)
     ELSE
        label = i*1000+3
        x_node = node_coords(label,1)
        EOSRad(ncladi,i) = x_node(1)/12.0_r8k
        label = i*1000+4
        x_node = node_coords(label,1)
        EOSRad(nmesh,i) = x_node(1)/12.0_r8k
     ENDIF
     dr = (EOSRad(nmesh,i) - EOSRad(ncladi,i))/(nmesh-ncladi)
     DO j=ncladi+1,nmesh-1
        EOSRad(j,i) = EOSRad(j-1,i) + dr
     ENDDO
     DeformedRadiusOfMesh(ncladi:nmesh,i) = EOSRad(ncladi:nmesh,i)

     ! Change units from in to ft
     AxialNodLen(i) = AxialNodLen(i)/12.0_r8k

     ! Element Data
     CldStrn(i,1:3) = 0.0_r8k
     CldPlasStrn(i,1:3) = 0.0_r8k
     CldStress(i,1:3) = 0.0_r8k
     CldElStrn(i,1:3) = 0.0_r8k
     CldThermStrn(i,1:3) = 0.0_r8k
     CladEffStress(i) = 0.0_r8k
     EffStrain(i) = 0.0_r8k
     Emodulus(i) = 0.0_r8k
     coefk(i) = 0.0_r8k
     coefn(i) = 0.0_r8k
     coefm(i) = 0.0_r8k
     CladYieldStress(i) = 0.0_r8k
     label = i*1000 + 2
     current_solid1d => first_solid1d
     nfound = 0
     DO WHILE( ASSOCIATED(current_solid1d) )
        IF ( current_solid1d%egroup == label ) THEN
           nfound = nfound + 1
           ! Sum up stress and strain values
           CldStrn(i,1:3) = CldStrn(i,1:3) + &
                current_solid1d%epstot((/3,2,1/))
           CldPlasStrn(i,1:3) = CldPlasStrn(i,1:3) + &
                current_solid1d%epspl((/3,2,1/))
           CldStress(i,1:3) =  CldStress(i,1:3) + &
                current_solid1d%sigma((/3,2,1/))
           CldElStrn(i,1:3) = CldElStrn(i,1:3) + &
                current_solid1d%epsel((/3,2,1/))
           CldThermStrn(i,1:3) = CldThermStrn(i,1:3) + current_solid1d%epsth
           CladEffStress(i) = CladEffStress(i) + current_solid1d%sigeff
           EffStrain(i) = EffStrain(i) + current_solid1d%epseff
           ! Sum up material parameter values
            emod = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=current_solid1d%gtemp, &
              &                 ColdWork=EffColdWkStrenCoef(i), Fluence=EffFastFluStrenCoef(i), OxygenConcen=OxygenConcenAve(i))
            Emodulus(i) = Emodulus(i) + emod
            srate = current_solid1d%gamma/dtime
            CALL ckmn (current_solid1d%gtemp,OxygenConcenAve(i), &
                EffFastFluStrenCoef(i),EffFastFluStrnHardExp(i), &
                EffColdWkStrenCoef(i),EffColdWkStrnHardExp(i),srate, &
                Kpl,npl,mpl)
           coefk(i) = coefk(i) + Kpl
           coefn(i) = coefn(i) + npl
           coefm(i) = coefm(i) + mpl
           CladYieldStress(i) = CladYieldStress(i) + Kpl* &
                (current_solid1d%epseff0 + 6.894757e3_r8k* &
                current_solid1d%sigeff/emod)**npl*(srate/0.001_r8k)**mpl
        ENDIF
        IF ( nfound == nce ) EXIT
        current_solid1d => current_solid1d%next
     ENDDO
     current_quad4 => first_quad4
     DO WHILE ( ASSOCIATED(current_quad4) )
        IF ( current_quad4%egroup == label ) THEN
           DO ig=1,4
              nfound = nfound + 1
              CldStrn(i,1:3) = CldStrn(i,1:3) + &
                   current_quad4%epstot((/3,2,1/),ig)
              CldPlasStrn(i,1:3) = CldPlasStrn(i,1:3) + &
                   current_quad4%epstot((/3,2,1/),ig)
              CldStress(i,1:3) =  CldStress(i,1:3) + &
                   current_quad4%sigma((/3,2,1/),ig)
              CldThermStrn(i,1:3) =  CldThermStrn(i,1:3) + &
                   current_quad4%epsth(ig)
              CladEffStress(i) = CladEffStress(i) + current_quad4%sigeff(ig)
              EffStrain(i) = EffStrain(i) + current_quad4%epseff(ig)
              ! Sum up material parameter values
              emod = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=current_quad4%gtemp(ig), &
                &                 ColdWork=EffColdWkStrenCoef(i), Fluence=EffFastFluStrenCoef(i), OxygenConcen=OxygenConcenAve(i))
              Emodulus(i) = Emodulus(i) + emod
              srate = current_quad4%gamma(ig)/dtime
              CALL ckmn (current_quad4%gtemp(ig),OxygenConcenAve(i), &
                   EffFastFluStrenCoef(i),EffFastFluStrnHardExp(i), &
                   EffColdWkStrenCoef(i),EffColdWkStrnHardExp(i),srate, &
                   Kpl,npl,mpl)
              coefk(i) = coefk(i) + Kpl
              coefn(i) = coefn(i) + npl
              coefm(i) = coefm(i) + mpl
              CladYieldStress(i) = CladYieldStress(i) + Kpl* &
                   (current_quad4%epseff0(ig) + 6.894757e3_r8k* &
                   current_quad4%sigeff(ig)/emod)**npl*(srate/0.001_r8k)**mpl
           ENDDO
        ENDIF
        current_quad4 => current_quad4%next
     ENDDO
     ! Average values
     CldStrn(i,1:3) = CldStrn(i,1:3)/nfound
     CldPlasStrn(i,1:3) = CldPlasStrn(i,1:3)/nfound
     CldStress(i,1:3) =  CldStress(i,1:3)/nfound
     CldThermStrn(i,1:3) =  CldThermStrn(i,1:3)/nfound
     CldElStrn(i,1:3) = CldElStrn(i,1:3)/nfound
     CladEffStress(i) = CladEffStress(i)/nfound
     EffStrain(i) = EffStrain(i)/nfound
     CldPermStrn(i) = CldPlasStrn(i,3)
     Emodulus(i) = Emodulus(i)/nfound
     coefk(i) = coefk(i)/nfound
     coefn(i) = coefn(i)/nfound
     coefm(i) = coefm(i)/nfound
     CladYieldStress(i) = CladYieldStress(i)/nfound/6.894757e3_r8k
     ! Multiply by Uncertainty coefficient
     CladYieldStress(i) = CladYieldStress(i) * sigcladyieldstr
  ENDDO
  RETURN

     END SUBROUTINE mech


    SUBROUTINE del_node(label)
    USE Kinds
    USE common_parameters
    IMPLICIT NONE
    !>@brief
    !> Remove node entry from the Database

    INTEGER(ipk), INTENT(IN) :: label
    TYPE(node_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT. ASSOCIATED(first_node) ) RETURN

    ! Mark first node to be deleted in Database
    IF ( first_node%label == label ) THEN
        IF ( enumber(first_node%number) > 0 ) THEN
            WRITE(UNIT=6,FMT='(A,I0,A)') 'WARNING tried to delete node ',label, ' which is used by solid element(s)'
            RETURN
        ENDIF
        lfound = .TRUE.
        tobedeleted => first_node
        first_node => first_node%next
    ENDIF

    ! Search for the to be deleted node and renumber rest of the nodes
    current => first_node
    DO WHILE ( ASSOCIATED(current) )
        IF ( lfound ) THEN
            current%number = current%number - 1
            enumber(current%number) = enumber(current%number + 1)
        ELSE If ( current%label == label ) THEN
            IF ( enumber(current%number) > 0 ) THEN
                WRITE(UNIT=6,FMT='(A,I0,A)') 'WARNING tried to delete node ', &
                    label, ' which is used by solid element(s)'
                RETURN
            ENDIF
            lfound = .TRUE.
            previous%next => current%next
            tobedeleted => current
        ENDIF
        previous => current
        current => current%next
    ENDDO

    ! Set the POINTER to the last node
    last_node => NULL()
    current => first_node
    DO WHILE ( ASSOCIATED(current) )
        x0(1:nd,current%number) = current%x0(1:nd)
        u(1:nd,current%number) = current%u0(1:nd)
        x(1:nd,current%number) = x0(1:nd,current%number) + u(1:nd,current%number)
        v(1:nd,current%number) = current%v0(1:nd)
        a(1:nd,current%number) = current%a0(1:nd)
        node_labels(current%number) = current%label
        last_node => current
        current => current%next
    ENDDO

    ! Deallocate node
    IF ( lfound ) THEN
        nnodes = nnodes - 1
        dof_numbering = .TRUE.
        init_fe = .TRUE.
        DEALLOCATE(tobedeleted)
    ENDIF  

    END SUBROUTINE del_node

    SUBROUTINE mesh1dto2d(refine,ne,ia)
    USE Kinds
    USE common_parameters
    USE geometry
    USE solid1d
    USE quad4
    USE gascav1d
    USE gascav2d
    USE pressure1d
    USE cont1d
    USE cont2d
    USE fraptran_variables
    IMPLICIT NONE
    !>@brief
    !> Replace a SOLID1D mesh with QUAD4 mesh

  INTEGER(ipk), INTENT(IN) :: &
       ne, & ! Number of radial elements
       ia ! Number of axial node to be remeshed
  REAL(r8k), INTENT(IN) :: refine ! Parameter to control mesh refinement
  INTEGER(ipk) :: &
       inode, & ! Cladding ID node label
       onode, & ! Cladding OD node label
       bnode, & ! Cladding Bottom node label
       unode, & ! Cladding Upper node label
       fonode, & ! Fuel OD node label
       fbnode, & ! Fuel Bottom node label
       funode, & ! Fuel Upper node label
       naq4, & ! Number of 2D elements in axial direction
       nr,in,ie,label,nodes(4),ig,id,emat,in0,iprs,oprs,uline,bline
  INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nodels, bnodes, unodes
  REAL(r8k), DIMENSION(:), ALLOCATABLE :: xrad, urad, epseff, grad, temp0, sigeff
  REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: epspl
  REAL(r8k) :: xz(2),uz(2),x_node(3),alpha,u0,prsi0,prso0,x_tmp(3)
  TYPE(solid1d_type), POINTER :: current_s1d
  TYPE(quad4_type), POINTER :: current_q4
  TYPE(line_type), POINTER :: currl
  TYPE(node_type), POINTER :: currn
  TYPE(cont1d_type), POINTER :: currc1d
  TYPE(cont2d_type), POINTER :: currc2d
  TYPE(pressure1d_type), POINTER :: currp1d

  ! Axial element division
  naq4 = axsliceheight(ia)/(refine*clad_width(ia))
  naq4 = MAX(1,naq4)

  ! Node numbers
  inode = ia*1000 + 3
  onode = ia*1000 + 4
  bnode = (ia - 1)*1000 + 6
  unode = ia*1000 + 6

  ! Allocate temporary arrays
  ALLOCATE(nodels(ne+1),bnodes(ne+1),unodes(ne+1),xrad(ne+1),urad(ne+1), &
       epseff(ne),epspl(3,ne),grad(ne),temp0(ne+1),sigeff(ne))

  ! Find all SOLID1D elements and save the displacements, coordinate values,
  ! and plastic deformations to temporary arrays and remove SOLID1D elements
  nr = 1
  in = inode
  ngroup = ia*1000 + 2
  egroup = ia*1000 + 2

  DO ie=1,ne
     current_s1d => first_solid1d
     DO WHILE ( ASSOCIATED(current_s1d) )
        IF ( current_s1d%node_labels(1) == in ) THEN
           emat = current_s1d%mat
           label = current_s1d%label
           nodels(nr) = in
           nodels(nr + 1) = current_s1d%node_labels(2)
           xrad(nr) = x0(1,current_s1d%node_numbers(1))
           urad(nr) = u(1,current_s1d%node_numbers(1))
           temp0(nr) = temp(current_s1d%node_numbers(1))
           xrad(nr + 1) = x0(1,current_s1d%node_numbers(2))
           urad(nr + 1) = u(1,current_s1d%node_numbers(2))
           temp0(nr + 1) = temp(current_s1d%node_numbers(2))
           grad(nr) = current_s1d%x0(1)
           xz(1) = x0(2,current_s1d%node_numbers(3))
           xz(2) = x0(2,current_s1d%node_numbers(4))
           uz(1) = u(2,current_s1d%node_numbers(3))
           uz(2) = u(2,current_s1d%node_numbers(4))
           epseff(nr) = current_s1d%epseff0
           epspl(1:3,nr) = current_s1d%epspl0(1:3)
           sigeff(nr) = current_s1d%sigeff0
           nr = nr + 1
           in = current_s1d%node_labels(2)
           EXIT
        ENDIF
        current_s1d => current_s1d%next
     ENDDO
     CALL solid1d_delete(label)
  ENDDO

  ! Delete contact element for pellet/cladding contact
  label = ia*1000
  CALL cont1d_delete(label)

  ! Delete contact element for exessive plastic deformation
  label = ia*1000 + 1
  CALL cont2d_delete(label)

  ! Delete pressure boundary conditions
  oprs = ia*1000
  CALL pressure1d_delete(oprs)

  ! Delete gas gap cavity element
  label = ia*1000
  CALL gascav1d_delete(label)

  ! remove nodes at cladding
  DO in=1,ne+1
     CALL del_node(nodels(in))
  ENDDO

  CALL update_Database()

  ! Create new corner nodes and lines bounding the QUAD4 mesh
  IF ( .NOT.exists_line(bnode) ) THEN
     nodes(1) = max_node + 1
     x_node = (/ xrad(1), xz(1), 0.0_r8k /)
     CALL create_node(nodes(1),x_node)
     nodes(2) = max_node + 1
     x_node = (/ xrad(ne+1), xz(1), 0.0_r8k /)
     CALL create_node(nodes(2),x_node)
     CALL create_line(bnode,nodes,ne)
     IF ( ia > 1 ) THEN
        CALL create_coupled(bnode,bnode,2)
        CALL create_lcoupled(2,bnode,bnode)
     ELSE
        CALL create_lfixed(2,bnode,1)
     ENDIF
  ENDIF
  IF ( .NOT.exists_line(unode) ) THEN
     nodes(1) = max_node + 1
     x_node = (/ xrad(1), xz(2), 0.0_r8k /)
     CALL create_node(nodes(1),x_node)
     nodes(2) = max_node + 1
     x_node = (/ xrad(ne+1), xz(2), 0.0_r8k /)
     CALL create_node(nodes(2),x_node)
     CALL create_line(unode,nodes,ne)
     CALL create_coupled(unode,unode,2)
     CALL create_lcoupled(2,unode,unode)
  ENDIF
  currl => first_line
  DO WHILE ( ASSOCIATED(currl) )
     IF ( currl%number == bnode ) THEN
        nodes(1) = currl%nodes(1)
        nodes(3) = currl%nodes(currl%nelems+1)
     ENDIF
     IF ( currl%number == unode ) THEN
        nodes(2) = currl%nodes(1)
        nodes(4) = currl%nodes(currl%nelems+1)
     ENDIF
     currl => currl%next
  ENDDO

  CALL create_line(inode,nodes(1:2),naq4)

  CALL create_line(onode,nodes(3:4),naq4)
  CALL create_lpress(oprs,onode)

  fonode = ia*1000 + 2
  fbnode = (ia - 1)*1000 + 5
  funode = ia*1000 + 5

  x_node(3) = 0.0_r8k
  x_tmp = node_coords(fonode,0)
  x_node(1) = x_tmp(1)
  x_tmp = node_coords(fbnode,0)
  x_node(2) = x_tmp(2)
  nodes(3) = max_node + 1
  CALL create_node(nodes(3),x_node)

  x_tmp = node_coords(funode,0)
  x_node(2) = x_tmp(2)
  nodes(4) = max_node + 1
  CALL create_node(nodes(4),x_node)

  CALL create_line(fonode,nodes(3:4),naq4)
  CALL create_lcoupled(1,fonode,fonode)
  CALL create_coupled(fonode,fonode,1)
  IF ( ia > 1 ) THEN
     CALL create_lcoupled(2,fonode,fbnode)
     CALL create_coupled(fbnode,fbnode,2)
  Else
     CALL create_lfixed(2,fonode,1)
  ENDIF

  bline = ia*1000 + 7
  uline = ia*1000 + 8

  CALL create_line(bline,nodes(1:3:2),1)
  CALL create_line(uline,nodes(2:4:2),1)

  CALL update_Database()

  ! Create nodes and QUAD4 elements
  CALL meshq4(inode,bnode,onode,unode,emat,0)

  ! Create gas cavity
  CALL meshq4(inode,uline,fonode,bline,1,2)

  ! Define contact elements that prevents exessive plastic deformation
  IF ( exists_line(1) ) THEN
     label = ia*1000 + 1
     CALL create_lcont2(label,1001,1,onode)
  ENDIF

  ! Create contact elements between pellet stack and cladding
  label = ia*1000
  CALL create_lcont2(label,emat,fonode,inode)

  ! Interpolate plastic strain Data from SOLID1D elements to QUAD4 elements
  CALL quad4_interp(ne,epspl,epseff,sigeff,grad)

  ! Interpolate displacement Data from SOLID1D mesh to QUAD4 mesh
  currn => first_node
  DO WHILE ( ASSOCIATED(currn) )
     IF ( currn%ngroup == ngroup ) THEN
        ! Axial displacements
        IF ( currn%dof_status(2) >= 0 ) THEN
           alpha = (x0(2,currn%number) - xz(1))/(xz(2) - xz(1))
           currn%u0(2) =  (1.0_r8k - alpha)*uz(1) + alpha*uz(2)
        ENDIF
        ! Radial displacements and temperatures
        IF ( currn%dof_status(1) >= 0 ) THEN
           IF ( currn%x0(1) <= xrad(1) ) THEN
              currn%u0(1) = urad(1)
              currn%temp0 = temp0(1)
           ELSE If ( currn%x0(1) >= xrad(ne+1) ) THEN
              currn%u0(1) = urad(ne+1)
              currn%temp0 = temp0(ne+1)
           ELSE
              DO in=2,ne+1
                 IF ( xrad(in) > currn%x0(1) ) THEN
                    alpha = (currn%x0(1) - xrad(in - 1)) / &
                         (xrad(in) - xrad(in - 1))
                    currn%u0(1) =  (1.0_r8k - alpha)*urad(in - 1) + &
                         alpha*urad(in)
                    currn%temp0 = (1.0_r8k - alpha)*temp0(in - 1) + &
                         alpha*temp0(in)
                    EXIT
                 ENDIF
              ENDDO
           ENDIF
        ENDIF
     ENDIF
     currn => currn%next
  ENDDO

  ! Deallocate temporary arrays
  DEALLOCATE(nodels,bnodes,unodes,xrad,urad,epseff,epspl,grad,temp0,sigeff)

    END SUBROUTINE mesh1dto2d

        SUBROUTINE read_output (numout, nout_unit)
    USE Kinds
    USE common_parameters
    USE materials_frap
    USE geometry
    USE spring
    USE quad4
    USE hex8
    USE solid1d
    USE pressure1d
    USE pressure2d
    USE pressure3d
    USE gascav1d
    USE gascav2d
    USE gascav3d
    USE cont1d
    USE cont2d
    USE cont3d
    IMPLICIT NONE
    !>@brief
    !> Read restart file for the FE model from output unit
  INTEGER(ipk), INTENT(IN) :: numout,nout_unit
  INTEGER(ipk) :: i,label,number,id,noutput0
  REAL(r8k) :: node_x0(3),mgas,p,V0_gc,VpT0_gc,V_tot_gc
  LOGICAL :: failed
  TYPE(node_type), POINTER :: current_node,previous_node
  TYPE(coupled_set), POINTER :: current_coupled,previous_coupled
  TYPE(gascav_type), POINTER :: current_gascav,previous_gascav

  Do
     ! Read output number
     Read(UNIT=nout_unit,ERR=20,End=10) noutput0

     noutput = noutput0

     ! Read analysis type
     Read(UNIT=nout_unit,ERR=20,End=20) analys
     Read(UNIT=nout_unit,ERR=20,End=20) dimens
     Read(UNIT=nout_unit,ERR=20,End=20) nd
     Read(UNIT=nout_unit,ERR=20,End=20) units

     ! Read general Data
     Read(UNIT=nout_unit,ERR=20,End=20) time0
     Read(UNIT=nout_unit,ERR=20,End=20) tref
     Read(UNIT=nout_unit,ERR=20,End=20) grav0

     ! WRITE FE model energies
     Read(UNIT=nout_unit,ERR=20,End=20) elastic_strain_energy
     Read(UNIT=nout_unit,ERR=20,End=20) plastic_strain_energy
     Read(UNIT=nout_unit,ERR=20,End=20) kinetic_energy

     ! Read nodal Data
     Read(UNIT=nout_unit) number
     IF ( number > n_array ) THEN
        n_array = number + 100
        IF ( ALLOCATED(x0) ) DEALLOCATE(x0,x,u,v,a,temp,Fgrav,Fext,Fint, &
             enumber,dof_number,node_labels,du,fres,Real_tmp,int_tmp)
        ALLOCATE(x0(nd,n_array),x(nd,n_array),u(nd,n_array),v(nd,n_array), &
             a(nd,n_array),temp(n_array),Fgrav(nd,n_array),Fext(nd,n_array), &
             Fint(nd,n_array),enumber(n_array),dof_number(nd,n_array), &
             node_labels(n_array),du(nd*n_array),fres(nd*n_array), &
             Real_tmp(nd*n_array),int_tmp(nd*n_array))
     ENDIF

     current_node => first_node
     nnodes = 0
     nfixed = 0
     DO i=1,number
        Read(UNIT=nout_unit,ERR=20,End=20) label
        node_x0 = 0.0_r8k
        Read(UNIT=nout_unit,ERR=20,End=20) node_x0(1:nd)
        IF ( ASSOCIATED(current_node) ) THEN
           ! OverWrite existing node in the Database
           current_node%label = label
           current_node%number = i
           current_node%x0 = node_x0
           last_node => current_node
        ELSE
           ! Create new node
           CALL create_node(label,node_x0)
        ENDIF
        last_node%dof_status(3) = -1
        Read(UNIT=nout_unit,ERR=20,End=20) last_node%ngroup
        Read(UNIT=nout_unit,ERR=20,End=20) last_node%dof_status(1:nd)
        DO id=1,nd
           IF ( last_node%dof_status(id) == -1 ) nfixed = nfixed + 1
        ENDDO
        Read(UNIT=nout_unit,ERR=20,End=20) last_node%u0(1:nd)
        IF ( analys(1:7) == 'DYNAMIC' ) THEN
           Read(UNIT=nout_unit,ERR=20,End=20) last_node%v0(1:nd)
           Read(UNIT=nout_unit,ERR=20,End=20) last_node%a0(1:nd)
        ENDIF
        Read(UNIT=nout_unit,ERR=20,End=20) last_node%temp0
        Read(UNIT=nout_unit,ERR=20,End=20) last_node%fint0(1:nd)
        Read(UNIT=nout_unit,ERR=20,End=20) last_node%force_flag
        IF ( last_node%force_flag ) THEN
           IF ( .NOT.ASSOCIATED(last_node%force) ) &
                ALLOCATE(last_node%force(nd),last_node%force0(nd), &
                last_node%force_End(nd))
           Read(UNIT=nout_unit,ERR=20,End=20) last_node%force0(1:nd)
        ELSE
           IF ( ASSOCIATED(last_node%force) ) DEALLOCATE(last_node%force, &
                last_node%force0,last_node%force_End)
           last_node%force => NULL()
           last_node%force0 => NULL()
           last_node%force_End => NULL()
        ENDIF
        current_node => last_node%next
        x0(1:nd,i) = node_x0(1:nd)
        node_labels(i) = label
        nnodes = i
     ENDDO

     ! Deallocate all surplus nodes from the Database
     DO WHILE( ASSOCIATED(current_node) )
        previous_node => current_node
        current_node => current_node%next
        DEALLOCATE(previous_node)
     ENDDO
     IF ( ASSOCIATED(last_node) ) last_node%next => NULL()

     ! Read coupled sets
     Read(UNIT=nout_unit) number
     ncoupled = 0
     current_coupled => first_coupled_set
     DO i=1,number
        Read(UNIT=nout_unit) label
        IF ( ASSOCIATED(current_coupled) ) THEN
           current_coupled%label = label
           current_coupled%number = i
           last_coupled_set => current_coupled
        ELSE
           ALLOCATE(current_coupled)
           current_coupled%label = label
           current_coupled%number = ncoupled
           current_coupled%next => NULL()

           ! Add coupled set to the Database
           IF ( .NOT.ASSOCIATED(first_coupled_set) ) THEN
              first_coupled_set => current_coupled
              last_coupled_set => current_coupled
           ELSE
              last_coupled_set%next => current_coupled
              last_coupled_set => current_coupled
           ENDIF
        ENDIF
        Read(UNIT=nout_unit) current_coupled%forced
        Read(UNIT=nout_unit) current_coupled%u_forced
        Read(UNIT=nout_unit) current_coupled%u
        IF ( analys(1:7) == 'DYNAMIC' ) THEN
           Read(UNIT=nout_unit) current_coupled%v
           Read(UNIT=nout_unit) current_coupled%a
        ENDIF
        Read(UNIT=nout_unit) current_coupled%force
        current_coupled => current_coupled%next
        ncoupled = i
     ENDDO

     ! Deallocate all surplus coupled sets from the Database
     DO WHILE( ASSOCIATED(current_coupled) )
        previous_coupled => current_coupled
        current_coupled => current_coupled%next
        DEALLOCATE(previous_coupled)
     ENDDO
     IF ( ASSOCIATED(last_coupled_set) )last_coupled_set%next => NULL()

     ! Read material Data
     CALL mat_read_output(nout_unit)

     ! Read gas cavity Data
     Read(UNIT=nout_unit,ERR=20,End=20) number
     current_gascav => first_gascav
     DO i=1,number
        Read(UNIT=nout_unit,ERR=20,End=20) label
        Read(UNIT=nout_unit,ERR=20,End=20) failed
        Read(UNIT=nout_unit,ERR=20,End=20) p
        Read(UNIT=nout_unit,ERR=20,End=20) mgas
        Read(UNIT=nout_unit,ERR=20,End=20) V0_gc
        Read(UNIT=nout_unit,ERR=20,End=20) VpT0_gc
        Read(UNIT=nout_unit,ERR=20,End=20) V_tot_gc
        IF ( ASSOCIATED(current_gascav) ) THEN
           last_gascav => current_gascav
        ELSE
           ALLOCATE(current_gascav)
           current_gascav%next => NULL()
           IF ( .NOT.ASSOCIATED(first_gascav) ) THEN
              first_gascav => current_gascav
              last_gascav => current_gascav
           ELSE
              last_gascav%next => current_gascav
              last_gascav => current_gascav
           ENDIF
        ENDIF
        current_gascav%label = label
        current_gascav%failed0 = failed
        current_gascav%p0 = p
        current_gascav%mgas0 = mgas
        current_gascav%mgas_End = mgas
        current_gascav%V0 = V0_gc
        current_gascav%VpT0 = VpT0_gc
        current_gascav%V_tot = V_tot_gc
        current_gascav => current_gascav%next
     ENDDO

     ! Deallocate all surplus gas cavity entries from the Database
     DO WHILE( ASSOCIATED(current_gascav) )
        previous_gascav => current_gascav
        current_gascav => current_gascav%next
        DEALLOCATE(previous_gascav)
     ENDDO
     IF ( ASSOCIATED(last_gascav) ) last_gascav%next => NULL()

     ! Read geometry Data
     CALL read_geometry(nout_unit)

     ! Read element Data
     CALL spring_read_output(nout_unit)
     CALL quad4_read_output(nout_unit)
     CALL hex8_read_output(nout_unit)
     CALL solid1d_read_output(nout_unit)
     CALL pressure1d_read_output(nout_unit)
     CALL pressure2d_read_output(nout_unit)
     CALL pressure3d_read_output(nout_unit)
     CALL gascav1d_read_output(nout_unit)
     CALL gascav2d_read_output(nout_unit)
     CALL gascav3d_read_output(nout_unit)
     CALL cont1d_read_output(nout_unit)
     CALL cont2d_read_output(nout_unit)
     CALL cont3d_read_output(nout_unit)

     IF ( noutput >= numout ) THEN
        ! Initialize Database
        CALL update_Database()
        RETURN
     ENDIF
  ENDDO

10 CONTINUE

  RETURN

20 CONTINUE
  WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading FE output file'
  lerror = .TRUE.
  CALL nlfemp_stop(0)

        END SUBROUTINE read_output

    SUBROUTINE remesh_fuel_rod (na, nmesh, RadialBoundO, AxialNodLen, cpl, pitch)
    USE Kinds
    USE common_parameters
    USE geometry
    USE pressure1d
    USE cont1d
    USE gascav1d
    USE gascav2d
    USE fraptran_variables
    IMPLICIT NONE
    !>@brief
    !> Modify FRAPCON3-element mesh to be fit for FRAPTRAN analysis
    ! Node group  Referred nodes
    ! 1001 ...... Fuel nodes at axial slice 1
    ! 1002 ...... Cladding nodes at axial slice 1
    !
    !
    ! Material flags
    !  1001 ....... UO2 fuel
    !  1002 ....... Zircaloy cladding
    !  1003 ....... M5 cladding
    !
    ! Pressure boundary conditions
    ! 1000 ....... Coolant pressure at axial node 1
    ! 1001 ....... Rod inner pressure at axial node 1
    !
    ! File units
    ! 500 ........ FRAPCON3 Fe model output
    
    INTEGER(ipk), INTENT(IN) :: &
        na, & ! Number of axial meshes
        nmesh ! Index numbers used to extract Data from RadialBoundO
    REAL(r8k), INTENT(IN) :: &
        RadialBoundO(nmesh,na), & ! Radial node coordinates (ft)
        AxialNodLen(na), & ! Lengths of the axial slices (in)
        cpl, & ! Upper plenum length
        pitch ! Rod pitch in the assembly
    INTEGER(ipk) :: ia, number, nodes(6)
    REAL(r8k) :: node_x(3), rod_length
    TYPE(node_type), POINTER :: current_node
    INTEGER(ipk) :: i
    CHARACTER(LEN=80) :: frapcon_output

    ! Set DIMENSION
    nd = 2

    ! Open FRAPCON3 FE output file and read FE Data for restart time
    frapcon_output = 'frapcon-fem.edb'
    Open (UNIT=500,FILE=frapcon_output,STATUS='OLD',FORM='UNFORMATTED',ERR=10)
    i = HUGE(i)
    CALL read_output(i,500)
    CLOSE(UNIT=500)

    ! Create gas cavity for the gap and plenum
    CALL create_gascav(1,0.0_r8k)

    ! Create nodes for a contact element that prevents exessive deformations
    ngroup = 10
    IF ( pitch > RadialBoundO(nmesh,1) ) THEN
        node_x(1) = pitch * 6.0_r8k
        node_x(2) = 0.0_r8k
        CALL create_node(1,node_x)
        last_node%dof_status = -1
        DO ia = 1, (na - 1)
            node_x(2) = node_x(2) + AxialNodLen(ia)
        ENDDO
        node_x(2) = node_x(2) + cpl
        CALL create_node(2,node_x)
        last_node%dof_status = -1
        nodes(1:2) = (/ 2, 1 /)
        CALL create_line(1,nodes,1)
    ENDIF

  ! Modify upper plenum height and fuel stack length
    rod_length = 0.0_r8k
    DO ia = 1, (na - 1)
        number = ia * 1000 + 5
        rod_length = rod_length + AxialNodLen(ia)
        current_node => first_node
        DO WHILE ( ASSOCIATED(current_node) )
            IF ( current_node%label == number ) THEN
                current_node%x0(2) = rod_length
                current_node%u0(2) = 0.0_r8k
            ENDIF
            current_node => current_node%next
        ENDDO
    ENDDO
    rod_length = rod_length + cpl
    number = na * 1000 + 6
    current_node => first_node
    DO WHILE ( ASSOCIATED(current_node) )
        IF ( current_node%label == number ) THEN
            current_node%x0(2) = rod_length
            current_node%u0(2) = 0.0_r8k
            EXIT
        ENDIF
        current_node => current_node%next
    ENDDO

    ! Delete pressure boundary conditions at cladding inner surface
    DO ia = 1, (na + 1)
        number = ia * 1000 + 1
        CALL pressure1d_delete(number)
    ENDDO

    ! Create meshing for the rod starting at the bottom
    DO ia = 1, (na - 1)
        ! Define contact element that prevents excessive plastic deformation
        IF ( pitch > RadialBoundO(nmesh,1) ) THEN
            nodes(1:3) = (/ 2, 1, (ia*1000 + 4) /)
            number = ia * 1000 + 1
            CALL cont2d_create(number,1001,1,nodes)
        ENDIF

        ! Gas cavity in pellet cladding gap
        egroup = ia * 1000
        number = ia * 1000
        nodes(1:4) = (/ ia*1000 + 2, ia*1000 + 3, (ia-1)*1000 + 5, ia*1000 + 5 /)
        CALL gascav1d_create(number,1,nodes,.FALSE.)
    ENDDO

    ! Gas cavity in upper plenum
    egroup = na * 1000
    number = na * 1000
    nodes(1:4) = (/ na*1000 + 7, na*1000 + 3, (na-1)*1000 + 5, na*1000 + 6 /)
    CALL gascav1d_create(number,1,nodes,.TRUE.)

    init_fe = .TRUE.

    RETURN

10  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,A/)') 'ERROR can not find file ',frapcon_output
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE remesh_fuel_rod
    
SUBROUTINE temp_storage(ic)
  USE Kinds
  USE common_parameters
  USE solid1d
  USE cont1d
  USE pressure1d
  IMPLICIT NONE
  CHARACTER(LEN=1), INTENT(IN) :: ic
  TYPE(node_type),POINTER :: cnode
  TYPE(solid1d_type),POINTER :: csolid
  TYPE(cont1d_type),POINTER :: ccont
  TYPE(pressure1d_type),POINTER :: cpress
  INTEGER(ipk) :: i

  IF ( ic == 'U' ) THEN
     i = 0
     cnode => first_node
     DO WHILE ( ASSOCIATED(cnode) )
        i = i + 1
        ut(1:2,i) = cnode%u0(1:2)
        tempt(i) = cnode%temp0
        cnode => cnode%next
     ENDDO

     i = 0
     csolid => first_solid1d
     DO WHILE ( ASSOCIATED(csolid) )
        i = i + 1
        epsplt(1:3,i) = csolid%epspl0(1:3)
        epsefft(i) = csolid%epseff0
        csolid => csolid%next
     ENDDO

     i = 0
     ccont => first_cont1d
     DO WHILE ( ASSOCIATED(ccont) )
        i = i + 1
        closedt(i) = ccont%closed0
        stickt(i) = ccont%stick0
        dz0t(i) = ccont%dz0
        ccont => ccont%next
     ENDDO

     i = 0
     cpress => first_pressure1d
     DO WHILE ( ASSOCIATED(cpress) )
        i = i + 1
        prt(i) = cpress%p0
        cpress => cpress%next
     ENDDO

  Else

     i = 0
     cnode => first_node
     DO WHILE ( ASSOCIATED(cnode) )
        i = i + 1
        cnode%u0(1:2) = ut(1:2,i)
        cnode%temp0 = tempt(i)
        cnode => cnode%next
     ENDDO

     i = 0
     csolid => first_solid1d
     DO WHILE ( ASSOCIATED(csolid) )
        i = i + 1
        csolid%epspl0(1:3) = epsplt(1:3,i)
        csolid%epseff0 = epsefft(i)
        csolid => csolid%next
     ENDDO

     i = 0
     ccont => first_cont1d
     DO WHILE ( ASSOCIATED(ccont) )
        i = i + 1
        ccont%closed0 = closedt(i)
        ccont%stick0 = stickt(i)
        ccont%dz0 = dz0t(i)
        ccont => ccont%next
     ENDDO

     i = 0
     cpress => first_pressure1d
     DO WHILE ( ASSOCIATED(cpress) )
        i = i + 1
        cpress%p0 = prt(i)
        cpress => cpress%next
     ENDDO

  ENDIF

END SUBROUTINE temp_storage
    
    END MODULE FEModel
    