program frapi_input_file

    !use h5file
    use conversions_frapcon
    use fpn_reader
    use frapi, only : frod_type
    use odfile, only : t_odfile

    implicit none

    !type (tp_h5file) :: ofile
    type (t_odfile) :: ofile

    type (frod_type) :: frod

    logical :: is_restart
    logical :: is_save

    integer, parameter :: ivars_array = 37, ivars_value = 4
    character(len=256) :: filename, string
    character(len=256) :: varname_array(ivars_array), varname_value(ivars_value)

    ! ITERATIONAL VARIABLES
    integer :: i, itime, starttime

    real(8) :: qtot

    real(8), allocatable :: value(:)
    real(8), allocatable :: linpow(:)
    real(8), allocatable :: t_cool(:)
    real(8), allocatable :: p_cool(:)
    real(8), allocatable :: f_cool(:)

    varname_array( 1) = 'bulk coolant temperature, C'
    varname_array( 2) = 'total gap conductance, W|(m^2*K)'
    varname_array( 3) = 'oxide thickness, um'
    varname_array( 4) = 'thermal gap thickness, um'
    varname_array( 5) = 'mechanical gap thickness, um'
    varname_array( 6) = 'gap pressure, MPa'
    varname_array( 7) = 'cladding hoop strain, %'
    varname_array( 8) = 'cladding axial strain, %'
    varname_array( 9) = 'cladding radial strain, %'
    varname_array(10) = 'cladding permanent hoop strain, %'
    varname_array(11) = 'cladding permanent axial strain, %'
    varname_array(12) = 'cladding permanent radial strain, %'
    varname_array(13) = 'cladding termal hoop strain, %'
    varname_array(14) = 'cladding termal axial strain, %'
    varname_array(15) = 'cladding termal radial strain, %'
    varname_array(16) = 'cladding hoop stress, MPa'
    varname_array(17) = 'cladding axial stress, MPa'
    varname_array(18) = 'cladding radial stress, MPa'
    varname_array(19) = 'cladding inner radius displacement, mm'
    varname_array(20) = 'cladding outer radius displacement, mm'
    varname_array(21) = 'cladding creep rate'
    varname_array(22) = 'fuel surface outward displacement, mm'
    varname_array(23) = 'fuel thermal expansion, mm'
    varname_array(24) = 'fuel swelling, um'
    varname_array(25) = 'fuel creep, mm'
    varname_array(26) = 'fuel densification, mm'
    varname_array(27) = 'fuel relocation, mm'
    varname_array(28) = 'cladding hydrogen concentration, ppm'
    varname_array(29) = 'coolant density, kg|m^3'
    varname_array(30) = 'coolant pressure, MPa'
    varname_array(31) = 'axial mesh, cm'
    varname_array(32) = 'centerline temperature, C'
    varname_array(33) = 'fuel stored energy, J|kg'
    varname_array(34) = 'fuel burnup, MW*d|kg'
    varname_array(35) = 'fuel volume average temperature, C'
    varname_array(36) = 'gap average temperature, C'
    varname_array(37) = 'cladding average temperature, C'

    varname_value( 1) = 'fission gas release, %'
    varname_value( 2) = 'time, day'
    varname_value( 3) = 'average linear power, W|cm'
    varname_value( 4) = 'average fuel burnup, MW*d|kg'

    ! READING INPUT FILE
    call get_command_argument(1, filename)

    call read_frapcon_file(filename)

    allocate(value(na))
    allocate(linpow(na+1))
    allocate(t_cool(na+1))
    allocate(p_cool(na+1))
    allocate(f_cool(na+1))

    ! CREATE OUTPUT FILE
    i = scan(filename, '.', back=.true.)
    call ofile % open(filename(1:i-1)//'-frapi.txt')

    !-------------------- FUEL ROD INITIALIZATION-----------------------------
    call frod % make(nr=nr, na=na, ngasr=ngasr, nce=nce, &
        mechan = mechan, ngasmod = ngasmod, &
        icm = icm, icor = icor, iplant = iplant, &
        imox = imox, igascal = igascal, zr2vintage = zr2vintage, &
        moxtype = moxtype, idxgas = idxgas, &
        iq = iq, ivardm=ivardm, &
        ifixedcoolt=ifixedcoolt, ifixedcoolp=ifixedcoolp, ifixedtsurf=ifixedtsurf, &
        verbose=.false., flag_iapws=.false.)

    if (.not. (jn(1) == na+1) ) then
        x(2: ) = (/( i * (x(jn(1)) - x(1)) / na, i = 1, na )/)
        qf(2:) = qf(1)
    endif

    call frod % set_value("cladding thickness, cm", thkcld(1) * intocm)
    call frod % set_value("gap thickness, cm", thkgap(1) * intocm)
    call frod % set_value("outer cladding diameter, cm", dco(1) * intocm)
    call frod % set_value("fuel rod pitch, cm", pitch * intocm)
    call frod % set_value("as-fabricated apparent fuel density, %TD", den)
    call frod % set_value("fuel enrichment by u-235, %", enrch(1))
    call frod % set_array("thickness of the axial nodes, cm", (x(2:na+1) - x(1:na)) * ftocm)
    call frod % set_value("additional fuel densification factor", afdn)
    call frod % set_value("clad texture factor", catexf)
    call frod % set_value("as-fabricated clad hydrogen content, wt.ppm", chorg)
    call frod % set_value("clad cold work", cldwks)
    call frod % set_value("cold plenum length, m", cpl * intom)
    call frod % set_value("constant crud thickness, mm", crdt * intomm)
    call frod % set_value("crud accumulation rate", crdtr)
    call frod % set_value("creep step duration, hr", crephr)
    call frod % set_value("fuel open porosity fraction, %TD", deng)
    call frod % set_value("spring diameter, mm", dspg*intomm)
    call frod % set_value("spring wire diameter, mm", dspgw*intomm)
    call frod % set_value("number of spring turns", vs)
    call frod % set_value("peak-to-average power ratio", fa)
    call frod % set_value("fill gas pressure, Pa", fgpav/PatoPSI)
    call frod % set_value("fuel oxygen-to-metal ratio", fotmtl)
    call frod % set_value("weight ppm H2O in fuel, wt.ppm", ppmh2o)
    call frod % set_value("weight ppm N2 in fuel, wt. ppm", ppmn2)
    call frod % set_value("expected resintering density increase, kg|m^3", rsntr)
    call frod % set_value("fision gas atoms per 100 fissions", sgapf)
    call frod % set_value("swelling limit", slim)
    call frod % set_value("pellet centering temperature, K", tsint)
    call frod % set_value("grain size of the fuel, um", grnsize)
    call frod % set_value("FEA friction coefficient", frcoef)
    call frod % set_value("boron-10 enrichment in ZrB2, atom %", b10)
    call frod % set_value("ZrB2 thickness, mm", zrb2thick*intomm)
    call frod % set_value("ZrB2 density, %TD", zrb2den)
    call frod % set_value("decay heat multiplier", fpdcay)
    call frod % set_value("molar fraction of air", amfair)
    call frod % set_value("molar fraction of argon", amfarg)
    call frod % set_value("molar fraction of fission gas", amffg)
    call frod % set_value("molar fraction of helium", amfhe)
    call frod % set_value("molar fraction of hydrogen", amfh2)
    call frod % set_value("molar fraction of water", amfh2o)
    call frod % set_value("molar fraction of krypton", amfkry)
    call frod % set_value("molar fraction of nitrogen", amfn2)
    call frod % set_value("molar fraction of xenon", amfxe)
    call frod % set_value("Bias on fuel thermal conductivity", sigftc)
    call frod % set_value("Bias on fuel thermal expansion", sigftex)
    call frod % set_value("Bias on fission gas release", sigfgr)
    call frod % set_value("Bias on fuel swelling", sigswell)
    call frod % set_value("Bias on cladding creep", sigcreep)
    call frod % set_value("Bias on cladding axial growth", siggro)
    call frod % set_value("Bias on cladding corrosion", sigcor)
    call frod % set_value("Bias on cladding hydrogen pickup", sigh2)
    call frod % set_value("fuel pellet Pu-239 content", enrpu39)
    call frod % set_value("fuel pellet Pu-240 content", enrpu40)
    call frod % set_value("fuel pellet Pu-241 content", enrpu41)
    call frod % set_value("fuel pellet Pu-242 content", enrpu42)
    call frod % set_value("pellet height, mm", hplt * intomm)
    call frod % set_value("chamfer height, mm", chmfrh * intomm)              
    call frod % set_value("chamfer width, mm", chmfrw * intomm)              
    call frod % set_value("dish shoulder width, mm", dishsd * intomm)
    call frod % set_value("clad roughness, mm", roughc * intomm)
    call frod % set_value("fuel roughness, mm", roughf * intomm)
    call frod % set_value("percent IFBA rods in core, %", ifba)
    call frod % set_value("end-node to plenum heat transfer fraction", qend(1))    
    call frod % set_value("rod internal pressure for FEA model, MPa", p1(1)/patoPSI)
    call frod % set_value("radius of the fuel pellet central annulus, mm", rc(1) * intomm)
    call frod % set_value("coolant system pressure, MPa", p2(1) * PSItoMPa)

    gadoln(:) = gadoln(1)
    flux(:)   = flux(1)
    comp(:)   = comp(1)
    buin(:)   = buin(1)
    crudmult(:) = crudmult(1)

    call frod % set_array("input fuel burnup", buin / MWskgUtoMWdMTU)
    call frod % set_array("PuO2 weight percent if MOX fuel, wt%", comp)
    call frod % set_array("gadolinia weight, wt%", gadoln)
    call frod % set_array("cladding surface temperature, K", (/( tfk(cladt(i)), i = 1, na )/) )
    call frod % set_array("axial crud thickness multiplier", crudmult)
    call frod % set_array("neutron flux, 1|(cm^2*s)", flux)

!------------------------ NOT USED: --------------------------------------------------------------
!    irefab                                  "Timestep to start using refabricated values (Default = 10,000)"
!    nrefab1                                 "Lower axial node for refabrication"
!    nrefab2                                 "Upper axial node for refabrication"
!    cplrefab                                "Refabricated upper plenum length"
!    vsrefab                                 "Number of spring turns in refabricated upper plenum"
!    dspgrefab                               "New plenum spring coil diameter"
!    dspgwrefab                              "New plenum spring wire diameter"
!    fgpavrefab                              "Fill gas pressure at time step of refabrication"
!    airrefab                                "Fraction of air in refabricated rod"
!    n2refab                                 "Fraction of nitrogen in refabricated rod"
!    arrefab                                 "Fraction of argon in refabricated rod"
!    fgrefab                                 "Fraction of fission gas in refabricated rod"
!    herefab                                 "Fraction of helium in refabricated rod (Default = 1.0)"
!    krrefab                                 "Fraction of krypton in refabricated rod"
!    xerefab                                 "Fraction of xenon in refabricated rod"
!    jstsurftemp                             "Sequential # of the cladding temperature profile to be used for each timestep"
!    jnsurftemp                              "# of cladt, xt pairs for each axial temperature distribution"
!    jn                                      "# of qf, x pairs for each axial power shape"
!    ctmax                                   ""
!    igas                                    "Timestep to begin calculation of fission gas release"
    !------------------- RUN TIME STEPS ---------------------------------------

    starttime = 1
    is_save = .false.

    if (starttime > 1) is_restart = .true.

    ! ITERATION OVER TIME
    do itime = starttime, im

        write(*,*) 'time step : ', itime

        qtot = 1.D+3 * qmpy(itime) / ftocm ! Wt/cm
        linpow = qf((na+1) * (jst(itime)-1) + 1 : (na+1) * (jst(itime)-1) + na + 1)
        linpow = qtot * linpow / sum(linpow)

        t_cool = (/( tfc(tcoolant(i+(na+1)*(itime-1))), i = 1, na+1 )/)
        p_cool = pcoolant(1+(na+1)*(itime-1):na+1+(na+1)*(itime-1)) * PSItoPa

        ! INITIAL STATE
        if (itime == starttime) then

            call frod % set_array("FRAPCON FORMAT: linear power, W|cm", linpow)
            call frod % set_array("FRAPCON FORMAT: coolant temperature, C", t_cool)
            call frod % set_array("FRAPCON FORMAT: coolant pressure, MPa", p_cool)
            call frod % set_value("inlet coolant temperature, C", tfc(tw(itime)))
            call frod % set_value("inlet coolant pressure, MPa", p2(itime) * PSItoMPa)
            call frod % set_value("coolant mass flux, kg|(s*m^2)", go(itime)*lbhrft2toksm2)

            call frod % init()
            call frod % accept()

            call frod % set_array("FRAPCON FORMAT: linear power, W|cm", linpow)
            call frod % set_array("FRAPCON FORMAT: coolant temperature, C", t_cool)
            call frod % set_array("FRAPCON FORMAT: coolant pressure, MPa", p_cool)
            call frod % set_value("inlet coolant temperature, C", tfc(tw(itime)))
            call frod % set_value("inlet coolant pressure, MPa", p2(itime) * PSItoMPa)
            call frod % set_value("coolant mass flux, kg|(s*m^2)", go(itime)*lbhrft2toksm2)

        else

            call frod % set_array("FRAPCON FORMAT: linear power, W|cm", linpow)
            call frod % set_array("FRAPCON FORMAT: coolant temperature, C", t_cool)
            call frod % set_array("FRAPCON FORMAT: coolant pressure, MPa", p_cool)
            call frod % set_value("inlet coolant temperature, C", tfc(tw(itime)))
            call frod % set_value("inlet coolant pressure, MPa", p2(itime) * PSItoMPa)
            call frod % set_value("coolant mass flux, kg|(s*m^2)", go(itime)*lbhrft2toksm2)

        endif

        if ((is_restart).and.(itime == starttime)) then

            write(string, '(A,I0.6,A,I0.6,A)') 'burnup_', itime-1, '.bin'

            call frod % load(string)

        endif


        if (itime == 1) then
            call frod % next(ProblemTime(itime-1))
        else
            call frod % next(ProblemTime(itime-1) - ProblemTime(itime-2))
        endif

        call frod % accept()

        call ofile % write_i4_0('frapi burnup step', itime)

        do i = 1, ivars_array
            call frod % get_array(varname_array(i), value)
            call ofile % write_r8_1(varname_array(i), value)
        enddo

        do i = 1, ivars_value
            call frod % get_value(varname_value(i), value(1))
            call ofile % write_r8_0(varname_value(i), value(1))
        enddo

        if (.not. is_restart) then

            write(string, '(A,I0.6,A,I0.6,A)') 'burnup_', itime, '.bin'
            if (is_save) call frod % save(string)

        endif

    enddo

    !----------------------------- DEALLOCATE THE FUEL RODS --------------------------
    call frod % destroy()

    call ofile % close()

    write(*,*) 'Successfully done!'

end program frapi_input_file