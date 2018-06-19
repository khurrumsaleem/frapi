program frapcon_input_file

    use conversions
    use fpn_reader
    use fuelrod, only : frod_type

    implicit none

    type (frod_type) :: frod

    character(len=256) :: filename

    ! ITERATIONAL VARIABLES
    integer :: i, itime

    real(8), allocatable :: thickness(:)
    real(8), allocatable :: linpow(:)
    real(8), allocatable :: t_cool(:)
    real(8), allocatable :: p_cool(:)
    real(8), allocatable :: f_cool(:)

    mechan      = 2
    ngasmod     = 2
    icm         = 4
    icor        = 0
    iplant      =-2
    imox        = 0
    igascal     = 1
    zr2vintage  = 1
    moxtype     = 0
    idxgas      = 1
    ifixedcoolt = 0
    ifixedcoolp = 0
    ifixedtsurf = 0

    ! READING INPUT FILE
    call get_command_argument(1, filename)

    call read_frapcon_file(filename)

    allocate(thickness(na))
    allocate(linpow(na))
    allocate(t_cool(na))
    allocate(p_cool(na))
    allocate(f_cool(na))

    !-------------------- FUEL ROD INITIALIZATION-----------------------------
    thickness = (x(2:na+1) - x(1:na)) * ftocm
    pitch     = pitch * intocm
    thkcld    = thkcld * intocm
    thkgap    = thkgap * intocm
    dco       = dco * intocm

    call frod % make(nr, na, ngasr, nce, &
        thkcld = thkcld(1), thkgap = thkgap(1), &
        dco = dco(1), pitch = pitch, &
        den = den, enrch = enrch(1), dx = thickness, &
        mechan = mechan, ngasmod = ngasmod, &
        icm = icm, icor = icor, iplant = iplant, &
        imox = imox, igascal = igascal, zr2vintage = zr2vintage, &
        moxtype = moxtype, idxgas = idxgas, iq = iq, &
        ifixedcoolt=ifixedcoolt, ifixedcoolp=ifixedcoolp, ifixedtsurf=ifixedtsurf, &
        verbose=.true.)

    !------------------- SETUP INPUT VARIABLES -------------------------------

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
    call frod % set_value("expected resintering density increase, kg/m^3", rsntr)
    call frod % set_value("fision gas atoms per 100 fissions", sgapf)
    call frod % set_value("swelling limit", slim)
    call frod % set_value("pellet centering temperature, K", tkf(tsint))
    call frod % set_value("grain size of the fuel, um", grnsize)
    call frod % set_value("FEA friction coefficient", frcoef)
    call frod % set_value("boron-10 enrichment in ZrB2, atom %", b10)
    call frod % set_value("ZrB2 thickness, mm", zrb2thick*intomm)
    call frod % set_value("ZrB2 density, mm", zrb2den * intomm)
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
    call frod % set_array("input fuel burnup", buin / MWskgUtoMWdMTU)
    call frod % set_array("PuO2 weight percent if MOX fuel, wt%", comp)
    call frod % set_array("gadolinia content at each axial node", gadoln)
    call frod % set_array("radius of the fuel pellet central annulus, mm", rc * intomm)
    call frod % set_array("cladding surface temperature, K", (/( tfk(cladt(i)), i = 1, na )/) )
    call frod % set_array("axial crud thickness multiplier", crudmult)

!------------------------ NOT USED: --------------------------------------------------------------
!    ivardm                                  "Option to specify variable axial node length (1 = ON, 0 = OFF(Default))"
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
!    zcool                                   "Elevation defining the coolant temperature profile. Used when ifixedcoolt = 1."
!    jstsurftemp                             "Sequential # of the cladding temperature profile to be used for each timestep"
!    jnsurftemp                              "# of cladt, xt pairs for each axial temperature distribution"
!    jn                                      "# of qf, x pairs for each axial power shape"
!    ctmax                                   ""
!    p2                                      "Coolant System Pressure, input for each timestep if nsp = 1"
!    igas                                    "Timestep to begin calculation of fission gas release"
    !------------------- RUN TIME STEPS ---------------------------------------

    ! ITERATION OVER TIME
    do itime = 1, im

        linpow = qmpy(itime) * qf(1-na+na*jst(itime):na*jst(itime)) * cmtoft
        t_cool = (/( tfc(tcoolant(i+na*(itime-1))), i = 1, na )/)
        p_cool = pcoolant(1+na*(itime-1):na+na*(itime-1)) * PSItoPa
        f_cool = go(itime) * lbhrft2toksm2

        ! SETUP THE UPDATED VARIABLES
        call frod % set_array("linear power, W/cm", linpow)
        call frod % set_array("coolant temperature, C", t_cool)
        call frod % set_array("coolant pressure, MPa", p_cool)
        call frod % set_value("inlet coolant temperature, C", tw(itime))
        call frod % set_value("inlet coolant pressure, MPa", p2(itime))
        call frod % set_value("coolant mass flux, kg/(s*m^2)", go(itime))

        if (itime == 1) then
            ! INITIAL STATE
            call frod % init()
        else
            ! DO TRIAL TIME STEP
            call frod % next(ProblemTime(itime) - ProblemTime(itime-1))
        endif

        ! TAKE OUTPUT VARIABLES FROM FRAPCON
!        call frod(i_frod) % get('axial fuel temperature, C', fue_avg_temp)
!        call frod(i_frod) % get('bulk coolant temperature, C', coo_avg_temp)
!        call frod(i_frod) % get('total gap conductance, W/(m^2*K)', fue_dyn_hgap)
!        call frod(i_frod) % get('oxide thickness, um', t_oxidelayer)
!        call frod(i_frod) % get('mechanical gap thickness, um', t_fuecladgap)
!        call frod(i_frod) % get('gap pressure, MPa', gap_pressure)
!        call frod(i_frod) % get('cladding hoop strain, %', hoop_strain)
!        call frod(i_frod) % get('cladding axial stress, MPa', hoop_stress)
!        call frod(i_frod) % get('axial mesh, cm', zmesh_FRPCN)

        ! ACCEPT THE LAST TRIAL TIME STEP
        call frod % accept()

    enddo

    !----------------------------- DEALLOCATE THE FUEL RODS --------------------------
    call frod % destroy()

    write(*,*) 'Successfully done!'

end program frapcon_input_file