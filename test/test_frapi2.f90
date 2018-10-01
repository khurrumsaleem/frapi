program frapi_input_file

    use conversions_frapcon
    use fpn_reader
    use frapi, only : frod_type
    use odfile, only : t_odfile

    implicit none

    type (t_odfile) :: ofile

    type (frod_type) :: frod

    logical :: is_restart
    logical :: is_save

    integer, parameter :: ivars_array = 37, ivars_value = 4, nvars = 41
    character(len=256) :: filename, string, frapmode
    character(len=256) :: varname_array(ivars_array), varname_value(ivars_value)

    integer :: i, itime, starttime

    real(8) :: qtot

    real(8), allocatable :: value(:)
    real(8), allocatable :: linpow(:)
    real(8), allocatable :: t_cool(:)
    real(8), allocatable :: p_cool(:)
    real(8), allocatable :: f_cool(:)

    character(len=256) :: varname(nvars)
    character(len=256) :: vartype(nvars)

    include "varlist.f90"

    call get_command_argument(1, frapmode)
    call get_command_argument(2, filename)

    select case (frapmode)
        case ('frapcon')
            call read_frapcon_file(filename)
        case ('fraptran')
            call read_fraptran_file(filename)
    end select

    call frod % make(nr=nr, na=na, ngasr=ngasr, nce=nce, &
        frapmode=frapmode, mechan = mechan, ngasmod = ngasmod, &
        icm = icm, icor = icor, iplant = iplant, &
        imox = imox, igascal = igascal, zr2vintage = zr2vintage, &
        moxtype = moxtype, idxgas = idxgas, &
        iq = iq, ivardm=ivardm, &
        ifixedcoolt=ifixedcoolt, ifixedcoolp=ifixedcoolp, ifixedtsurf=ifixedtsurf, &
        verbose=.false., flag_iapws=.false.)

    write(*,*) 'Successfuly finished!'

    stop

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

    call frod % set_r8_0("cladding thickness, cm", thkcld(1) * intocm)
    call frod % set_r8_0("gap thickness, cm", thkgap(1) * intocm)
    call frod % set_r8_0("outer cladding diameter, cm", dco(1) * intocm)
    call frod % set_r8_0("fuel rod pitch, cm", pitch * intocm)
    call frod % set_r8_0("as-fabricated apparent fuel density, %TD", den)
    call frod % set_r8_0("fuel enrichment by u-235, %", enrch(1))
    call frod % set_r8_0("additional fuel densification factor", afdn)
    call frod % set_r8_0("clad texture factor", catexf)
    call frod % set_r8_0("as-fabricated clad hydrogen content, wt.ppm", chorg)
    call frod % set_r8_0("clad cold work", cldwks)
    call frod % set_r8_0("cold plenum length, m", cpl * intom)
    call frod % set_r8_0("constant crud thickness, mm", crdt * intomm)
    call frod % set_r8_0("crud accumulation rate", crdtr)
    call frod % set_r8_0("creep step duration, hr", crephr)
    call frod % set_r8_0("fuel open porosity fraction, %TD", deng)
    call frod % set_r8_0("spring diameter, mm", dspg*intomm)
    call frod % set_r8_0("spring wire diameter, mm", dspgw*intomm)
    call frod % set_r8_0("number of spring turns", vs)
    call frod % set_r8_0("peak-to-average power ratio", fa)
    call frod % set_r8_0("fill gas pressure, Pa", fgpav/PatoPSI)
    call frod % set_r8_0("fuel oxygen-to-metal ratio", fotmtl)
    call frod % set_r8_0("weight ppm H2O in fuel, wt.ppm", ppmh2o)
    call frod % set_r8_0("weight ppm N2 in fuel, wt. ppm", ppmn2)
    call frod % set_r8_0("expected resintering density increase, kg|m^3", rsntr)
    call frod % set_r8_0("fision gas atoms per 100 fissions", sgapf)
    call frod % set_r8_0("swelling limit", slim)
    call frod % set_r8_0("pellet centering temperature, K", tsint)
    call frod % set_r8_0("grain size of the fuel, um", grnsize)
    call frod % set_r8_0("FEA friction coefficient", frcoef)
    call frod % set_r8_0("boron-10 enrichment in ZrB2, atom %", b10)
    call frod % set_r8_0("ZrB2 thickness, mm", zrb2thick*intomm)
    call frod % set_r8_0("ZrB2 density, %TD", zrb2den)
    call frod % set_r8_0("decay heat multiplier", fpdcay)
    call frod % set_r8_0("molar fraction of air", amfair)
    call frod % set_r8_0("molar fraction of argon", amfarg)
    call frod % set_r8_0("molar fraction of fission gas", amffg)
    call frod % set_r8_0("molar fraction of helium", amfhe)
    call frod % set_r8_0("molar fraction of hydrogen", amfh2)
    call frod % set_r8_0("molar fraction of water", amfh2o)
    call frod % set_r8_0("molar fraction of krypton", amfkry)
    call frod % set_r8_0("molar fraction of nitrogen", amfn2)
    call frod % set_r8_0("molar fraction of xenon", amfxe)
    call frod % set_r8_0("Bias on fuel thermal conductivity", sigftc)
    call frod % set_r8_0("Bias on fuel thermal expansion", sigftex)
    call frod % set_r8_0("Bias on fission gas release", sigfgr)
    call frod % set_r8_0("Bias on fuel swelling", sigswell)
    call frod % set_r8_0("Bias on cladding creep", sigcreep)
    call frod % set_r8_0("Bias on cladding axial growth", siggro)
    call frod % set_r8_0("Bias on cladding corrosion", sigcor)
    call frod % set_r8_0("Bias on cladding hydrogen pickup", sigh2)
    call frod % set_r8_0("fuel pellet Pu-239 content", enrpu39)
    call frod % set_r8_0("fuel pellet Pu-240 content", enrpu40)
    call frod % set_r8_0("fuel pellet Pu-241 content", enrpu41)
    call frod % set_r8_0("fuel pellet Pu-242 content", enrpu42)
    call frod % set_r8_0("pellet height, mm", hplt * intomm)
    call frod % set_r8_0("chamfer height, mm", chmfrh * intomm)              
    call frod % set_r8_0("chamfer width, mm", chmfrw * intomm)              
    call frod % set_r8_0("dish shoulder width, mm", dishsd * intomm)
    call frod % set_r8_0("clad roughness, mm", roughc * intomm)
    call frod % set_r8_0("fuel roughness, mm", roughf * intomm)
    call frod % set_r8_0("percent IFBA rods in core, %", ifba)
    call frod % set_r8_0("end-node to plenum heat transfer fraction", qend(1))    
    call frod % set_r8_0("rod internal pressure for FEA model, MPa", p1(1)/patoPSI)
    call frod % set_r8_0("radius of the fuel pellet central annulus, mm", rc(1) * intomm)
    call frod % set_r8_0("coolant system pressure, MPa", p2(1) * PSItoMPa)

    gadoln(:) = gadoln(1)
    flux(:)   = flux(1)
    comp(:)   = comp(1)
    buin(:)   = buin(1)
    crudmult(:) = crudmult(1)

    call frod % set_r8_1("thickness of the axial nodes, cm", (x(2:na+1) - x(1:na)) * ftocm)
    call frod % set_r8_1("input fuel burnup", buin / MWskgUtoMWdMTU)
    call frod % set_r8_1("PuO2 weight percent if MOX fuel, wt%", comp)
    call frod % set_r8_1("gadolinia weight, wt%", gadoln)
    call frod % set_r8_1("cladding surface temperature, K", (/( tfk(cladt(i)), i = 1, na )/) )
    call frod % set_r8_1("axial crud thickness multiplier", crudmult)
    call frod % set_r8_1("neutron flux, 1|(cm^2*s)", flux)

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

            call frod % set_r8_1("FRAPCON FORMAT: linear power, W|cm", linpow)
            call frod % set_r8_1("FRAPCON FORMAT: coolant temperature, C", t_cool)
            call frod % set_r8_1("FRAPCON FORMAT: coolant pressure, MPa", p_cool)
            call frod % set_r8_0("inlet coolant temperature, C", tfc(tw(itime)))
            call frod % set_r8_0("inlet coolant pressure, MPa", p2(itime) * PSItoMPa)
            call frod % set_r8_0("coolant mass flux, kg|(s*m^2)", go(itime)*lbhrft2toksm2)

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
            call frod % get_r8_1(varname_array(i), value)
            call ofile % write_r8_1(varname_array(i), value)
        enddo

        do i = 1, ivars_value
            call frod % get_r8_0(varname_value(i), value(1))
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