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
    character(len=256) :: filename, string, frapmode, restart_file_name
    character(len=256) :: varname_array(ivars_array), varname_value(ivars_value)

    integer :: i, j, itime, starttime

    real(8) :: qtot

    real(8), allocatable :: value(:)
    real(8), allocatable :: linpow(:)
    real(8), allocatable :: t_cool(:)
    real(8), allocatable :: p_cool(:)
    real(8), allocatable :: f_cool(:)

    character(len=256) :: varname(nvars)
    character(len=256) :: vartype(nvars)

    include "fi_varlist_h.f90"

    call get_command_argument(1, frapmode)
    call get_command_argument(2, filename)
    call get_command_argument(3, restart_file_name)

    select case (frapmode)

        case ('frapcon')

            call read_frapcon_file(filename)

            call frod % make(nr=nr, na=na, ngasr=ngasr, nce=nce, &
                 frapmode=frapmode, mechan = mechan, ngasmod = ngasmod, &
                 icm = icm, icor = icor, iplant = iplant, &
                 imox = imox, igascal = igascal, zr2vintage = zr2vintage, &
                 moxtype = moxtype, idxgas = idxgas, &
                 iq = iq, ivardm=ivardm, &
                 ifixedcoolt=ifixedcoolt, ifixedcoolp=ifixedcoolp, ifixedtsurf=ifixedtsurf, &
                 verbose=.false., flag_iapws=.false.)

        case ('fraptran')

            call read_fraptran_file(filename)

            call frod % make(nr=nfmesh, na=naxn, nce=ncmesh, relocmodel=relocmodel, &
                 coolant=coolant,bheat=bheat,mheat=mheat,verbose=.false., frapmode=frapmode, &
                 reflood=reflood,internal=internal,metal=metal,deformation=deformation,&
                 inst=inst,geomet=geomet,nvol1=nvol1,lowpl=lowpl,pressu=pressu,massfl=massfl,&
                 coreav=coreav,chf=chf,filmbo=filmbo,coldwa=coldwa,axpow=axpow,&
                 bowing=bowing,spefbz=spefbz,geometry=geometry,nbundl=nbundl,refloodtime=time,&
                 radiat=radiat,ruptur=ruptur,liquid=liquid,inlet=inlet,reflo=reflo,&
                 pressure=pressure,collaps=collaps,frapt4=frapt4,geom=geom,temp=temp,&
                 tape2=tape2,nvol2=nvol2,zone=zone,upppl=upppl,&
                 jfb=jfb,nucbo=nucbo,unitin=unitin,unitout=unitout,res=res,&
                 pow=pow,gasflo=gasflo,idoxid=idoxid,cathca=cathca,baker=baker,&
                 noball=noball,cenvoi=cenvoi,soltyp=soltyp)

            call frod % set_ch_0('restart file', restart_file_name)
            call frod % set_r8_0('splbp', splbp)
            call frod % set_r8_0('tpowf', tpowf)
            call frod % set_r8_0('ruptstrain', ruptstrain)
            call frod % set_r8_0('frcoef', frcoef)
            call frod % set_r8_0('epsht1', epsht1)
            call frod % set_r8_0('CladPower', CladPower)
            call frod % set_i4_0('azang', azang)
            call frod % set_r8_0('pitch', pitch)
            call frod % set_r8_0('bowthr', bowthr)
            call frod % set_i4_0('iStoicGrad', iStoicGrad)
            call frod % set_i4_0('prestmp', prestmp)
            call frod % set_r8_0('dofang', dofang)
            call frod % set_r8_0('coldbp', coldbp)
            call frod % set_i4_0('nbhtc', nbhtc)
            call frod % set_i4_0('rtheta', rtheta)
            call frod % set_r8_0('frden', frden)
            call frod % set_r8_0('RodDiameter', RodDiameter)
            call frod % set_i4_0('prescri', prescri)
            call frod % set_i4_0('mechan', mechan)
            call frod % set_i4_0('nfmesh', nfmesh)
            call frod % set_r8_0('refdtm', refdtm)
            call frod % set_r8_0('totnb', totnb)
            call frod % set_r8_0('powop', powop)
            call frod % set_r8_0('flxsec', flxsec)
            call frod % set_i4_0('NumAxProfiles', NumAxProfiles)
            call frod % set_i4_0('radiat', radiat)
            call frod % set_r8_0('ffch', ffch)
            call frod % set_r8_0('fpdcay', fpdcay)
            call frod % set_r8_0('roughc', roughc)
            call frod % set_r8_0('roughf', roughf)
            call frod % set_r8_0('prsacc', prsacc)
            call frod % set_r8_0('fpowr', fpowr)
            call frod % set_i4_0('maxit', maxit)
            call frod % set_r8_0('tref', tref)
            call frod % set_i4_0('tape1', tape1)
            call frod % set_i4_0('jtr', jtr)
            call frod % set_r8_0('pelh', pelh)
            call frod % set_r8_0('pdrato', pdrato)
            call frod % set_i4_0('NRestart', NRestart)
            call frod % set_i4_0('irupt', irupt)
            call frod % set_r8_0('tgas0', tgas0)
            call frod % set_r8_0('tsntrk', tsntrk)
            call frod % set_i4_0('CladType', CladType)
            call frod % set_r8_0('spdbp', spdbp)
            call frod % set_i4_0('odoxid', odoxid)
            call frod % set_r8_0('achn', achn)
            call frod % set_i4_0('nchn', nchn)
            call frod % set_r8_0('tflux', tflux)
            call frod % set_i4_0('TranSwell', TranSwell)
            call frod % set_r8_0('RodLength', RodLength)
            call frod % set_i4_0('noiter', noiter)
            call frod % set_i4_0('nthermex', nthermex)
            call frod % set_i4_0('ncmesh', ncmesh)
            call frod % set_r8_0('OpenPorosityFraction', OpenPorosityFraction)
            call frod % set_r8_0('zad', zad)
            call frod % set_r8_0('rshrd', rshrd)
            call frod % set_i4_0('IndexFC2Print', IndexFC2Print)
            call frod % set_r8_0('doffst', doffst)
            call frod % set_i4_0('irefine', irefine)
            call frod % set_r8_0('emptm', emptm)
            call frod % set_i4_0('ProtectiveOxide', ProtectiveOxide)
            call frod % set_r8_0('trise', trise)
            call frod % set_r8_0('fltgap2', fltgap2)
            call frod % set_r8_0('hydiam', hydiam)
            call frod % set_r8_0('dishd', dishd)
            call frod % set_r8_0('ph', ph)
            call frod % set_r8_0('hrad', hrad)
            call frod % set_r8_0('dtss', dtss)
            call frod % set_r8_0('bup', bup)
            call frod % set_r8_0('cldwdc', cldwdc)
            call frod % set_r8_0('timop', timop)
            call frod % set_i4_0('nIDoxide', nIDoxide)
            call frod % set_i4_0('IndexGrainBndSep', IndexGrainBndSep)
            call frod % set_r8_0('cfluxa', cfluxa)
            call frod % set_r8_0('rvoid', rvoid)
            call frod % set_r8_0('dofset', dofset)
            call frod % set_i4_0('grass', grass)
            call frod % set_r8_0('pl', pl)
            call frod % set_i4_0('ncolbp', ncolbp)
            call frod % set_r8_0('fltgap', fltgap)
            call frod % set_i4_0('presfgr', presfgr)
            call frod % set_r8_0('frpo2', frpo2)
            call frod % set_r8_0('trest', trest)
            call frod % set_i4_0('inp', inp)
            call frod % set_r8_0('fgrns', fgrns)
            call frod % set_r8_0('refine', refine)
            call frod % set_r8_0('modheat', modheat)
            call frod % set_r8_0('tmpac1', tmpac1)
            call frod % set_r8_0('coldw', coldw)
            call frod % set_r8_0('dhe', dhe)
            call frod % set_r8_0('explenumv', explenumv)
            call frod % set_r8_0('dhy', dhy)
            call frod % set_i4_0('naz', naz)
            call frod % set_i4_0('jchf', jchf)
            call frod % set_r8_0('volbp', volbp)
            call frod % set_r8_0('rshd', rshd)
            call frod % set_i4_0('profile', profile)
            call frod % set_r8_0('fotmtl', fotmtl)
            call frod % set_r8_0('gsms', gsms)
            call frod % set_r8_0('dishv0', dishv0)           
            call frod % set_i4_0('nsym', nsym)
            call frod % set_r8_0('rnbnt', rnbnt)
            call frod % set_r8_0('zvoid2', zvoid2)
            call frod % set_r8_0('gapthk', gapthk)
            call frod % set_r8_0('zvoid1', zvoid1)
            call frod % set_r8_0('zs', zs)
            call frod % set_r8_0('FuelPelDiam', FuelPelDiam)
            call frod % set_r8_1('scd', scd)
            call frod % set_r8_1('azpang', azpang)
            call frod % set_r8_1('htclev', htclev)
            call frod % set_r8_1('ExtentOfBow', ExtentOfBow)
            call frod % set_r8_1('vplen', vplen)
            call frod % set_r8_1('gadoln', gadoln)
            call frod % set_r8_1('gfrac', gfrac)
            call frod % set_r8_1('gbse', gbse)
            call frod % set_r8_1('fluxz', fluxz)
            call frod % set_r8_1('nodchf', nodchf)
            call frod % set_i4_1('ngastmp', ngastmp)
            call frod % set_r8_1('swd', swd)
            call frod % set_r8_1('oxideod', oxideod)
            call frod % set_r8_1('cexh2a', cexh2a)
            call frod % set_r8_2('pazp', pazp)
            call frod % set_r8_1('radpel', radpel)
            call frod % set_r8_1('gappr0', gappr0)
            call frod % set_r8_1('butemp', butemp)
            call frod % set_r8_1('oxideid', oxideid)
            call frod % set_r8_1('spl', spl)
            call frod % set_r8_1('eppinp', eppinp)
            call frod % set_r8_1('techf', techf)
            call frod % set_i4_1('ncs', ncs)
            call frod % set_r8_1('tschf', tschf)
            call frod % set_r8_1('zelev', zelev)
            call frod % set_r8_1('fmesh', fmesh)
            call frod % set_r8_1('cmesh', cmesh)

            do i = 1, 1

                call frod % set_r8_0('hbh', hbh(2*i-1))
                call frod % set_r8_0('hupta', hupta(2*i-1))
                call frod % set_r8_0('hinta', hinta(2*i-1))
                call frod % set_r8_0('gbh', gbh(2*i-1))
                call frod % set_r8_0('explenumt', explenumt(2*i-1))
                call frod % set_r8_0('pbh2', pbh2(2*i-1))
                call frod % set_r8_0('RodAvePower', RodAvePower(2*i-1))
                call frod % set_r8_1('htca', htca(2*i-1,:))
                call frod % set_r8_1('tblka', tblka(2*i-1,:))
                call frod % set_r8_1('gasths', gasths(2*i-1,:))
                call frod % set_r8_0('FuelGasSwell', FuelGasSwell(2*i-1))
                call frod % set_r8_0('temptm', temptm(2*i-1))
                call frod % set_r8_0('relfraca', relfraca(2*i-1))
                call frod % set_r8_0('prestm', prestm(2*i-1))
                call frod % set_r8_0('fldrat', fldrat(2*i-1))
                call frod % set_r8_0('gasphs', gasphs(2*i-1))
                call frod % set_r8_1('axpowprofile', AxPowProfile(:,2*i-1))
                call frod % set_r8_0('pbh1', pbh1(2*i-1))
                call frod % set_r8_0('hlqcl', hlqcl(2*i-1))
                call frod % set_r8_1('radtemp', (/( radpowprofile(2*j-1 + 2*naxialnodes*(i-1)), j = 1, naxialnodes )/) )
                call frod % set_r8_1('fuelrad', (/( radpowprofile(2*j   + 2*naxialnodes*(i-1)), j = 1, naxialnodes )/) )

                if (i == 1) call frod % init()

                !call frod % next(dtmaxa(2*i))

            enddo

    end select

    write(*,*) 'Successfuly done!'

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