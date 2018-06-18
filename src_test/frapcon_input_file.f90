program frapcon_input_file

    use conversions
    use fpn_reader
    use fuelrod, only : frod_type

    implicit none

    type (frod_type) :: frod

    logical :: verbose = .false.

    character(len=256) :: filename

    ! ITERATIONAL VARIABLES
    integer :: i

    real(8) :: or_pellet
    real(8) :: or_clad
    real(8) :: ir_clad

    real(8), allocatable :: thickness(:)

    ! READING INPUT FILE
    call get_command_argument(1, filename)

    call read_frapcon_file(filename)

    allocate(thickness(na))

    !-------------------- FUEL ROD INITIALIZATION-----------------------------
    or_clad   = 0.5d0 * dco(1)
    ir_clad   = or_clad - thkcld(1)
    or_pellet = ir_clad - thkgap(1)
    thickness = x(2:na+1) - x(1:na)

    call frod % make(nr, na, ngasr, nce, or_pellet, ir_clad, &
                 or_clad, pitch, den, enrch(1), thickness, verbose, &
                 mechan, ngasmod, icm, icor, iplant, &
                 imox, igascal, zr2vintage, moxtype, idxgas)

    call frod % set_value("additional fuel densification factor", afdn)
    call frod % set_value("clad texture factor", catexf)
    call frod % set_value("as-fabricated clad hydrogen content, wt.ppm", chorg)
    call frod % set_value("clad cold work", cldwks)
    call frod % set_value("cold plenum length", cpl)
    call frod % set_value("constant crud thickness", crdt)
    call frod % set_value("crud accumulation rate", crdtr)
    call frod % set_value("creep step duration, hr", crephr)
    call frod % set_value("fuel open porosity fraction, %TD", deng)
    call frod % set_value("spring diameter, mm", dspg)
    call frod % set_value("spring wire diameter, mm", dspgw)
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
    call frod % set_array("input fuel burnup", buin / MWskgUtoMWdMTU)
    call frod % set_array("PuO2 weight percent if MOX fuel, wt%", comp)
    call frod % set_array("gadolinia content at each axial node", gadoln)
    call frod % set_array("end-node to plenum heat transfer fraction", qend)
    call frod % set_array("radius of the fuel pellet central annulus, mm", rc * intomm)
    call frod % set_array("cladding surface temperature, K", (/( tfk(cladt(i)), i = 1, na )/) )
    call frod % set_array("rod internal pressure for each time tep for FEA model, MPa", p1/patoPSI)
    call frod % set_array("axial crud thickness multiplier", crudmult)

    !------------------- RUN TIME STEPS ---------------------------------------
!
!    ! ITERATION OVER TIME
!    do i_bu_step = 1, n_bu

             ! ITERATION BETWEEN RAST-K AND FRAPCON CODES
!            do i_iter = 1, n_iter
!
!                ! LEAD LINEAR POWER HISTORY TO THE FRAPCON INPUT
!                power = line_pow_hist(:, i_bu_step) * thickness_RASTK(:)
!                power_FRPCN(1)  = sum(power(:z_meshes(1))) / thickness_FRPCN(1)
!                power_FRPCN(2:) = (/( sum(power(sum(z_meshes(:i-1))+1:sum(z_meshes(:i)))) / thickness_FRPCN(i), i = 2, na_in )/)
!
!                tcool_FRPCN(:) = ctf_coo_temp(:,i_bu_step)
!                pcool_FRPCN(:) = ctf_coo_pres(:,i_bu_step)
!                fcool_FRPCN(1) = mass_flow_rate_in
!
!                ! SETUP THE UPDATED VARIABLES
!                call frod(i_frod) % set("linear power, W/cm", power_FRPCN)
!                call frod(i_frod) % set("coolant temperature, C", tcool_FRPCN)
!                call frod(i_frod) % set("coolant pressure, MPa", pcool_FRPCN)
!                call frod(i_frod) % set("inlet coolant temperature, C", tcool_FRPCN(:2))
!                call frod(i_frod) % set("inlet coolant pressure, MPa", pcool_FRPCN(:2))
!                call frod(i_frod) % set("coolant mass flux, kg/(s*m^2)", fcool_FRPCN)
!
!                if(i_bu_step == 1) then
!                    ! DO INITIAL TIME STEP
!                    call frod(i_frod) % init()
!                else
!                    ! DO TRIAL TIME STEP
!                    dtime = tmp_time(i_bu_step) - tmp_time(i_bu_step-1)
!                    call frod(i_frod) % next(dtime)
!                endif
!
!                ! TAKE OUTPUT VARIABLES FROM FRAPCON
!                call frod(i_frod) % get('axial fuel temperature, C', fue_avg_temp)
!                call frod(i_frod) % get('bulk coolant temperature, C', coo_avg_temp)
!                call frod(i_frod) % get('total gap conductance, W/(m^2*K)', fue_dyn_hgap)
!                call frod(i_frod) % get('oxide thickness, um', t_oxidelayer)
!                call frod(i_frod) % get('mechanical gap thickness, um', t_fuecladgap)
!                call frod(i_frod) % get('gap pressure, MPa', gap_pressure)
!                call frod(i_frod) % get('cladding hoop strain, %', hoop_strain)
!                call frod(i_frod) % get('cladding axial stress, MPa', hoop_stress)
!                call frod(i_frod) % get('axial mesh, cm', zmesh_FRPCN)
!
!                ! ACCEPT THE LAST TRIAL TIME STEP
!                if(i_iter == n_iter) call frod(i_frod) % accept()
!            enddo
!
!    enddo

    !----------------------------- DEALLOCATE THE FUEL RODS --------------------------
    call frod % destroy()

    write(*,*) 'Test done!'

end program frapcon_input_file