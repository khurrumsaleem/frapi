program test
    use fpn4rastk, only : init, next, get, set, stp0

    implicit none

    character(len = 32) :: iname
    character(len = 32) :: oname

    integer :: i_input_file = 42
    integer :: i_output_file = 43

    integer(4) :: n_fuel_rad_in
    integer(4) :: na_r
    integer(4) :: na_in
    integer(4) :: n_bu
    integer(4) :: run_media
    integer(4) :: iz
    integer(4) :: i_bu_step

    real(8) :: init_enrich
    real(8) :: pitch_in
    real(8) :: gap_rad
    real(8) :: clad_rad
    real(8) :: fuel_rad
    real(8) :: inlet_temp_in
    real(8) :: mass_flow_rate_in
    real(8) :: init_den
    real(8) :: bu_step

    integer(4), allocatable :: z_meshes(:)

    real(8), allocatable  :: pow_hist(:,:)
    real(8), allocatable  :: line_pow_hist(:,:)    
    real(8), allocatable  :: thickness_RASTK(:)
    real(8), allocatable  :: thickness_FRPCN(:)
    real(8), allocatable  :: height_FRPCN(:)
    real(8), allocatable  :: height_RASTK(:)
    real(8), allocatable  :: tmp_time(:)
    real(8), allocatable  :: pow_hist_loc(:,:)
    real(8), allocatable  :: power_dist_in(:)
    real(8), allocatable  :: ctf_coo_pres(:,:)
    real(8), allocatable  :: ctf_coo_temp(:,:)
    real(8), allocatable  :: ctf_clad_temp(:,:)

    real(8), allocatable   ::  fue_avg_temp(:)
    real(8), allocatable   ::  coo_avg_temp(:)
    real(8), allocatable   ::  fue_dyn_hgap(:)
    real(8), allocatable   ::  t_oxidelayer(:)
    real(8), allocatable   ::  t_fuecladgap(:)
    real(8), allocatable   ::  gap_pressure(:)
    real(8), allocatable   ::  contact_pres(:)
    real(8), allocatable   ::  hoop_strain (:)
    real(8), allocatable   ::  hoop_stress (:)

    ! ARRAYS FOR FRAPCON
    real(8), allocatable  :: power_FRPCN(:)
    real(8), allocatable  :: tcool_FRPCN(:)
    real(8), allocatable  :: pcool_FRPCN(:)
    real(8), allocatable  :: zmesh_FRPCN(:)
    real(8)               :: fcool_FRPCN(1)

    ! ITERATIONAL VARIABLES
    integer :: i

    ! READING INPUT FILE

    call get_command_argument(1, iname)
    call get_command_argument(2, oname)

    open(i_input_file, file=iname, status='old')

    read(i_input_file,*) n_fuel_rad_in
    read(i_input_file,*) init_enrich
    read(i_input_file,*) pitch_in
    read(i_input_file,*) fuel_rad
    read(i_input_file,*) gap_rad
    read(i_input_file,*) clad_rad

    read(i_input_file,*) inlet_temp_in   
    read(i_input_file,*) mass_flow_rate_in
    read(i_input_file,*) na_in, na_r

    allocate(z_meshes(1:na_in))
    allocate(thickness_RASTK(1:na_r))
    allocate(thickness_FRPCN(1:na_in))

    read(i_input_file,*) z_meshes(1:na_in)
    read(i_input_file,*) thickness_RASTK(1:na_r)
    read(i_input_file,*) init_den
    read(i_input_file,*) n_bu
    read(i_input_file,*) run_media

    allocate(height_RASTK(1:na_r +1))
    allocate(height_FRPCN(1:na_in+1))
    allocate(tmp_time(1:n_bu))
    allocate(pow_hist(1:na_in,1:n_bu))
    allocate(pow_hist_loc(1:na_in,1:n_bu))
    allocate(line_pow_hist(1:na_r,1:n_bu))
    allocate(power_dist_in(1:na_in))
    allocate(ctf_coo_pres(1:na_in,1:n_bu))
    allocate(ctf_coo_temp(1:na_in,1:n_bu))
    allocate(ctf_clad_temp(1:na_in,1:n_bu))

    read(i_input_file,*) tmp_time(1:n_bu)  

    select case (run_media)
        case(1)
            read(i_input_file,*) (ctf_coo_temp(iz,:), iz = 1, na_in)
            read(i_input_file,*) (ctf_coo_pres(iz,:), iz = 1, na_in)
            read(i_input_file,*) (line_pow_hist(iz,:), iz = 1, na_r)
        case(2)
            read(i_input_file,*) (ctf_clad_temp(iz,:), iz = 1, na_in)
        case default
            read(i_input_file,*) (line_pow_hist(iz,:), iz = 1, na_r)
    end select

    close(i_input_file)

    allocate(power_FRPCN(1:na_in))
    allocate(tcool_FRPCN(1:na_in))
    allocate(pcool_FRPCN(1:na_in))
    allocate(zmesh_FRPCN(1:na_in))

    allocate(fue_avg_temp(1:na_in))
    allocate(coo_avg_temp(1:na_in))
    allocate(fue_dyn_hgap(1:na_in))
    allocate(t_oxidelayer(1:na_in))
    allocate(t_fuecladgap(1:na_in))
    allocate(gap_pressure(1:na_in))
    allocate(hoop_strain (1:na_in))
    allocate(hoop_stress (1:na_in))

    ! HEIGHTS FOR RASTK and FRAPCON, LIKE (0.5, 1.0, 1.5, 2.0, ...)
    height_RASTK(1)  = 0.
    height_RASTK(2:) = (/( sum(thickness_RASTK(1:i)) , i = 1, na_r )/)
    height_FRPCN(1)  = 0.
    height_FRPCN(2:) = (/( height_RASTK(sum(z_meshes(1:i))+1), i = 1, na_in )/)
    thickness_FRPCN(:) = height_FRPCN(2:na_in+1) - height_FRPCN(1:na_in)

    ! FRAPCON INITIALIZATION

    call init(n_fuel_rad_in-1, na_in, thickness_FRPCN, fuel_rad, gap_rad, &
              clad_rad, pitch_in, init_den, init_enrich)

    ! FRAPCON RUN

    do i_bu_step = 1, n_bu

        power_FRPCN(1) = sum( line_pow_hist( 1:z_meshes(1), i_bu_step) * thickness_RASTK(1:z_meshes(1)) ) &
                               / sum( thickness_RASTK(1:z_meshes(1)) )

        power_FRPCN(2:na_in) = (/( sum( line_pow_hist(sum(z_meshes(:i)):sum(z_meshes(:i+1)),i_bu_step) &
                                 * thickness_RASTK(sum(z_meshes(:i)):sum(z_meshes(:i+1))) ) &
                                 / sum( thickness_RASTK(sum(z_meshes(:i)):sum(z_meshes(:i+1)))), i = 1, na_in-1 )/)

        tcool_FRPCN(:) = ctf_coo_temp(:,i_bu_step)
        pcool_FRPCN(:) = ctf_coo_pres(:,i_bu_step)
        fcool_FRPCN(1) = mass_flow_rate_in

        ! SETUP THE UPDATED VARIABLES
        call set("linear power", power_FRPCN)
        call set("coolant temperature", tcool_FRPCN)
        call set("coolant pressure", pcool_FRPCN)
        call set("coolant mass flux", fcool_FRPCN)

        ! VERY FIRST TIME STEP
        if (i_bu_step == 1) call stp0()

        ! REGULAR TIME STEP
        if (i_bu_step >  1) call next(tmp_time(i_bu_step) - tmp_time(i_bu_step-1))

    enddo

    ! GET OUTPUT VARIABLES FROM FRAPCON
    call get('axial fuel temperature, C', fue_avg_temp)
    call get('bulk coolant temperature, C', coo_avg_temp)
    call get('gap conductance, W/(m^2*K)', fue_dyn_hgap)
    call get('oxide thickness, um', t_oxidelayer)
    call get('mechanical gap thickness, um', t_fuecladgap)
    call get('gap pressure, MPa', gap_pressure)
    call get('cladding hoop strain, %', hoop_strain)
    call get('cladding hoop stress, MPa', hoop_stress)
    call get('axial mesh, cm', zmesh_FRPCN)

    open(i_output_file, file=oname, status='unknown')

    do i = 1, na_in
        write(i_output_file,'(100es13.5)') fue_avg_temp(i), coo_avg_temp(i), &
        fue_dyn_hgap(i), t_oxidelayer(i) , t_fuecladgap(i), gap_pressure(i), &
        hoop_strain(i), hoop_stress(i), zmesh_FRPCN(i)
    enddo

    write(*,*) 'done!'

end program test