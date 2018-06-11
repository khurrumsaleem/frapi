program test

    use fuelrod, only : frod_type

    implicit none

    character(len = 32) :: string
    character(len = 32) :: iname
    character(len = 32) :: oname

    integer :: i_input_file = 42
    integer :: i_output_file = 43

    integer(4) :: n_fuel_rad_in
    integer(4) :: na_r
    integer(4) :: na_in
    integer(4) :: n_bu       ! Number of burnup steps
    integer(4) :: n_iter     ! Number of coupling iterations
    integer(4) :: run_media
    integer(4) :: iz
    integer(4) :: i_bu_step
    integer(4) :: n_frod     ! Number of fuel rods
    integer(4) :: i_frod
    integer(4) :: i_iter

    real(8) :: init_enrich
    real(8) :: pitch_in
    real(8) :: gap_rad
    real(8) :: clad_rad
    real(8) :: fuel_rad
    real(8) :: inlet_temp_in
    real(8) :: mass_flow_rate_in
    real(8) :: init_den
    real(8) :: dtime

    ! ARRAY OF FUEL RODS
    type(frod_type), allocatable :: frod(:)

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
    real(8), allocatable  :: power(:)

    real(8), allocatable   ::  fue_avg_temp(:)
    real(8), allocatable   ::  coo_avg_temp(:)
    real(8), allocatable   ::  fue_dyn_hgap(:)
    real(8), allocatable   ::  t_oxidelayer(:)
    real(8), allocatable   ::  t_fuecladgap(:)
    real(8), allocatable   ::  gap_pressure(:)
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
    allocate(power(1:na_r))

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

    ! ALLOCATE FUEL RODS ARRAY
    n_frod = 2
    n_iter = 2

    allocate(frod(n_frod))

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

    !-------------------- FUEL RODS INITIALIZATION-----------------------------

    ! HEIGHTS FOR RASTK and FRAPCON, LIKE (0.5, 1.0, 1.5, 2.0, ...)
    height_RASTK(1)  = 0.
    height_RASTK(2:) = (/( sum(thickness_RASTK(1:i)) , i = 1, na_r )/)
    height_FRPCN(1)  = 0.
    height_FRPCN(2:) = (/( height_RASTK(sum(z_meshes(1:i))+1), i = 1, na_in )/)
    thickness_FRPCN(:) = height_FRPCN(2:na_in+1) - height_FRPCN(1:na_in)

    ! arguments must be the same for all fuel rods
    do i_frod = 1, n_frod
        call frod(i_frod) % make(n_fuel_rad_in-1, na_in, thickness_FRPCN, fuel_rad, gap_rad, &
                                 clad_rad, pitch_in, init_den, init_enrich)
    enddo

    !-------------------- INITIAL VALUE ---------------------------------------

    i_bu_step = 1

    ! SET INITIAL VALUE
    do i_frod = 1, n_frod

        ! LEAD LINEAR POWER HISTORY TO THE FRAPCON INPUT
        power = line_pow_hist(:, i_bu_step) * thickness_RASTK(:)
        power_FRPCN(1)  = sum(power(:z_meshes(1))) / thickness_FRPCN(1)
        power_FRPCN(2:) = (/( sum(power(sum(z_meshes(:i-1))+1:sum(z_meshes(:i)))) / thickness_FRPCN(i), i = 2, na_in )/)

        tcool_FRPCN(:) = ctf_coo_temp(:,i_bu_step)
        pcool_FRPCN(:) = ctf_coo_pres(:,i_bu_step)
        fcool_FRPCN(1) = mass_flow_rate_in

        ! SETUP THE UPDATED VARIABLES
        call frod(i_frod) % set("linear power, W/cm", power_FRPCN)
        call frod(i_frod) % set("coolant temperature, C", tcool_FRPCN)
        call frod(i_frod) % set("coolant pressure, MPa", pcool_FRPCN)
        call frod(i_frod) % set("coolant mass flux, kg/(s*m^2)", fcool_FRPCN)

        call frod(i_frod) % init()
    enddo

    !------------------- RUN TIME STEPS ---------------------------------------

    ! ITERATION OVER TIME
    do i_bu_step = 2, n_bu

        dtime = tmp_time(i_bu_step) - tmp_time(i_bu_step-1)

        ! ITERATION OVER FUEL RODS
        do i_frod = 1, n_frod

            ! ITERATION BETWEEN RAST-K AND FRAPCON CODES
            do i_iter = 1, n_iter

                ! LEAD LINEAR POWER HISTORY TO THE FRAPCON INPUT
                power = line_pow_hist(:, i_bu_step) * thickness_RASTK(:)
                power_FRPCN(1)  = sum(power(:z_meshes(1))) / thickness_FRPCN(1)
                power_FRPCN(2:) = (/( sum(power(sum(z_meshes(:i-1))+1:sum(z_meshes(:i)))) / thickness_FRPCN(i), i = 2, na_in )/)

                tcool_FRPCN(:) = ctf_coo_temp(:,i_bu_step)
                pcool_FRPCN(:) = ctf_coo_pres(:,i_bu_step)
                fcool_FRPCN(1) = mass_flow_rate_in

                ! SETUP THE UPDATED VARIABLES
                call frod(i_frod) % set("linear power, W/cm", power_FRPCN)
                call frod(i_frod) % set("coolant temperature, C", tcool_FRPCN)
                call frod(i_frod) % set("coolant pressure, MPa", pcool_FRPCN)
                call frod(i_frod) % set("coolant mass flux, kg/(s*m^2)", fcool_FRPCN)

                ! DO TRIAL TIME STEP
                call frod(i_frod) % next(dtime)

                ! TAKE OUTPUT VARIABLES FROM FRAPCON
                call frod(i_frod) % get('axial fuel temperature, C', fue_avg_temp)
                call frod(i_frod) % get('bulk coolant temperature, C', coo_avg_temp)
                call frod(i_frod) % get('total gap conductance, W/(m^2*K)', fue_dyn_hgap)
                call frod(i_frod) % get('oxide thickness, um', t_oxidelayer)
                call frod(i_frod) % get('mechanical gap thickness, um', t_fuecladgap)
                call frod(i_frod) % get('gap pressure, MPa', gap_pressure)
                call frod(i_frod) % get('cladding hoop strain, %', hoop_strain)
                call frod(i_frod) % get('cladding axial stress, MPa', hoop_stress)
                call frod(i_frod) % get('axial mesh, cm', zmesh_FRPCN)

                ! ACCEPT THE LAST TRIAL TIME STEP
                if(i_iter == n_iter) call frod(i_frod) % accept()
            enddo

            ! WRITE and READ FUEL ROD STATE FROM A BINARY FILE
            write(string, '(A,I0.6,A,I0.6,A)') 'burnup_', i_bu_step, '_frod_', i_frod, '.bin'
            call frod(i_frod) % save(string)
            call frod(i_frod) % load(string)

        enddo

    enddo

    !----------------------------- SAVE LAST STATE ------------------------------------

    open(i_output_file, file=oname, status='unknown')

    do i = 1, na_in
        write(i_output_file,'(100es13.5)') fue_avg_temp(i), coo_avg_temp(i), &
        fue_dyn_hgap(i), t_oxidelayer(i) , t_fuecladgap(i), gap_pressure(i), &
        hoop_strain(i), hoop_stress(i), zmesh_FRPCN(i)
    enddo

    write(*,*) 'Test done!'

end program test