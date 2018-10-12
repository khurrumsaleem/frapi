module m_save2file
    implicit none
    contains
    subroutine fpt_save2file
    ! EOSTemp(j)            - EOS Temperature at calculation node j
    ! EOSRad(i,j)           - radius to node j at time t
    ! RadialBound           - cold state radius to node j (reference for strains)
    ! sigmah                - cladding hoop stress
    ! sigmaz                - cladding axial stress
    ! CldPermAxStrn         - permanent hoop strain in cladding
    ! CldPermHoopStrn       - permanent axial strain in clad
    ! Ih                    - rod-to-coolant heat transfer mode
    ! n1                    - Dimension of radial node vectors
    ! n2                    - Dimension of axial node vectors. 1 = implied dimension for no. of fuel rods
    ! nmesh                 - number of nodes in mesh - to outer surface of clad
    ! ncladi                - number of nodes to gas gap / clad boundary
    ! igpnod                - node number at surface of fuel pellet stack
    ! modfd                 - deformation model indicator
    ! naxn                  - number of axial nodes
    ! IterationCount        - iteration count
    ! RuptFailIndex         - rupture indicator ( 1 --# yes )
    ! CladCollapseIndex     - buckled clad indicator ( 1 --# yes )
    ! unit                  - .TRUE. --# i/o in british units, .FALSE. --# s.i.
    ! t                     - current time
    ! GasPress              - gap pressure (psia)
    ! AxialPowr             - local fuel rod power   (kW/ftom)
    ! delth                 - fuel pellet stack axial displacement
    ! dcldh                 - cladding axial displacement
    ! HeatFlux              - surface heat flux (btu/sec-ftom**2)
    ! AxNodElevat           - axial position
    ! CritHtFlux            - critical heat flux (btu/sec-ftom**2)
    ! powave                - average fuel rod power (kW/ftom)
    ! CoolPress             - pressure in coolant channel (psia)
    ! flwblk                - percent flow blockage, If ruptured
    ! ruptem                - temperature (F) at rupture
    ! tp                    - temperature of plenum gas (F)
    ! flowg                 - rate of flow of gas from plenum (gm-moles/sec)
    ! FilmCoeffAv           - heat transfer coeficient at rod surface (btu/hr-ftom**2-F)
    ! HGapAv                - heat transfer coeficient across gas gap (btu/hr-ftom**2-F)
    ! EOSOxideThick         - depth of metal-water reaction on cladding (inches)
    ! WatrMetlEnrgy         - energy generated by metal-water reaction on cladding surface (kW/ftom)
    ! AxialNodLen(n)        - length associated with axial node n (ftom)
    ! CldPermStrn(k)        - cladding permanent radial strain
    ! EnrgyMeltZ(ll,k)      - energy absorbed in melting by l-th half mesh (two half meshes per radial node) (btu/ftom**3)
    ! qmaxmelt(ll)          - maximum energy half mesh ll can absorb in melting (btu/ftom**3)
    ! n3                    - total number of half meshes = 2*n1
    ! bup                   - fuel burnup (mwd/mtu)
    ! frpo2                 - fraction of fuel by weight that is plutonium
    ! tmelt(1)              - melt temperature of fuel (F)
    ! RInterfacGap(k)       - gap thickness at pellet interfaces (ftom)
    ! RInterfacPrs(k)       - interface pressure between fuel and cladding, psi
    ! rbmax                 - cladding radial displacement at peak ballooning point,in.
    ! CladEffStress(k)      - cladding effective stress (psi)
    ! modfal(jrod)          - mode of cladding failure. see sub. fala for detai
    ! TotalVoidVol          - total fuel rod void volume (ftom**3)
    ! EffStrain(k)          - effective plastic strain at axial node k,cladding
    ! EinstabilStrain(k)    - instability strain of cladding, axial node k
    ! StressAtInstStrain(k) - stress at instability strain, psi
    ! CladYieldStress(k)    - work-hardened yield stress, psi
    !
    ! common block  frcb  is in Subroutines fraptran, deform, initia,frap, cardin, heat, and restrw
    ! it stores in lcm arrays used by fracas-2 subcode. (sub. fcmi2)
    ! common block thcntl is link whereby thermal-hydraulic code controls the FrapTran calculations
    !
    ! common block bloona contains balon2 and far1 variables that are used outside of comput, or are required for restart
    !
    !    balon2 variables
    ! chstrs = maximum hoop stress in balloon region (psi)
    ! frbal  = balon2 circular/radial model switch
    ! pmxbal = balon2 maximum gap pressure (Pa)
    ! r8bal  = balon2 average radius at balon2 axial node 8 (m)
    ! tcebal = maximum circumferential strain in balloon region
    ! ifbaln = 1 If balon2 predicts failure
    ! jmnbal = balon2 radial contact node
    ! kntbal = 1 If balon2 predicts pellet-clad contact
    ! nbncal = 1 If balon2 has been called once
    !
    !    far1 variables
    ! pdrato = bundle pitch to rod diameter ratio
    ! rnbnt  = ratio of balloonable cells to total cells
    ! totnb  = total number of cells in a bundle
    ! nodprm = number flow area reduction subnodes
    ! farbal = flow area reduction   (m**2/m**2)
    ! sdfar  = standard deviation of flow area reduction
    ! zfarbl = axial location flow area reduction subnodes  (m)
    subroutine fpt_save2file
end module m_save2file