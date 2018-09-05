MODULE Restart
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon
    USE Comde
    USE FissionGas
    IMPLICIT NONE
    !>@brief
    !> This module contains the following restart information:
    !> 1) Reads and writes to a FRAPCON Restart File
    !> 2) FRAPCON to FRAPTRAN restart file
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/8/2014
    !
    CONTAINS
    !
    SUBROUTINE WriteRestart
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine writes to the restart file
    ! *** NOTE: This subroutine is not a working part of FRAPCON-4.0 release ***
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/8/2014
    !
    CHARACTER(LEN=70) :: Description = 'FRAPCON'
    !
    ! **************************************
    ! ***** Time Independent Variables *****
    ! **************************************
    !
    IF (it == 1) THEN
        WRITE (ounit,100)
        WRITE (0,100)
        WRITE (ntaps) Description
        ! ************************
        ! *** Variables Module ***
        ! ************************
        WRITE (ntaps) im, na, ngasr, nr, nce
        WRITE (ntaps) cvv, vfrcpl
        WRITE (ntaps) icm, imox, iplant, zr2vintage, ngasmod
        WRITE (ntaps) ncreep
        WRITE (ntaps) nvoid
        WRITE (ntaps) gapmin, gaph, gapt
        WRITE (ntaps) airin
        WRITE (ntaps) angi
        WRITE (ntaps) an2in, argin, avflux
        WRITE (ntaps) avgqi
        WRITE (ntaps) cfv, cfva
        WRITE (ntaps) cpv
        WRITE (ntaps) ctemp, dcobol, delbp, delbu
        WRITE (ntaps) deltfc, deltjl
        WRITE (ntaps) dhfll
        WRITE (ntaps) fgin
        WRITE (ntaps) fpor1
        WRITE (ntaps) gases
        WRITE (ntaps) hein, hfll
        WRITE (ntaps) hfper
        WRITE (ntaps) hpl
        WRITE (ntaps) hporv, hpv, hva
        WRITE (ntaps) kryin, nu
        WRITE (ntaps) rhofuel
        WRITE (ntaps) rp, rstran, rtran
        WRITE (ntaps) t, thvv
        WRITE (ntaps) tfgfr, thefr, th2ofr, tn2fr
        WRITE (ntaps) fnck
        WRITE (ntaps) fncn, cwkf, cwnf, coldwk, frcoef
        WRITE (ntaps) afal, afdn, afgr
        WRITE (ntaps) amfair, amffg
        WRITE (ntaps) catexf, chorg, cldwks, cpl, crephr
        WRITE (ntaps) crdt, crdtr, dishsd
        WRITE (ntaps) fa, fgpav, fotmtl
        WRITE (ntaps) chmfrh, chmfrw, hdish, hplt
        WRITE (ntaps) pitch, ppmh2o, ppmn2, rdish, roughc
        WRITE (ntaps) roughf, sgapf, tcc
        WRITE (ntaps) rsntr, tsint, grnsize
        WRITE (ntaps) b10, zrb2thick, zrb2den, prvden, ifba
        WRITE (ntaps) excesh2, frden
    ELSE
        ! **************************************
        ! ****** Time Dependent Variables ******
        ! **************************************
        WRITE (ounit,110) it-1, ProblemTime(it)
        ! ************************
        ! *** Variables Module ***
        ! ************************
        ! Write the time-step (it-1) and the time in seconds
        WRITE (ntaps) it-1, ProblemTime(it)
        WRITE (ntaps) bu, buold
        WRITE (ntaps) FastFluxd
        WRITE (ntaps) FastFluenced
        ! ***Axial-Dependent Arrays, dimensioned (na)***
        WRITE (ntaps) IgapGapIndex
        WRITE (ntaps) IgapIndexOld
        WRITE (ntaps) IgapIndexPrevOld
!        WRITE (ntaps) dpwxrate
        WRITE (ntaps) axialnode                   ! Axial node dimensions
        WRITE (ntaps) PelletRad
        WRITE (ntaps) sigeff
        WRITE (ntaps) FuelSurfDispl
        WRITE (ntaps) CladInSurDisp
        WRITE (ntaps) CladInPermDef
        WRITE (ntaps) sigy
        WRITE (ntaps) AxialNodLength
        WRITE (ntaps) CladEffPlasStrain
        WRITE (ntaps) RinterfacPress
        WRITE (ntaps) FuelCladGap
        WRITE (ntaps) GapPress
        WRITE (ntaps) CoolantPress
        WRITE (ntaps) PlastStrnep1
        WRITE (ntaps) OldCladStrn
        WRITE (ntaps) OldFuelStrn
        WRITE (ntaps) OldGapPress
        WRITE (ntaps) OldCoolPress
        WRITE (ntaps) OldFuelDispl
        WRITE (ntaps) OldCladAvTemp
        WRITE (ntaps) CreepStrain
        WRITE (ntaps) CreepStrain1
        WRITE (ntaps) CladDiamHot
        WRITE (ntaps) PrevCladEffPlasStrn
        WRITE (ntaps) PrevFuelStrain
        WRITE (ntaps) HotNodLength
!        WRITE (ntaps) AxMaxFuelDispl
        WRITE (ntaps) PermFuelDispl
        WRITE (ntaps) BOSNodeburnup
        WRITE (ntaps) EOSNodeburnup
        WRITE (ntaps) StepNodeburnup
        WRITE (ntaps) BOSZrO2Thk
        WRITE (ntaps) EOSZrO2Thk
        WRITE (ntaps) ZrO2ThkNoAd
!        WRITE (ntaps) AnnulusVolume
        WRITE (ntaps) PrevOldCoolPress
        WRITE (ntaps) PrevOldGapPress
        WRITE (ntaps) PrevOldCladAvTemp
        WRITE (ntaps) PrevOldFuelDispl
        WRITE (ntaps) PrevCreepStrain
        WRITE (ntaps) CladIrradGrowStrn
        WRITE (ntaps) UniformAxNodStrn
        WRITE (ntaps) CladH2Concen
        WRITE (ntaps) ExcessH2Concen
        WRITE (ntaps) PrevCladStrain
        WRITE (ntaps) StartofStepH2Con
        WRITE (ntaps) StartofStepPickupH2Con
        WRITE (ntaps) EndofStepPickupH2Con
        WRITE (ntaps) OldHealedCrackRadius
        WRITE (ntaps) Power
        WRITE (ntaps) gpthe
!        WRITE (ntaps) gpth
        WRITE (ntaps) gpthpg
        WRITE (ntaps) qc
        WRITE (ntaps) buin
        WRITE (ntaps) dci
        WRITE (ntaps) dco
        WRITE (ntaps) de
        WRITE (ntaps) deltaz
        WRITE (ntaps) dp
        WRITE (ntaps) flux
        WRITE (ntaps) qend
        WRITE (ntaps) crudmult
        WRITE (ntaps) crdtt
        WRITE (ntaps) ifbarel
        WRITE (ntaps) NodalMoles
        WRITE (ntaps) NodalGMLES
        WRITE (ntaps) rlcstrn
        WRITE (ntaps) rlcstrnold
        WRITE (ntaps) colddef
        WRITE (ntaps) colddec
        WRITE (ntaps) qnode
        WRITE (ntaps) heprod
        WRITE (ntaps) he
        WRITE (ntaps) heold
        WRITE (ntaps) boron10
        WRITE (ntaps) epsav
!        WRITE (ntaps) wimsburnup
        WRITE (ntaps) oldwimsburnup
        ! Replaces the common block primarycreep
        WRITE (ntaps) stold
        WRITE (ntaps) stnew
        WRITE (ntaps) deltimeold
        WRITE (ntaps) deltimenew
        WRITE (ntaps) sagold
        WRITE (ntaps) sagnew
        WRITE (ntaps) delsagold
        WRITE (ntaps) delsagnew
        WRITE (ntaps) delst
        ! ***These arrays are dimensioned (na, 2)***
!        WRITE (ntaps) angr
!        WRITE (ntaps) fgmgp
        WRITE (ntaps) fmgr
        WRITE (ntaps) hemgp
        WRITE (ntaps) hmgr
        WRITE (ntaps) ang
        ! ***These arrays are dimensioned (na, 3)***
        WRITE (ntaps) epp
        WRITE (ntaps) eppp
        WRITE (ntaps) eppsv
        WRITE (ntaps) eps
        WRITE (ntaps) feps
        WRITE (ntaps) reps
        WRITE (ntaps) repsv
        WRITE (ntaps) repsp
        WRITE (ntaps) rfeps
        WRITE (ntaps) rfepp
        WRITE (ntaps) rfpsv
        WRITE (ntaps) sig
        WRITE (ntaps) epp1
        WRITE (ntaps) sig1
        WRITE (ntaps) eps1
        ! ***Time-Dependent***
        WRITE (ntaps) acmfg(1)                           !
        WRITE (ntaps) acmhe(1)                           !
        WRITE (ntaps) acmh2(1)                           !
        WRITE (ntaps) acmn2(1)                           !
        WRITE (ntaps) amgpt(1)                           !
        WRITE (ntaps) gasmo(1)                           !
        WRITE (ntaps) hmgpt(1)                           !
        WRITE (ntaps) tafa(1)                            !
        WRITE (ntaps) taga(1)                            !
        WRITE (ntaps) tsfa(1)                            !
        WRITE (ntaps) taca(1)                            !
        WRITE (ntaps) pit(1)                             !
        WRITE (ntaps) buavearray(1)
        WRITE (ntaps) voidvolarray(1)
        WRITE (ntaps) he_ifba(1)
        ! *** Radial-Dependent Arrays, dimensioned (nr)***
        WRITE (ntaps) tfuelr
        WRITE (ntaps) tfuelr2
        WRITE (ntaps) tfring
        WRITE (ntaps) rrapow
        ! *** Multi-Dependent in a single dimensionalized array (timesteps*radialnodes*axialnodes)***
        WRITE (ntaps) ansd                                ! Diffusion PARAMETER, used in Subroutine ans54
        !
        ! 2-D Arrays
        !
        WRITE (ntaps) porosold                      ! porosity left at beginning of power step (m/m)        
        WRITE (ntaps) porosnew                      ! porosity left after power step (m/m)
        WRITE (ntaps) rrev                          ! fuel mesh point radii array (in)
        WRITE (ntaps) crad                          ! cold fuel ring radius (m)
        WRITE (ntaps) hrad                          ! hot fuel ring radius (m)
        WRITE (ntaps) rapow                         ! Normalized radial power profile array
        WRITE (ntaps) dpw
        WRITE (ntaps) dpwpp
        WRITE (ntaps) dpw2
        WRITE (ntaps) dpwpp2
        WRITE (ntaps) densf
        WRITE (ntaps) densp
        WRITE (ntaps) brnup3
        ! ********************
        ! *** Comde Module ***
        ! ********************
        WRITE (ntaps) lschni
        WRITE (ntaps) brnup1
        WRITE (ntaps) ntot
        WRITE (ntaps) conU235
        WRITE (ntaps) conU238
        WRITE (ntaps) conPu239
        WRITE (ntaps) conPu240
        WRITE (ntaps) conPu241
        WRITE (ntaps) conPu242
        WRITE (ntaps) coU235av
        WRITE (ntaps) coU238av
        WRITE (ntaps) coPu239av
        WRITE (ntaps) coPu240av
        WRITE (ntaps) coPu241av
        WRITE (ntaps) coPu242av
        WRITE (ntaps) enriU235
        WRITE (ntaps) enriU238
        WRITE (ntaps) enriPu239
        WRITE (ntaps) enriPu240
        WRITE (ntaps) enriPu241
        WRITE (ntaps) enriPu242
        WRITE (ntaps) por000
        ! ********************************
        ! *** FissionGasRelease Module ***
        ! ********************************
        WRITE (ntaps) FGRData%ANS54%pfave
        WRITE (ntaps) FGRData%ansr
        WRITE (ntaps) FGRData%gp
        WRITE (ntaps) FGRData%gpold
        WRITE (ntaps) FGRData%rls
        WRITE (ntaps) FGRData%rlsold
        WRITE (ntaps) FGRData%brn
        WRITE (ntaps) FGRData%brnold
        WRITE (ntaps) FGRData%grnold
        WRITE (ntaps) FGRData%frdenold
        WRITE (ntaps) FGRData%gbtot
        WRITE (ntaps) FGRData%tempkold
        WRITE (ntaps) FGRData%ANS54%pf
        WRITE (ntaps) FGRData%ANS54%decay
        WRITE (ntaps) FGRData%ANS54%half
        WRITE (ntaps) FGRData%FRAPFGR%openp
        WRITE (ntaps) FGRData%gb
        WRITE (ntaps) FGRData%grs
        WRITE (ntaps) FGRData%gg
        WRITE (ntaps) FGRData%gbold
        WRITE (ntaps) FGRData%ggold
        WRITE (ntaps) FGRData%grsold
        WRITE (ntaps) FGRData%g1
        WRITE (ntaps) FGRData%g2
        WRITE (ntaps) FGRData%g3
        WRITE (ntaps) FGRData%g4
        WRITE (ntaps) FGRData%g1old
        WRITE (ntaps) FGRData%g2old
        WRITE (ntaps) FGRData%g3old
        WRITE (ntaps) FGRData%g4old
        IF (it == im) WRITE (ntaps) 'END FRAPCON'
    END IF
    !
100 FORMAT (/,5x,' FRAPCON restart information will be written.',/)
110 FORMAT (/,5x,' restart information has been written at power step: ',i5,/,34x,'with a problem time of: ',E11.4,' s')
    !
    END SUBROUTINE WriteRestart
    !
    !
    !
    SUBROUTINE ReadRestart
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine reads the restart file
    ! *** NOTE: This subroutine is not a working part of FRAPCON-4.0 release ***
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/8/2014
    !
    INTEGER(ipk) :: i, im2, na2, ngasr2, nr2, nce2, itrest2, it2
    REAL(r8k) :: RestartTime2
    LOGICAL :: CorrectTimestep = .FALSE.
    CHARACTER(LEN=70) :: Description
    !
    ! ************************
    ! *** Variables Module ***
    ! ************************
    READ (ntapi) Description
    IF (Description == 'FRAPCON') THEN
    READ (ntapi) im2, na2, ngasr2, nr2, nce2
    ! Ensure there are no errors between the restart file and the file being modeled
    IF (na2 /= na .OR. ngasr /= ngasr2 .OR. nr /= nr2 .OR. nce /= nce2) THEN
        WRITE (0, 100)
        WRITE (ounit, 100)
100     FORMAT(' Error reading restart file. The number of nodes (na, ngasr, nr or nce) is not consistent.')
        STOP
    END IF

    READ (ntapi) cvv, vfrcpl
    READ (ntapi) icm, imox, iplant, zr2vintage, ngasmod
    READ (ntapi) ncreep
    READ (ntapi) nvoid
    READ (ntapi) gapmin, gaph, gapt
    READ (ntapi) airin
    READ (ntapi) angi
    READ (ntapi) an2in, argin, avflux
    READ (ntapi) avgqi
    READ (ntapi) cfv, cfva
    READ (ntapi) cpv
    READ (ntapi) ctemp, dcobol, delbp, delbu
    READ (ntapi) deltfc, deltjl
    READ (ntapi) dhfll
    READ (ntapi) fgin
    READ (ntapi) fpor1
    READ (ntapi) gases
    READ (ntapi) hein, hfll
    READ (ntapi) hfper
    READ (ntapi) hpl
    READ (ntapi) hporv, hpv, hva
    READ (ntapi) kryin, nu
    READ (ntapi) rhofuel
    READ (ntapi) rp, rstran, rtran
    READ (ntapi) t, thvv
    READ (ntapi) tfgfr, thefr, th2ofr, tn2fr
    READ (ntapi) fnck
    READ (ntapi) fncn, cwkf, cwnf, coldwk, frcoef
    READ (ntapi) afal, afdn, afgr
    READ (ntapi) amfair, amffg
    READ (ntapi) catexf, chorg, cldwks, cpl, crephr
    READ (ntapi) crdt, crdtr, dishsd
    READ (ntapi) fa, fgpav, fotmtl
    READ (ntapi) chmfrh, chmfrw, hdish, hplt
    READ (ntapi) pitch, ppmh2o, ppmn2, rdish, roughc
    READ (ntapi) roughf, sgapf, tcc
    READ (ntapi) rsntr, tsint, grnsize
    READ (ntapi) b10, zrb2thick, zrb2den, prvden, ifba
    READ (ntapi) excesh2, frden
    !
    ! **************************************
    ! ****** Time Dependent Variables ******
    ! **************************************
    !
    ! ************************
    ! *** Variables Module ***
    ! ************************
    ! READ the time-step (it-1) and the time in seconds
    DO WHILE (.NOT. CorrectTimestep)
        READ (ntapi) it2, RestartTime2
        it = it2
        READ (ntapi) bu, buold
        READ (ntapi) FastFluxd
        READ (ntapi) FastFluenced
        ! ***Axial-Dependent Arrays, dimensioned (na)***
        READ (ntapi) IgapGapIndex
        READ (ntapi) IgapIndexOld
        READ (ntapi) IgapIndexPrevOld
!        READ (ntapi) dpwxrate
        READ (ntapi) axialnode                   ! Axial node dimensions
        READ (ntapi) PelletRad
        READ (ntapi) sigeff
        READ (ntapi) FuelSurfDispl
        READ (ntapi) CladInSurDisp
        READ (ntapi) CladInPermDef
        READ (ntapi) sigy
        READ (ntapi) AxialNodLength
        READ (ntapi) CladEffPlasStrain
        READ (ntapi) RinterfacPress
        READ (ntapi) FuelCladGap
        READ (ntapi) GapPress
        READ (ntapi) CoolantPress
        READ (ntapi) PlastStrnep1
        READ (ntapi) OldCladStrn
        READ (ntapi) OldFuelStrn
        READ (ntapi) OldGapPress
        READ (ntapi) OldCoolPress
        READ (ntapi) OldFuelDispl
        READ (ntapi) OldCladAvTemp
        READ (ntapi) CreepStrain
        READ (ntapi) CreepStrain1
        READ (ntapi) CladDiamHot
        READ (ntapi) PrevCladEffPlasStrn
        READ (ntapi) PrevFuelStrain
        READ (ntapi) HotNodLength
!        READ (ntapi) AxMaxFuelDispl
        READ (ntapi) PermFuelDispl
        READ (ntapi) BOSNodeburnup
        READ (ntapi) EOSNodeburnup
        READ (ntapi) StepNodeburnup
        READ (ntapi) BOSZrO2Thk
        READ (ntapi) EOSZrO2Thk
        READ (ntapi) ZrO2ThkNoAd
!        READ (ntapi) AnnulusVolume
        READ (ntapi) PrevOldCoolPress
        READ (ntapi) PrevOldGapPress
        READ (ntapi) PrevOldCladAvTemp
        READ (ntapi) PrevOldFuelDispl
        READ (ntapi) PrevCreepStrain
        READ (ntapi) CladIrradGrowStrn
        READ (ntapi) UniformAxNodStrn
        READ (ntapi) CladH2Concen
        READ (ntapi) ExcessH2Concen
        READ (ntapi) PrevCladStrain
        READ (ntapi) StartofStepH2Con
        READ (ntapi) StartofStepPickupH2Con
        READ (ntapi) EndofStepPickupH2Con
        READ (ntapi) OldHealedCrackRadius
        READ (ntapi) Power
        READ (ntapi) gpthe
!        READ (ntapi) gpth
        READ (ntapi) gpthpg
        READ (ntapi) qc
        READ (ntapi) buin
        READ (ntapi) dci
        READ (ntapi) dco
        READ (ntapi) de
        READ (ntapi) deltaz
        READ (ntapi) dp
        READ (ntapi) flux
        READ (ntapi) qend
        READ (ntapi) crudmult
        READ (ntapi) crdtt
        READ (ntapi) ifbarel
        READ (ntapi) NodalMoles
        READ (ntapi) NodalGMLES
        READ (ntapi) rlcstrn
        READ (ntapi) rlcstrnold
        READ (ntapi) colddef
        READ (ntapi) colddec
        READ (ntapi) qnode
        READ (ntapi) heprod
        READ (ntapi) he
        READ (ntapi) heold
        READ (ntapi) boron10
        READ (ntapi) epsav
!        READ (ntapi) wimsburnup
        READ (ntapi) oldwimsburnup
        ! Replaces the common block primarycreep
        READ (ntapi) stold
        READ (ntapi) stnew
        READ (ntapi) deltimeold
        READ (ntapi) deltimenew
        READ (ntapi) sagold
        READ (ntapi) sagnew
        READ (ntapi) delsagold
        READ (ntapi) delsagnew
        READ (ntapi) delst
        ! ***These arrays are dimensioned (na, 2)***
!        READ (ntapi) angr
!        READ (ntapi) fgmgp
        READ (ntapi) fmgr
        READ (ntapi) hemgp
        READ (ntapi) hmgr
        READ (ntapi) ang
        ! ***These arrays are dimensioned (na, 3)***
        READ (ntapi) epp
        READ (ntapi) eppp
        READ (ntapi) eppsv
        READ (ntapi) eps
        READ (ntapi) feps
        READ (ntapi) reps
        READ (ntapi) repsv
        READ (ntapi) repsp
        READ (ntapi) rfeps
        READ (ntapi) rfepp
        READ (ntapi) rfpsv
        READ (ntapi) sig
        READ (ntapi) epp1
        READ (ntapi) sig1
        READ (ntapi) eps1
        ! ***Time-Dependent***
        READ (ntapi) acmfg(it2)                           !
        READ (ntapi) acmhe(it2)                           !
        READ (ntapi) acmh2(it2)                           !
        READ (ntapi) acmn2(it2)                           !
        READ (ntapi) amgpt(it2)                           !
        READ (ntapi) gasmo(it2)                           !
        READ (ntapi) hmgpt(it2)                           !
        READ (ntapi) tafa(it2)                                !
        READ (ntapi) taga(it2)                                !
        READ (ntapi) tsfa(it2)                                !
        READ (ntapi) taca(it2)                                !
        READ (ntapi) pit(it2)                                 !
        READ (ntapi) buavearray(it2)
        READ (ntapi) voidvolarray(it2)
        READ (ntapi) he_ifba(it2)
        ! *** Radial-Dependent Arrays, dimensioned (nr)***
        READ (ntapi) tfuelr
        READ (ntapi) tfuelr2
        READ (ntapi) tfring
        READ (ntapi) rrapow
        ! *** Multi-Dependent in a single dimensionalized array (timesteps*radialnodes*axialnodes)***
        READ (ntapi) ansd                                ! Diffusion PARAMETER, used in Subroutine ans54
        !
        ! 2-D Arrays
        !
        READ (ntapi) porosold                  ! porosity left at beginning of power step (m/m)        
        READ (ntapi) porosnew                  ! porosity left after power step (m/m)
        READ (ntapi) rrev                          ! fuel mesh point radii array (in)
        READ (ntapi) crad                          ! cold fuel ring radius (m)
        READ (ntapi) hrad                          ! hot fuel ring radius (m)
        READ (ntapi) rapow                         ! Normalized radial power profile array
        READ (ntapi) dpw
        READ (ntapi) dpwpp
        READ (ntapi) dpw2
        READ (ntapi) dpwpp2
        READ (ntapi) densf
        READ (ntapi) densp
        READ (ntapi) brnup3
        ! ********************
        ! *** Comde Module ***
        ! ********************
        READ (ntapi) lschni
        READ (ntapi) brnup1
        READ (ntapi) ntot
        READ (ntapi) conU235
        READ (ntapi) conU238
        READ (ntapi) conPu239
        READ (ntapi) conPu240
        READ (ntapi) conPu241
        READ (ntapi) conPu242
        READ (ntapi) coU235av
        READ (ntapi) coU238av
        READ (ntapi) coPu239av
        READ (ntapi) coPu240av
        READ (ntapi) coPu241av
        READ (ntapi) coPu242av
        READ (ntapi) enriU235
        READ (ntapi) enriU238
        READ (ntapi) enriPu239
        READ (ntapi) enriPu240
        READ (ntapi) enriPu241
        READ (ntapi) enriPu242
        READ (ntapi) por000
        ! ********************************
        ! *** FissionGasRelease Module ***
        ! ********************************
        READ (ntapi) FGRData%ANS54%pfave
        READ (ntapi) FGRData%ansr
        READ (ntapi) FGRData%gp
        READ (ntapi) FGRData%gpold
        READ (ntapi) FGRData%rls
        READ (ntapi) FGRData%rlsold
        READ (ntapi) FGRData%brn
        READ (ntapi) FGRData%brnold
        READ (ntapi) FGRData%grnold
        READ (ntapi) FGRData%frdenold
        READ (ntapi) FGRData%gbtot
        READ (ntapi) FGRData%tempkold
        READ (ntapi) FGRData%ANS54%pf
        READ (ntapi) FGRData%ANS54%decay
        READ (ntapi) FGRData%ANS54%half
        READ (ntapi) FGRData%FRAPFGR%openp
        READ (ntapi) FGRData%gb
        READ (ntapi) FGRData%grs
        READ (ntapi) FGRData%gg
        READ (ntapi) FGRData%gbold
        READ (ntapi) FGRData%ggold
        READ (ntapi) FGRData%grsold
        READ (ntapi) FGRData%g1
        READ (ntapi) FGRData%g2
        READ (ntapi) FGRData%g3
        READ (ntapi) FGRData%g4
        READ (ntapi) FGRData%g1old
        READ (ntapi) FGRData%g2old
        READ (ntapi) FGRData%g3old
        READ (ntapi) FGRData%g4old
        ! Check to see if this is the correct timestep to use for restart
        IF (nread < 0) THEN
            IF (RestartTime2 == RestartTime) CorrectTimestep = .TRUE.
        ELSE
            IF (nread == it2) THEN
                CorrectTimestep = .TRUE.
                RestartTime = RestartTime2
            END IF
        END IF
    END DO
    !
    ! Determine which timestep to start the calculation from by ensuring that the calculation is not repeated
    ! from a smaller timestep
    !
    i = 1
    DO WHILE (RestartTime > ProblemTime(i))
        i = i + 1
        it = i
    END DO
    ! Ensure that the problem time for the previous timestep is the time at which the restart data was written
    ProblemTime(it) = RestartTime
    ! The following were taken from frpcon and placed in this to clean up the code
    !CALL store2
    itt = 2
    nab = 2
    END IF
    !
175 FORMAT (/2x,'This run will start from a FRAPCON restart file. ',e12.5/)
180 FORMAT (/5x,'stime= ',e12.5,2x,'n= ',i3,2x,'ProblemTime= ',e12.5/)
190 FORMAT (5x,'Frapcon4 will be restarted from ProblemTime = ',e12.5)
200 FORMAT (//5x,'***** restart was attempted at a time greater than the specified time '//)
    !
    END SUBROUTINE ReadRestart
    !
    ! ***********************
    !  FRAPTRAN Restart File
    ! ***********************
    !
    SUBROUTINE restfs
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : na, nr, im, ngasr, gadoln, colddef, colddec, ProblemTime, rlcstrn, fgmgp, epp, &
      &                   gasmo, rapow, ifixedcoolp, CoolantPressure, crad, gases, press, it, nt, rci, rp, dcoBOL
    USE FissionGas, ONLY : FGRData
    IMPLICIT NONE
    !> @brief
    !> Subroutine restfs writes a restart file that can be Read by FRAPTRAN.
    !> @author
    !> modified by Ian Porter
    !> @date
    !> February 2014
    !
    ! Input
    !
    ! EOSNodeburnup         - burnup (MWd/mtU)
    ! ctmax                 - maximum cladding temperature reached during rod life (F)
    ! FrapTranRestWorkArray - work array
    ! CladEffPlasStrain     - effective plastic strain
    ! epp                   - hoop, axial, & radial plastic strains
    ! gases                 - mole fraction of rod gases
    ! gasmol                - gm-moles of gas in the rod
    ! na                    - number of axial nodes
    ! nt                    - (na - 1)
    ! nr                    - number of radial nodes
    ! IgapIndexOld          - old values of gap closure indicies
    ! OldCoolPress          - old values of coolant pressure (psi)
    ! OldGapPress           - old values of gap pressure (psi)
    ! OldCladAvTemp         - old values of cladding average temperature (F)
    ! OldFuelDispl          - old values of fuel outside surface displacement (in)
    ! OldCladStrn           - old values of cladding strain
    ! OldFuelStrn           - old values of fuel strain
    ! FuelPorosity          - volume fraction of fuel porosity
    ! RinterfacPress        - interfacial pressure (psi)
    ! press                 - rod gas gap pressure (psia)
    ! crad                  - cold radius to each radial node (in)
    ! rci                   - radius of the cladding inside surface (in)
    ! dcoBOL                - diameter of the cladding outside surface as-fabricated (in)
    ! reps                  - residual cladding strains
    ! rfeps                 - residual fuel strains
    ! rlcstrn               - relocation strain from gtreloc (1/2 is used to calculate ureloc)
    ! rp                    - pellet as-fabricated radius (inches)
    ! CladInSurfTemp        - cladding  inside surface temperature (F)
    ! CladOutSurfTemp       - cladding outside surface temperature (F)
    ! ProblemTime           - time to End of step (sec)
    ! tmpfuel               - fuel temperature distribution (F)
    ! PermFuelDispl         - permanent fuel displacement (ft)
    ! EOSZrO2Thk            - zircaloy-oxide thickness (ft)
    ! 
    ! Internal
    !
    ! rco                   - As-fabricated outer cladding radius
    !
    INTEGER(ipk) :: j, k, nap1, nrp1, nrp2, i, l, im2, im2na, im2na2, ii, dum
    REAL(r8k) :: rco
    LOGICAL :: ndebug
    REAL(r8k), DIMENSION(na) :: CoolantPress
    REAL(r8k), DIMENSION(na-1) :: efstrn, ureloc
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FrapTranRestWorkArray
    ! Force updated_restart to be FALSE as it is still a work in progress
    updated_restart = .FALSE.
    !
    dum = MAX(na+2,nr+2)
    ALLOCATE (FrapTranRestWorkArray(1:dum))
    FrapTranRestWorkArray = 0.0_r8k
    rco = dcoBOL / 2.0_r8k
    nap1 = nt + 1
    nrp1 = nr + 1
    nrp2 = nr + 2
    ndebug = .FALSE.
    ! If updated, write -1 so that FRAPTRAN knows it's an updated restart file (a restart file created from after FRAPCON-3.5)
    IF (updated_restart) WRITE (ftunit,10) -1.0_r8k 
    ! Write the end of step time (sec)
    WRITE (ftunit,10) ProblemTime(it)
    ! Write the number of frapcon axial nodes
    WRITE (ftunit,20) nt
    IF (ndebug) WRITE (ounit,210) nt
    IF (updated_restart) THEN
        ! Write the plant type
        WRITE (ftunit,20) iplant
        ! Write the moderator heating value (fraction)
        WRITE (ftunit,10) modheat
        ! Write the coolant pressure (psi)
        DO k = 2, nap1
            IF (ifixedcoolp == 1) THEN ! User supplied axially varying coolant pressure
                CoolantPress(k) = CoolantPressure(it,k)
            ELSE ! The same value is repeated because it is assumed constant at all axial nodes in FRAPCON.
                CoolantPress(k) = p2(it)
            END IF
        END DO
        WRITE (ftunit,10) (CoolantPress(k),k=2,nap1)
        ! Write plenum temperature (C)
        WRITE (ftunit,10) tplens
        ! Write # moles in all axial nodes + plenum
        WRITE (ftunit,10) (NodalGMLES(k),k=1,nap1)
        ! Write the gap thickness at each axial node
        WRITE (ftunit,10) (FuelCladGap(k),k=2,nap1) !Need to modify to always print mils
        ! Write the gap temperature at each axial node. Gap temperature is assumed as average temperature between 
        ! fuel pellet surface & cladding inner surface.
        WRITE (ftunit,10) (GapAveTemp(k-1),k=2,nap1) !Need to modify to always print Celcius
        ! Write the pellet outer surface radius at each axial node
        WRITE (ftunit,10) (PelletRad(k-1),k=2,nap1)
    END IF
    ! Write the zircaloy-oxide thickness of the cladding outside surface in inches
    DO k = 2, nap1
        FrapTranRestWorkArray(k) = EOSZrO2Thk(k-1) * fttoin
    END DO
    WRITE (ftunit,10) (FrapTranRestWorkArray(k),k=2,nap1)
    IF (ndebug) WRITE (ounit,220)(FrapTranRestWorkArray(k),k=2,nap1)
    ! Write the total H2 concentration
    WRITE (ftunit,10) (CladH2Concen(k-1),k=2,nap1)
    IF (ndebug) WRITE (ounit,225) (CladH2Concen(k-1),k=2,nap1)
    ! Write the cladding peak temperature during operation life (f)
    WRITE (ftunit,10) (ctmax(k),k=2,nap1)
    IF (ndebug) WRITE (ounit,260) (ctmax(k),k=2,nap1)
    ! Write the open porosity of each axial node (fraction of pellet)
    WRITE (ftunit,10) (FuelPorosity(k),k=1,nt)
    IF (ndebug) WRITE (ounit,280) (FuelPorosity(k),k=1,nt)
    ! Write the fuel burnup (mw-sec/kg)
    DO k = 2, nap1
        FrapTranRestWorkArray(k) = EOSNodeburnup(k-1) * 86.4_r8k
    END DO
    WRITE (ftunit,10) (FrapTranRestWorkArray(k),k=2,nap1)
    IF (ndebug) WRITE (ounit,270)(FrapTranRestWorkArray(k),k=2,nap1)
    ! Write the cladding fast neutron fluence
    WRITE (ftunit,10) (FastFluence(k), k=1,nt)
    IF (ndebug) WRITE (ounit,285) (FastFluence(k), k=1,nt)
    ! Write radial nodes at fuel surface, cladding inside surface, cladding outside surface
    WRITE (ftunit,20) nr,nrp1,nrp2
    IF (ndebug) WRITE (ounit,290) nr, nrp1, nrp2
    ! Write the gm-moles of gas in the rod
    WRITE (ftunit,10) gasmo(it-1)
    IF (ndebug) WRITE (ounit,340) gasmo(it-1)
    ! Write the mole fraction of rod gases(i)
    ! i = 1  helium
    ! i = 2  argon
    ! i = 3  krypton
    ! i = 4  xenon
    ! i = 5  hydrogen
    ! i = 6  nitrogen
    ! i = 7  air
    ! i = 8  water vapor
    !
    ! Note: agreed by IP, PR & KG that the FRAPTRAN restart file can be updated before FRAPTRAN-2.0 is completed
    WRITE (ftunit,10) (gases(i),i=1,ngases)
    IF (ndebug) WRITE (ounit,350) (gases(i),i=1,ngases)
    !
    ! *** NOTE: Why is the following only written for one axial node? ***
    ! *** This should loop over all (j-1) nodes due to differences ***
    ! *** in radial locations at each axial node. ***
    !
    ! Write the node locations from fuel center to surface in ft
    DO i = 1, nr
        FrapTranRestWorkArray(i) = crad(nrp1-i,2) * intoft
    END DO
    FrapTranRestWorkArray(nrp1) = rci * intoft
    FrapTranRestWorkArray(nrp2) = rco * intoft
    WRITE (ftunit,10) (FrapTranRestWorkArray(i),i=1,nrp2)
    IF (ndebug) WRITE (ounit,301) (FrapTranRestWorkArray(i),i=1,nrp2)
    !
    ! *** END NOTE ***
    !
    ! fracas-1 converted to fracas-1 link
    ! Write the cladding plastic strains including creep
    ! l=1   hoop
    ! l=2   axial
    ! l=3   radial
    DO l = 1, 3
        DO k = 1, nt
            im2 = k - 1
            im2na = im2 + na
            im2na2 = im2na + na
            IF (l == 1) i = im2
            IF (l == 2) i = im2na
            IF (l == 3) i = im2na2
            WRITE (ftunit,10) epp(k,l)
            IF (ndebug) WRITE (ounit,240) l, epp(k,l)
        END DO
    END DO
    ! Write the cladding effective plastic strains
    DO k = 1, nt
        efstrn(k) = ABS(epp(k,1) * 2.0_r8k / SQRT(3.0_r8k))
        WRITE (ftunit,10) efstrn(k)
        IF (ndebug) WRITE (ounit,250) efstrn(k)
    END DO
    ! Write the radial temperature distribution (F)
    DO k = 1, nt
        DO j = 1, nr
            FrapTranRestWorkArray(j) = tmpfuel(nr-(j-1),k)
        END DO
        FrapTranRestWorkArray(nr+1) = CladInSurfTemp(k)
        FrapTranRestWorkArray(nr+2) = CladOutSurfTemp(k)
        WRITE (ftunit,10) (FrapTranRestWorkArray(j),j=1,nrp2)
        IF (ndebug) WRITE (ounit,310)(FrapTranRestWorkArray(j),j=1,nrp2)
    END DO
    ! Write the net permanent fuel displacement due to swelling and densification (colddef),
    ! cladding displacement due to creep/plastic (colddec) and fuel pellet permanent relocation (ureloc) (INCHES)
    WRITE (ftunit,10) (colddef(k),k=1,nt)
    IF (updated_restart) THEN
        WRITE (ftunit,10) (fueldentot(k),k=1,nt)    ! Fuel densification, inches
        WRITE (ftunit,10) (cladinpermdef(k),k=1,nt) ! Cladding inner surface permanent deformation
    ELSE
        WRITE (ftunit,10) (colddec(k),k=1,nt)       ! Cladding outer surface permanent deformation
    END IF
    DO k = 1, nt
        ureloc(k) = rlcstrn(k) * rp / 2.0_r8k
    END DO
    WRITE (ftunit,10) (ureloc(k), k=1,nt)
    IF (ndebug) THEN
        WRITE (ounit,230)(colddef(k),k=1,nt)
        IF (updated_restart) THEN
            WRITE (ounit,231)(cladinpermdef(k),k=1,nt)
        ELSE
            WRITE (ounit,231)(colddec(k),k=1,nt)
        END IF
        WRITE (ounit,232)(ureloc(k),k=1,nt)
    END IF
    ! Write gadolinia content
    IF (updated_restart) THEN ! Gad content from each node
        WRITE (ftunit,10) (gadoln(j),j=1,nt)
    ELSE ! Gad content from bottom node
        WRITE (ftunit,10) (gadoln(1))
    END IF
    IF (ndebug) WRITE (ounit,400) (gadoln(j),j=1,nt)
    ! Write burnup at each radial boundary, each axial node numbered from the inside outward
    DO k = 1, nt
        DO i = 1, nr
            WRITE (ftunit,10) brnup3(k,i,1)
            IF (ndebug) WRITE (ounit,401) k, i, brnup3(k,i,1)
        END DO
    END DO
    ! Write radial form factor at each radial boundary, each axial node
    DO k = 1, nt
        DO ii = 1, nr
            i = nr + 1 - ii
            WRITE (ftunit,10) rapow(i,k) !formf(k,i)
            IF (ndebug) WRITE (ounit,402) k,i,formf(k,i)
        END DO
    END DO
    !
    WRITE (ounit,200) ProblemTime(it)
    !
    WRITE (ftunit,*) ngasmod
    IF (ngasmod == 3) THEN
        WRITE (ftunit,*) ngasr
        WRITE (ftunit,241) (FGRData%ansr(k),k=1,ngasr)
        DO j = 1, nt
            WRITE (ftunit,241) (gasavail1(k,j),k=1,ngasr)
            WRITE (ftunit,241) (gasavail2(k,j),k=1,ngasr)
            WRITE (ftunit,242) fgmgp(j,2)
        END DO
241     FORMAT(50(2x,e11.4))
242     FORMAT(e11.4)
    END IF
    !
10  FORMAT(2x,30(e11.4,2x))
20  FORMAT(2x,30(i5,2x))
200 FORMAT (//,5x,' a Frap-T restart has been written at ProblemTime =  ',e12.5,' (sec)',//)
210 FORMAT (' number of axial nodes is          ',i3)
220 FORMAT ('  zircaloy-oxide thickness (in)    ',/,11(1x,e11.4))
225 FORMAT ('  total H2 concentration (ppm)     ',/,11(1x,e11.4))
230 FORMAT (' fuel permanent surface disp (in)  ',/,11(1x,e11.4))
231 FORMAT (' cladding ID surface dislpl. (in)  ',/,11(1x,e11.4))
232 FORMAT (' fuel relocation displacement(in)  ',/,11(1x,e11.4))
240 FORMAT (' direction, cladding epp',/,2x,i5,(1x,e11.4))
250 FORMAT (' cladding CladEffPlasStrains       ',(1x,e11.4))
260 FORMAT (' maximum cladding average temp (K) ',/,11(1x,e11.4))
270 FORMAT (' burnup (mws/kg)                   ',/,11(1x,e11.4))
280 FORMAT (' open porosity fraction            ',/,11(1x,e11.4))
285 FORMAT (' cladding fast neutron fluence n/m^2',/,11(1x,e11.4))
290 FORMAT ('  nr , nr+1 , nr+2                 ',/,3i5)
301 FORMAT ('  radius to each node (ft)         ',/,11(1x,e11.4))
305 FORMAT ('  radius to each node (in)         ',/,11(1x,e11.4))
310 FORMAT ('  radial temp. dist (F)            ',/,11(1x,e11.4))
340 FORMAT ('  gm-moles of gas                  ',/,11(1x,e11.4))
350 FORMAT ('  mole fractions of gas            ',/,11(1x,e11.4))
400 FORMAT (' gadolinia content', 2x,e11.4)
401 FORMAT (' axial node', 1x, i5, 1x, 'rad.bound.', 1x, i5, 1x, 'burnup', 1x, e11.4, 1x, 'MWd/MTU')
402 FORMAT (' axial node', 1x, i5, 1x, 'rad.bound.', 1x, i5, 1x, 'form factor', 1x, e11.4)
    !
    DEALLOCATE(FrapTranRestWorkArray)
    !
    END SUBROUTINE restfs
    !
END MODULE Restart

