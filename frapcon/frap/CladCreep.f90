MODULE CladCreep
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutine that calls the creep and 
    !> deformation models in fcmi
    !> Subroutines include ccreep
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    SUBROUTINE ccreep
    USE Kinds
    USE Conversions
    USE Variables
    USE FEModel
    USE Deformation
    IMPLICIT NONE
    !
    INTEGER(ipk) :: i, ii, ij, ik, ihere, nrelax, kflag, n1, n22, n3, n4, jjj, crep, ncrep
    REAL(r8k) :: deltime, x11, x22, atime, dlrod2, dlrel2, ffluxx
    REAL(r8k), DIMENSION(na) :: cladstrn, fuelstrn
    !
    DO i = 1, (na - 1)
        OldCladAvTemp(i) = PrevOldCladAvTemp(i)
        OldGapPress(i) = PrevOldGapPress(i)
        OldCoolPress(i) = PrevOldCoolPress(i)
        OldFuelDispl(i) = PrevOldFuelDispl(i)
        IgapIndexOld(i) = IgapIndexPrevOld(i)
        OldCladStrn(i) = PrevCladStrain(i)
        OldFuelStrn(i) = PrevFuelStrain(i)
        CladEffPlasStrain(i) = epsav(i)
        DO ii = 1, 3
            epp(i,ii) = eppsv(i,ii)
            reps(i,ii) = repsv(i,ii)
            rfeps(i,ii) = rfpsv(i,ii)
        END DO
    END DO
    ! ********************************************************************
    !                creep calculations start here
    ! ********************************************************************
    ihere = 0
    nrelax = 1
    x11 = delh
    x22 = crephr
    atime = ProblemTime(it-1) * sectohr
    deltime = x22
    crep = INT(x11 / x22)
    !
    SELECT CASE (mechan)
    CASE (1)
        CALL mech (1, 1, feps, eps, epp, CladEffPlasStrain, CreepStrain, CladStrn, FuelStrn, &
          &        OldGapPress, OldCoolPress, OldFuelDispl, CladAveTemp, OldCladAvTemp, IgapIndexOld, &
          &        sig, sigeff, reps, rfeps, eppsv, repsv, rfpsv)
        !
        DO ij = 1, (na - 1)
            PrevCladEffPlasStrn(ij) = CladEffPlasStrain(ij)
            DO ik = 1, 3
                eppp(ij,ik) = epp(ij,ik)
                repsp(ij,ik) = reps(ij,ik)
                rfepp(ij,ik) = rfeps(ij,ik)
            END DO
        END DO
        RETURN
    CASE (2)
        ncrep = crep
        CreepLoop: DO
            IF (ncrep >= 1) THEN
                DO i = 1, ncrep
                    DO ij = 1, (na - 1)
                        PrevCreepStrain(ij) = CreepStrain(ij)
                        PrevOldCladAvTemp(ij) = OldCladAvTemp(ij)
                        PrevOldGapPress(ij) = OldGapPress(ij)
                        PrevOldCoolPress(ij) = OldCoolPress(ij)
                        PrevOldFuelDispl(ij) = OldFuelDispl(ij)
                        IgapIndexPrevOld(ij) = IgapIndexOld(ij)
                        PrevCladStrain(ij) = OldCladStrn(ij)
                        PrevFuelStrain(ij) = OldFuelStrn(ij)
                        PrevCladEffPlasStrn(ij) = CladEffPlasStrain(ij)
                        DO ik = 1, 3
                            eppp(ij,ik) = epp(ij,ik)
                            repsp(ij,ik) = reps(ij,ik)
                            rfepp(ij,ik) = rfeps(ij,ik)
                        END DO
                    END DO
                    ! The following call to fcmi results in cladding creep mechanical calculations using the Fracas-1 subcode
                    CALL fcmi (IgapGapIndex, eps, eppp, sig, repsp, rfepp, feps, dlrod, dlrel, CreepStrain1, &
                      &        PrevCladStrain, PrevFuelStrain, PrevOldCladAvTemp, PrevOldGapPress, PrevOldCoolPress, &
                      &        PrevOldFuelDispl, IgapIndexPrevOld, eppsv, deltime, atime, PrevCreepStrain, repsv, &
                      &        rfpsv, epp1, CladH2Concen, ExcessH2Concen, UniformAxNodStrn, iquit, j-1, it, nplast, nrelax)
                    DO ij = 1, (na - 1)
                        CreepStrain(ij) = PrevCreepStrain(ij)
                        OldCladAvTemp(ij) = PrevOldCladAvTemp(ij)
                        OldCoolPress(ij) = PrevOldCoolPress(ij)
                        OldGapPress(ij) = PrevOldGapPress(ij)
                        OldFuelDispl(ij) = PrevOldFuelDispl(ij)
                        IgapIndexOld(ij) = IgapIndexPrevOld(ij)
                        OldCladStrn(ij) = PrevCladStrain(ij)
                        OldFuelStrn(ij) = PrevFuelStrain(ij)
                        CladEffPlasStrain(ij) = PrevCladEffPlasStrn(ij)
                        DO ik = 1, 3
                            epp(ij,ik) = eppp(ij,ik)
                            reps(ij,ik) = repsp(ij,ik)
                            rfeps(ij,ik) = rfepp(ij,ik)
                            ! Update creep paramters
                            stold(ij) = stnew(ij)
                            deltimeold(ij) = deltimenew(ij)
                            sagold(ij) = sagnew(ij)
                            delsagold(ij) = delsagnew(ij)
                        END DO
                    END DO
                    !
                    atime = atime + deltime
                END DO
            END IF
            deltime = MOD(x11, x22)
            atime = ProblemTime(it) * sectohr - deltime
            IF (deltime <= 0.0_r8k .OR. ihere == 1) RETURN
            ihere = 1
            ncrep = 1
        END DO CreepLoop
    END SELECT
    !
    END SUBROUTINE ccreep
    !
END MODULE CladCreep