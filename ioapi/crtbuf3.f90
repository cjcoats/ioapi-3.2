LOGICAL FUNCTION CRTBUF3( FID ) RESULT( CRTFLAG )

    !***********************************************************************
    ! Version "$Id: crtbuf3.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2011 Baron Advanced Meteorological Systems, and
    ! (C) 2011 David Wong,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line 75
    !
    !  FUNCTION:  Create "BUFFERED "file" FNAME using info stored in the FDESC3
    !             common.
    !
    !  PRECONDITIONS REQUIRED:  File does not yet exist.  Should only be
    !                           called from OPEN3().
    !                           "File" must be one of types GRDDED3,
    !                           BNDARY3, or CUSTOM3.
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !
    !  REVISION  HISTORY:
    !       prototype 07/1994 by CJC
    !
    !       Revised   10/1996 by CJC:  new file type TSERIES3 for hydrology work.
    !
    !       Modified  05/1998 by CJC for OpenMP thread-safety
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Revised 4/2011 by David Wong, US EPA, and by CJC, to add state for
    !       full buffered-file file descriptions.  Arg-list bugfix for call
    !       to BUFCRE3().
    !
    !       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO;
    !       support for M3INT8 variables
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  index into STATE3 arrays


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: BUFCRE3 !  creates buffered file allocations


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         VSIZE           !  size for one variable
    INTEGER         VAR, LVL        !  loop counters

    !.............................................................................
    !   begin body of subroutine  CRTBUF3

    !.......   Compute size for a one-variable/one-layer buffer for supported
    !.......   "file" types, or error return:

    IF ( FTYPE3D .EQ. CUSTOM3 ) THEN        !  other dimensions not known
        VSIZE = NCOLS3D
    ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
        VSIZE = NCOLS3D * NROWS3D
    ELSE IF ( FTYPE3D .EQ. TSRIES3 ) THEN
        VSIZE = NCOLS3D * NROWS3D
    ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
        VSIZE = 2 * NTHIK3D
        VSIZE = IABS( VSIZE ) * ( NCOLS3D + NROWS3D + VSIZE )
    ELSE    !  illegal file type

    !$OMP CRITICAL( S_LOGOUT )

        WRITE( LOGDEV,91010 )                                       &
         'BUFFERED-file creation error for file ' // FLIST3( FID ), &
         'ILLEGAL FILE TYPE.', FTYPE3D
        CRTFLAG = .FALSE.

    !$OMP END CRITICAL( S_LOGOUT )

        RETURN

    END IF

    BSIZE3( FID ) = VSIZE
    CDFID3( FID ) = BUFFIL3

    !...........   Set attributes valid for all file types:
    !...........   FTYPE:  file type ID

    FTYPE3( FID ) = FTYPE3D
    NVARS3( FID ) = NVARS3D
    SDATE3( FID ) = SDATE3D
    STIME3( FID ) = STIME3D
    TSTEP3( FID ) = TSTEP3D
    MXREC3( FID ) = 0
    TINDX3( FID ) = IMISS3

    !...........   Set grid and coordinate system parameters

    NTHIK3( FID ) = NTHIK3D
    NCOLS3( FID ) = NCOLS3D
    NROWS3( FID ) = NROWS3D
    NLAYS3( FID ) = NLAYS3D
    NVARS3( FID ) = NVARS3D
    GDTYP3( FID ) = GDTYP3D
    P_ALP3( FID ) = P_ALP3D
    P_BET3( FID ) = P_BET3D
    P_GAM3( FID ) = P_GAM3D
    XCENT3( FID ) = XCENT3D
    YCENT3( FID ) = YCENT3D
    XORIG3( FID ) = XORIG3D
    YORIG3( FID ) = YORIG3D
    XCELL3( FID ) = XCELL3D
    YCELL3( FID ) = YCELL3D

    VGTYP3( FID ) = VGTYP3D
    VGTOP3( FID ) = VGTOP3D

    DO LVL = 1, MIN( NLAYS3D+1, MXLAYS3)
        VGLVS3( LVL,FID ) = VGLVS3D( LVL )
    END DO

    !.......   Define all the Models-3 variables for this file:

    DO  VAR = 1 , NVARS3D
        VINDX3( VAR,FID ) = IMISS3
        VTYPE3( VAR,FID ) = VTYPE3D( VAR )
        ILAST3( VAR,FID ) = 0
        LDATE3( VAR,FID ) = IMISS3
        LTIME3( VAR,FID ) = IMISS3
        NDATE3( VAR,FID ) = IMISS3
        NTIME3( VAR,FID ) = IMISS3
        VLIST3( VAR,FID ) = VNAME3D( VAR )
        UNITS3( VAR,FID ) = UNITS3D( VAR )
        IF ( VTYPE3D( VAR ) .EQ. M3DBLE ) THEN
            CALL M3WARN( 'OPEN3/CRTBUF3', 0, 0,                     &
               'DOUBLE PRECISION BUFFERRED not supported for '//    &
               VNAME3D( VAR ) )
            CRTFLAG = .FALSE.
            RETURN
        END IF
    END DO

    !.......   Call BUFCRE3() to allocate buffers for each variable
    !.......   in this "file"

    CRTFLAG = ( 0 .NE. BUFCRE3( FID, NVARS3D, NLAYS3D,          &
                                VSIZE, VTYPE3D, TSTEP3D ) )

    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Error and warning message formats..... 91xxx

91010 FORMAT ( //5X , '>>> WARNING in subroutine CRTBUF3 <<<',  &
               2 ( /5X , A , : ) , I5, // )


END FUNCTION CRTBUF3

