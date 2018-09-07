
        PROGRAM MTXCPLE

C***********************************************************************
C Version "$Id: mtxcple.f 108 2018-09-07 18:59:37Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
C (C) 2014-2018 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  143
C
C  DESCRIPTION:
C       Reads sparse (grid-to-grid transform) matrix.
C       For each time step in the specified time step sequence,
C       reads all variables from the specified input file, optionally
C       under the control of the specified synchronization file,
C       uses the sparse matrix to transform it to the specified
C       output grid, and write it to the specified output file.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <physical (path) name> for the input,
C       output, and GRIDDESC files.
C       Input file and output grid use the same coordinate system.
C       Specified time step sequence is valid for both the input and
C       synch files.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Adapted  9/2000 by Carlie J. Coats, Jr., NCSC, from "m3cple.f"
C       Version 11/2001 by CJC for I/O API Version 2.1
C       Version  5/2004 by CJC:  additional map projection support, etc.
C       Version  6/2005 by CJC:  improved default for NRECS
C       Version 11/2005 by CJC:  eliminate unused vbles
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version 01/2013 by CJC:  use new LASTTIME() to find EDATE:ETIME
C       Version 12/2014 by CJC for I/O API v3.2:  USE MODATTS3::GETMTXATT();
C       consistency checking with FILCHK3(), GRDCHK3(); OpenMP parallel
C       matrix-multiply
C       Version 04/2018 by CJC:  Re-grid only REAL variables; bug-fixes
C       at lines 251, 423-425
C***********************************************************************

      USE M3UTILIO
      USE MODATTS3
      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

        INTEGER,      PARAMETER :: TYPES( 3 ) = 
     &       (/ GRDDED3, BNDARY3, CUSTOM3 /)

        CHARACTER*32, PARAMETER :: TMENU( 3 ) =
     &       (/ 'Output file type GRIDDED  ',
     &          'Output file type BOUNDARY ',
     &          'Output file type CUSTOM   '    /)

        CHARACTER*24, PARAMETER :: GTYPES( 0:10 ) =
     &       (/ 'Unknown/Invalid       ',
     &          'Latitude-Longitude    ',
     &          'Lambert               ',
     &          'General Mercator      ',
     &          'General Stereographic ',
     &          'UTM                   ',
     &          'Polar Stereographic   ',
     &          'Equatorial Mercator   ',
     &          'Transverse Mercator   ',
     &          'Albers Equal-Area     ',
     &          'Unknown/Invalid       '    /)

       CHARACTER*16, PARAMETER :: PNAME = 'MTXCPLE'

C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    MNAME   !  input matrix file logical name
        CHARACTER*16    FNAME   !  input data   file logical name
        CHARACTER*16    SNAME   !  input synch  file logical name
        CHARACTER*16    SVBLE   !  input   synch variable   name
        CHARACTER*16    ONAME   !  output data file logical name
        CHARACTER*16    CNAME   !  output coordinate system name

        LOGICAL         SFLAG   !  true iff controlled by synch file
        LOGICAL         EFLAG

        CHARACTER*256   MESG

        INTEGER         LDEV        !  log-device
        INTEGER         STATUS      !  allocation-status

        INTEGER         V, N, L, C, R     !  loop counters

        CHARACTER*16    GDNAM1      !!  for input file
        INTEGER         GDTYP1
        REAL*8          P_ALP1
        REAL*8          P_BET1
        REAL*8          P_GAM1
        REAL*8          XCENT1
        REAL*8          YCENT1
        REAL*8          XORIG1
        REAL*8          YORIG1
        REAL*8          XCELL1
        REAL*8          YCELL1
        INTEGER         NCOLS1
        INTEGER         NROWS1
        INTEGER         NLAYS1
        INTEGER         NSIZE1      ! number of input-grid cells

        CHARACTER*16    GDNAM2      !!  for output file
        INTEGER         GDTYP2
        REAL*8          P_ALP2
        REAL*8          P_BET2
        REAL*8          P_GAM2
        REAL*8          XCENT2
        REAL*8          YCENT2
        REAL*8          XORIG2
        REAL*8          YORIG2
        REAL*8          XCELL2
        REAL*8          YCELL2
        INTEGER         NCOLS2
        INTEGER         NROWS2
        INTEGER         NSIZE2      ! number of output-grid cells
        INTEGER         FTYPE2      ! output file type

        INTEGER         NCOLSM      !!  for matrix file
        INTEGER         NROWSM

        INTEGER         JDATE, JTIME, TSTEP
        INTEGER         EDATE, ETIME, TSECS, NRECS

        REAL,    ALLOCATABLE::   INBUF( :, :, : )
        REAL,    ALLOCATABLE::   OUTBUF( :, :, : )
        INTEGER, ALLOCATABLE::   CBUF( : )


C***********************************************************************
C   begin body of program MTXCPLE

        LDEV = INIT3()

        WRITE( *, '( 5X, A )' )
     & ' ',
     & 'Program MTXCPLE to read a sparse (grid-to-grid transform)',
     & 'matrix, and then all REAL variables in each time step in',
     & 'the specified time step sequence from the specified input',
     & 'file, optionally under the control of the specified',
     & 'synchronization file, copy or interpolate them to the ',
     & 'output grid, and write them to the specified output file.',
     & ' ',
     & 'THE PROGRAM WILL PROMPT YOU for the logical names of the',
     & 'input matrix file, the input data file, the input synch file',
     & 'the GRIDDESC name of the output grid, the output file, the',
     & 'time step sequence.',
     & 'Default responses are indicated in square brackets',
     & '[LIKE THIS], and may be accepted by hitting the RETURN key.',
     & ' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv <input matrix file>    <path-name>',
     & '    setenv <input data   file>    <path-name>',
     & '    setenv <input synch  file>    <path-name, or "NONE">',
     & '    setenv GRIDDESC               <path-name> (if interp)',
     & '    time step sequence is valid for both input files',
     & '    For interpolation, file type must be GRIDDED, and either',
     & '    the input and output coordinate system types must be the',
     & '    same, or must be selected from one of the following',
     & '    supported coordinate conversions:',
     & '        Lambert  to/from  Lambert (w/ different parameters),',
     & '        Lambert  to/from  Lat-Lon,',
     & '        Lambert  to/from  UTM,',
     & '        Lat-Lon  to/from  Polar Stereographic,',
     & '        Lat-Lon  to/from  Transverse Mercator,',
     & '        Lat-Lon  to/from  Equatorial Mercator,',
     & '    For interpolation, the output grid should have a finer',
     & '    resolution than the input grid (else you should use an',
     & '    aggregation program instead of an input program).',
     & '    For copy, file type must be GRIDDED, BOUNDARY, or CUSTOM.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     & ' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC., and (C) 2014-2018 UNC',
     &'Institute for the Environment.',
     &'Released under Version 2 of the GNU General Public License.',
     &'See enclosed GPL.txt, or URL',
     &'https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    carlie@jyarborough.com',
     &'or',
     &'    UNC Institute for the Environment',
     &'    137 E. Franklin St. Suite 602 Room 613-C',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: mtxcple.f 108 2018-09-07 18:59:37Z coats                $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Program terminated at user request', 2 )
        END IF


C...............  Open and get description for optional synch file

        MESG  = 'Enter name for input synch file, or "NONE"'
        SNAME = PROMPTMFILE( MESG, FSREAD3, 'NONE', PNAME )

        CALL LUSTR( SNAME )
        SFLAG = ( SNAME .NE. 'NONE ' )

        IF ( SFLAG ) THEN

            IF ( .NOT. DESC3( SNAME ) ) THEN
                MESG = 'Could not get file description for ' // SNAME
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            NCOLS2 = NCOLS3D
            NROWS2 = NROWS3D
            SVBLE  = VNAME3D( 1 )

        END IF          !  if synch-flag option taken


C...............  Open and get description for input matrix transform file

        MESG  = 'Enter name for input matrix transform file'
        MNAME = PROMPTMFILE( MESG, FSREAD3, 'MATRIX', PNAME )

        IF ( .NOT. DESC3( MNAME ) ) THEN
            MESG = 'Could not get file description for ' // MNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.GETMTXATT( MNAME,
     &      GDNAM1, GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &              XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1,
     &      GDNAM2, GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,
     &              XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 )
     &           ) THEN
            MESG = 'Could not get MATX ATTS for ' // MNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        NCOLSM = NCOLS3D
        NROWSM = NROWS3D

C...............  Open and get description for input data file

        MESG  = 'Enter name for input data file'
        FNAME = PROMPTMFILE( MESG, FSREAD3,
     &                       'IN_DATA', PNAME )

        IF ( .NOT. DESC3( FNAME ) ) THEN
            MESG = 'Could not get file description for ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.FILCHK3( FNAME,  FTYPE3D, NCOLS1, NROWS1,
     &                          NLAYS3D, NTHIK3D ) ) THEN
            MESG = 'Inconsistent dimensions  for ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.GRDCHK3( FNAME,
     &                      P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &                      XORIG1, YORIG1, XCELL1, YCELL1,
     &                      NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
            MESG = 'Inconsistent coord/grid  for ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE
            NLAYS1 = NLAYS3D
        END IF

        IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            NSIZE1 = NCOLS1 * NROWS1
        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
            NSIZE1 = 2*ABS( NTHIK3D )*( NCOLS3D + NROWS3D + 2*NTHIK3D )
        ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
            NSIZE1 = NCOLS3D
        END IF


C...............  Get output grid description, time step sequence

        MESG = 'Interpolating from grid "' // TRIM( GDNAM1 ) //
     &         '" to grid "'               // TRIM( GDNAM2 ) // '"'
        CALL M3MSG2( MESG )

        IF ( TSTEP3D .EQ. 0 ) THEN
            JDATE = 0
            JTIME = 0
            TSTEP = 0
            NRECS = 1
            GO TO  11
        END IF

        JDATE = GETNUM( SDATE3D, 9999999, SDATE3D,
     &                  'Enter STARTING DATE for time step sequence' )

        JTIME = GETNUM(      0, 9999999, STIME3D,
     &                  'Enter STARTING TIME for time step sequence' )

        TSTEP = GETNUM( TSTEP3D, 9999999, TSTEP3D,
     &                  'Enter   TIME STEP   for time step sequence' )

        CALL LASTTIME( SDATE3D,STIME3D,TSTEP3D, MXREC3D, EDATE,ETIME )
        N  =  CURREC( EDATE, ETIME, JDATE, JTIME, TSTEP, C, R )
        NRECS = GETNUM( 1, 9999999, N,
     &                  'Enter     NRECS     for time step sequence' )

11      CONTINUE        !  target of "if tstep3d is zero"

        FTYPE2  = TYPES( GETMENU( 3, 1,
     &                    'Enter FILE TYPE for output file', TMENU ) )
        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = TSTEP
        FTYPE3D = FTYPE2


C...............  Create output file, borrowing most of file
C...............  description from FNAME, grid-description part
C...............  from GRIDDESC file:

        GDNAM3D = GDNAM2
        NCOLS3D = NCOLS2
        NROWS3D = NROWS2
        NLAYS3D = NLAYS1
        GDTYP3D = GDTYP2
        P_ALP3D = P_ALP2
        P_BET3D = P_BET2
        P_GAM3D = P_GAM2
        XCENT3D = XCENT2
        YCENT3D = YCENT2
        XORIG3D = XORIG2
        YORIG3D = YORIG2
        XCELL3D = XCELL2
        YCELL3D = YCELL2
        N = 0
        DO V = 1, NVARS3D
            IF ( VTYPE3D( V ) .EQ. M3REAL ) THEN
                N = N + 1
                VNAME3D( N ) = VNAME3D( V )
                UNITS3D( N ) = UNITS3D( V )
                VDESC3D( N ) = VDESC3D( V )
                VTYPE3D( N ) = VTYPE3D( V )                
            ELSE
                MESG = 'Excluding non-REAL variable "' // 
     &                  TRIM( VNAME3D( V ) ) // '"'
                CALL M3MESG( MESG )
            END IF
        END DO
        NVARS3D = N

        IF ( FTYPE2 .EQ. GRDDED3 ) THEN
            NSIZE2 = NCOLS3D*NROWS3D
            IF ( NROWSM .NE. NSIZE2 ) THEN
                MESG = 'Matrix NROWS does not match output NROWS*NCOLS'
                CALL M3MSG2( MESG )
                MESG = 'Inconsistent dimensions for sparse matrix'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
        ELSE IF ( FTYPE2 .EQ. BNDARY3 ) THEN
            NSIZE2 = 2*ABS( NTHIK3D )*( NCOLS3D + NROWS3D + 2*NTHIK3D )
            IF ( NROWSM .NE. NSIZE2 ) THEN
                MESG = 'Matrix NROWS does not match output PERIMETER'
                CALL M3MSG2( MESG )
                MESG = 'Inconsistent dimensions for sparse matrix'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
        ELSE IF ( FTYPE2 .EQ. CUSTOM3 ) THEN
            NSIZE2 = NCOLS3D
            IF ( NROWSM .NE. NSIZE2 ) THEN
                MESG = 'Matrix NROWS does not match output NCOLS'
                CALL M3MSG2( MESG )
                MESG = 'Inconsistent dimensions for sparse matrix'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
        END IF

        !!  Use the first empty FDESC3D line to describe the
        !!  interpolation source and target grids:

        DO L = 1, MXDESC3
            IF ( ( FDESC3D( L )        .EQ. ' ' ) .OR.
     &           ( FDESC3D( L )( 1:1 ) .EQ. CHAR( 0 ) )   ) THEN
                FDESC3D( L ) = 'Data interpolated from grid "' //
     &                         TRIM( GDNAM1 ) //
     &                         '" to grid "' // TRIM( GDNAM2 ) // '"'
                EXIT
            END IF
        END DO


C...............  Allocate buffers; compute re-gridding matrix

        ALLOCATE( INBUF ( NCOLS1, NROWS1, NLAYS3D ),
     &            OUTBUF( NCOLS2, NROWS2, NLAYS3D ),
     &            CBUF  ( 0:NROWSM + 2 * NCOLSM ),
     &            STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )
     &               'Buffer allocation failed:  STAT=', STATUS
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Open output file

        MESG  = 'Enter name for output data file'
        ONAME = PROMPTMFILE( MESG, FSUNKN3, 'OUT_DATA', PNAME )


C...............  Read the transform matrix, and put into parallel-algorithm order

        IF ( .NOT.READ3( MNAME, 'ALL', 1, 0, 0, CBUF(1) ) ) THEN
            MESG = 'Could not read transform matrix from ' // MNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF
        CBUF(0) = 0
        DO N = 1, NROWSM
            CBUF(N) = CBUF(N) + CBUF(N-1)
        END DO

C...............  Process output time step sequence

        DO  N = 1, NRECS

            IF ( SFLAG ) THEN
                IF ( .NOT. CHECK3( SNAME, SVBLE, JDATE, JTIME ) ) THEN
                    MESG = 'Failure checking variable "' //
     &                     TRIM( SVBLE ) // '" from synch file "' //
     &                     TRIM( SNAME ) // '"'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF
            END IF

            IF ( TSTEP3D .GT. 0 ) THEN
                WRITE( MESG, '( A, I7.7, A, I6.6 )' )
     &              'Processing  ', JDATE, ':', JTIME
                CALL M3MSG2( ' ' )
                CALL M3MSG2( MESG )
            END IF

            DO  V = 1, NVARS3D  !  loop on variables

                IF ( .NOT. READ3( FNAME, VNAME3D( V ), ALLAYS3,
     &                            JDATE, JTIME, INBUF ) ) THEN
                    MESG = 'Failure reading variable "' //
     &                     TRIM( VNAME3D( V ) )
     &                     // '" from file "' //
     &                     TRIM( FNAME ) // '"'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF

                CALL MATVEC( NSIZE1, NSIZE2, NLAYS3D,
     &                       NCOLSM, CBUF(0), CBUF(NROWSM+1),
     &                       CBUF(NCOLSM+NROWSM+1),
     &                       INBUF, OUTBUF )

                IF ( .NOT.WRITE3( ONAME, VNAME3D( V ),
     &                            JDATE, JTIME, OUTBUF ) ) THEN
                    MESG = 'Failure writing variable "' //
     &                     TRIM( VNAME3D( V ) ) // '" to file "' //
     &                     TRIM( ONAME ) // '"'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF

            END DO      !  end loop on variables


            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output time steps


C...............  Successful completion

        CALL M3EXIT( PNAME, 0, 0,
     &               'Successful completion of program MTXCPLE', 0 )

        END  PROGRAM MTXCPLE


C=======================================================================

        SUBROUTINE MATVEC( NCOLS, NROWS, NLAYS, NCOFF, N, I, M, U, V )

C***********************************************************************
C  subroutine body starts at line  51
C
C  FUNCTION:  multiply a sparse matrix <N,I,C> by a layered vector U and
C             return the layered result V
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       Adapted 9/2000 by CJC from I/O API "smatvec.f"
C
C***********************************************************************

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS           ! length of input vector
        INTEGER         NROWS           ! length of output vector
        INTEGER         NLAYS           ! number of layers in vectors
        INTEGER         NCOFF           ! max number of coefficients

        INTEGER         N( 0:NROWS )    ! cumulative # of entries per row
        INTEGER         I( NCOFF )      ! columns list
        REAL            M( NCOFF )      ! coefficient array

        REAL            U( NCOLS, NLAYS )      !  input vector
        REAL            V( NROWS, NLAYS )      ! output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, C, L
        REAL*8          SUM


C***********************************************************************
C   begin body of subroutine  MATVEC

        IF ( NLAYS .EQ. 1 ) THEN

!$OMP       PARALLEL DO
!$OMP&          DEFAULT( NONE ),
!$OMP&           SHARED( NROWS, NB, I, M, U, V ),
!$OMP&          PRIVATE( R, C, SUM )

            DO  R = 1, NROWS

                SUM = 0.0D0

                DO  C = N(R-1)+1, N(R)
                    SUM = SUM  +  M( C ) * U( I( C ), 1 )
                END DO

                V( R,1 ) = SUM

            END DO

        ELSE

!$OMP       PARALLEL DO
!$OMP&          DEFAULT( NONE ),
!$OMP&           SHARED( NLAYS, NROWS, NB, I, M, U, V ),
!$OMP&          PRIVATE( L, R, C, SUM )

            DO  L = 1, NLAYS

                DO  R = 1, NROWS

                    SUM = 0.0

                    DO  C = 1, N( R )
                        SUM = SUM  +  M( C ) * U( I( C ), L )
                    END DO

                    V( R,L ) = SUM

                END DO

            END DO

        END IF

        RETURN

        END SUBROUTINE MATVEC

