
        LOGICAL   FUNCTION DESC3 ( FNAME )

C***********************************************************************
C Version "$Id: desc3.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems, and
C (C) 2011 David Wong, US EPA
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 110
C
C  FUNCTION:
C       puts description of FNAME into file description commons,
C       found in include file FDESC3.EXT.
C
C  RETURN VALUE:
C       TRUE iff successful
C
C  PRECONDITIONS REQUIRED:
C       Models-3 file with logical name FNAME must have already
C       been opened by OPEN3()
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C	prototype 3/1992 by CJC
C
C	modified  7/1994 by CJC to return new coordinate-system and
C                             grid-description parameters
C
C	Modified 10/1996 by CJC to support file type TSERIES3
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  5/1999 by ALT for coupling-mode operation
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified  3/2002 by CJC:  STATE3V changes
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses new INTEGER NAME2FID and
C       LOGICAL SYNCFID; more critical sections for OpenMP thread-safety
C
C       Modified 06/2005 by CJC:  formatting
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Revised 4/2011 by David Wong, US EPA, and by CJC, to add state for
C       full buffered-file file descriptions.  Re-organized accordingly CJC
C
C       Modified 05/2011 by CJC:  better error-message
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'FDESC3.EXT'
        INCLUDE 'ATDSC3.EXT'
        INCLUDE 'NETCDF.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be described.


C...........   EXTERNAL FUNCTIONS & BLOCK DATA

        INTEGER, EXTERNAL :: DSCBIN3    !  native-binary (BINIO3) description stuff
        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
        INTEGER, EXTERNAL :: INIT3      !  initialize I/O system files.
        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup
        LOGICAL, EXTERNAL :: SYNCFID

        EXTERNAL :: INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript for STATE3 arrays
        INTEGER         CDF             !  netCDF file ID from NCCRE()
        INTEGER         IERR            !  netCDF error status return
        INTEGER         VAR, LVL, ATT   !  loop counter for file variables
        CHARACTER*256   MESG            !  message-buffer
        LOGICAL         EFLAG

#ifdef IOAPICPL
        LOGICAL       DESC3V
        EXTERNAL      DESC3V
#endif

C.............................................................................
C   begin body of subroutine  OPEN3

C.......   Find STATE3 index for the file:

        FID  = NAME2FID( FNAME )

        IF ( FID .EQ. 0 ) THEN
            MESG = 'Invalid file name argument "' // FNAME // '"'
            CALL M3WARN( 'DESC3', 0, 0, MESG )
            DESC3 = .FALSE.
            RETURN
        END IF

        CDF  = CDFID3( FID )
        EFLAG = .FALSE.

C.......   Characteristics stored in STATE3.EXT:

        FTYPE3D = FTYPE3( FID )

        NVARS3D = NVARS3( FID )
        DO   VAR = 1, NVARS3D
            VNAME3D( VAR ) = VLIST3( VAR,FID )
            VTYPE3D( VAR ) = VTYPE3( VAR,FID )
            UNITS3D( VAR ) = UNITS3( VAR,FID )
        END DO

        IF ( FTYPE3D .NE. DCTNRY3 ) THEN

            SDATE3D = SDATE3( FID )
            STIME3D = STIME3( FID )
            TSTEP3D = TSTEP3( FID )
            MXREC3D = MXREC3( FID )
            NTHIK3D = NTHIK3( FID )
            NCOLS3D = NCOLS3( FID )
            NROWS3D = NROWS3( FID )
            NLAYS3D = NLAYS3( FID )
            GDTYP3D = GDTYP3( FID )
            P_ALP3D = P_ALP3( FID )
            P_BET3D = P_BET3( FID )
            P_GAM3D = P_GAM3( FID )
            XCENT3D = XCENT3( FID )
            YCENT3D = YCENT3( FID )
            XORIG3D = XORIG3( FID )
            YORIG3D = YORIG3( FID )
            XCELL3D = XCELL3( FID )
            YCELL3D = YCELL3( FID )
            GDNAM3D = GDNAM3( FID )

            VGTYP3D = VGTYP3( FID )
            VGTOP3D = VGTOP3( FID )

            DO LVL = 1, NLAYS3D+1
                VGLVS3D( LVL ) = VGLVS3( LVL,FID )
            END DO

        END IF  !  if not a dictionary file


        IF ( CDF .EQ. BUFFIL3 ) THEN

            EXECN3D = EXECN3
            CDATE3D = CURDATE
            CTIME3D = CURTIME
            WDATE3D = CURDATE
            WTIME3D = CURTIME
            UPNAM3D = CMISS3
            FDESC3D = ' '
            UPDSC3D = ' '

            DESC3 = .TRUE.
            RETURN

        END IF		!  if "buffered" file

#ifdef IOAPICPL

        IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  VIRTUAL "file"
            DESC3  = DESC3V( PLIST3( FID ) )
            RETURN
        END IF		!  if "virtual" file

#endif

        IF ( VOLAT3( FID ) ) THEN      !  volatile file:  synch with disk

            IF ( .NOT. SYNCFID( FID ) ) THEN
                MESG = 'Error with disk synch for file '//FLIST3( FID )
                CALL M3WARN( 'DESC3', 0, 0, MESG )
                DESC3 = .FALSE.
                RETURN
            END IF              !  if synch failed

        END IF                  !  if file is volatile

        IF ( CDF .EQ. BINFIL3 ) THEN

            IF ( DSCBIN3( FID ) .EQ. 0 ) THEN
                MESG = 'Error reading header for BINFIL3 file '// FNAME
                CALL M3WARN( 'DESC3', 0, 0, MESG )
                DESC3 = .FALSE.
                RETURN
            END IF

            RETURN

        END IF		!  if "native-binary file; else if "buffered" file


C.......   If not netcdf file:  already processed, so return

        IF ( CDF .LT. 0 ) THEN

            DESC3 = .TRUE.
            RETURN

        END IF		!  if "native-binary file; else if "buffered" file


C.......   Else read characteristics from the file header:

!$OMP CRITICAL( S_NC )

        CALL NCAGTC( CDF, NCGLOBAL, 'EXEC_ID', EXECN3D, MXDLEN3, IERR)
        IF ( IERR .NE. 0 ) THEN
            EXECN3D = CMISS3
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute EXEC_ID ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
        END IF          !  ierr nonzero:  NCAGT() failed

        CALL NCAGT( CDF, NCGLOBAL, 'CDATE', CDATE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute CDATE ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCAGT() failed

        CALL NCAGT( CDF, NCGLOBAL, 'CTIME', CTIME3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute CTIME ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCAGT() failed

        CALL NCAGT( CDF, NCGLOBAL, 'WDATE', WDATE3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute WDATE ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCAGT() failed

        CALL NCAGT( CDF, NCGLOBAL, 'WTIME', WTIME3D, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute WTIME ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCAGT() failed

        CALL NCAGTC( CDF, NCGLOBAL, 'UPNAM', UPNAM3D, NAMLEN3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute UPNAM ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCAGT() failed

        CALL NCAGTC( CDF, NCGLOBAL, 'FILEDESC', FDESC3D,
     &               MXDLEN3 * MXDESC3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute FDESC ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCAGT() failed

        CALL NCAGTC( CDF, NCGLOBAL, 'HISTORY', UPDSC3D,
     &               MXDLEN3 * MXDESC3, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 4 A )' )
     &          'netCDF file header attribute UPDSC ',
     &          'not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  NCAGT() failed


        IF ( FTYPE3D .NE. DCTNRY3 ) THEN

            DO  222  VAR = 1, NVARS3D

                CALL NCAGTC( CDF, VINDX3( VAR,FID ), 'var_desc',
     &                       VDESC3D( VAR ), MXDLEN3, IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' )'netCDF error number', IERR
                    CALL M3MSG2( MESG )
                    WRITE( MESG, '( 4 A )' )
     &                  'Error reading netCDF variable attribute ',
     &                  '"var_desc" for file "', FNAME, '"'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF          !  ierr nonzero:  NCAGT() failed

222         CONTINUE

C...............   TSERIES3 files:  get numbers of extra per-variable
C...............   attributes, lists of names and values for them:

            IF ( FTYPE3D .EQ. TSRIES3 ) THEN

                DO 244 VAR = 1,NVARS3D

                    CALL NCAGT( CDF, VINDX3( VAR,FID ), 'natts',
     &                          NATTS3D( VAR ), IERR )
                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, '( A, I9)' )
     &                      'netCDF error number', IERR
                        CALL M3MSG2( MESG )
                        WRITE( MESG, '( 4 A )' )
     &                      'Error reading netCDF dimension ',
     &                      '"natts" for file "', FNAME, '"'
                        CALL M3MSG2( MESG )
                        EFLAG = .TRUE.
                    END IF

                    DO 233 ATT = 1, NATTS3D( VAR )   ! loop on each addtnl atts


                        CALL NCANAM( CDF, VINDX3( VAR,FID ), ATT+4,
     &                               ATNAM3D( ATT,VAR ), IERR )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, '( A, I9)' )
     &                          'netCDF error number', IERR
                            CALL M3MSG2( MESG )
                            WRITE( MESG, '( A, I9, 1X, 4A )' )
     &                          'Error reading attribute ', ATT,
     &                          ' for variable "',  VNAME3D( VAR ),
     &                          '" in file "', FNAME, '"'
                            CALL M3MSG2( MESG )
                            EFLAG = .TRUE.
                        END IF

                        CALL NCAGT( CDF, VINDX3( VAR,FID ),
     &                              ATNAM3D( ATT,VAR ),
     &                              FATTS3D( ATT,VAR ), IERR )
                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, '( 7A )' )
     &                          'Error reading attribute "',
     &                          ATNAM3D( ATT,VAR ),
     &                          '" for variable "',  VNAME3D( VAR ),
     &                          '" in file "', FNAME, '"'
                            CALL M3MSG2( MESG )
                            EFLAG = .TRUE.
                        END IF

233                 CONTINUE		!  end loop on ATTs for this VAR

244             CONTINUE	!  end loop on variables VAR in this file

            END IF		!  if ftype tseries3

        END IF          !  if not a dictionary file

!$OMP END CRITICAL( S_NC )

        IF ( EFLAG ) THEN
            WRITE( MESG, '( 3 A )' ) 'Error(s) in DESC3(', FNAME, ')'
            CALL M3WARN( 'DESC3', 0, 0, MESG )
        END IF

        DESC3 = ( .NOT. EFLAG )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91001   FORMAT ( //5X , '>>> WARNING in subroutine DESC3 <<<',
     &            /5X , 3A,
     &            /5X , A , I5, // )

91010   FORMAT ( //5X , '>>> WARNING in subroutine DESC3 <<<',
     &            3 ( /5X , A , : ) , I5, // )

91020   FORMAT ( //5X , '>>> WARNING in subroutine DESC3 <<<',
     &            /5X , A , I5, A,
     &            /5X , A , I5 , // )

        END FUNCTION DESC3

