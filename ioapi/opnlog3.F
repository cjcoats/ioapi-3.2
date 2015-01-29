
        SUBROUTINE OPNLOG3 ( FID , EQNAME, STATUS )

C***********************************************************************
C Version "$Id: opnlog3.F 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  99
C
C  FUNCTION:  puts description of file at FID into the program log.
C
C  PRECONDITIONS REQUIRED:
C       Models-3 file with index FID must have already opened by OPEN3().
C       To be called by OPEN3().
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       Prototype 3/1992 by CJC
C       Modified  7/1994 by CJC to return new coordinate-system and
C                             grid-description parameters
C       Modified  5/1998 by CJC:  for OpenMP thread-safety
C       Modified  5/2003 by CJC:  Log volatile, read/write status
C       Modified  1/2004 by CJC:  use "m3flush()" from "m3msg2.F"
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER      , INTENT(IN   ) :: FID             !  subscript for STATE3 arrays
        CHARACTER*(*), INTENT(IN   ) :: EQNAME          !  physical file name
        INTEGER      , INTENT(IN   ) :: STATUS          !  file-opening status

                        
C...........   EXTERNAL FUNCTIONS:
        
        CHARACTER*24, EXTERNAL :: DT2STR
        CHARACTER*10, EXTERNAL :: HHMMSS
        INTEGER,      EXTERNAL :: LBLANK
        
        
C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER        FNUM       ! netCDF file ID from NCCRE()
        INTEGER        IERR       ! netCDF error-result argument
        CHARACTER*24   TVALUE     ! for DT2STR()  result
        CHARACTER*10   SVALUE     ! for HHMMSS()  result
        CHARACTER*80   EXECID     ! execution-id
        CHARACTER*20   RBUF       ! for NROWS3D
        CHARACTER*20   CBUF       ! for NROWS3D
        CHARACTER*20   LBUF       ! for NCOLS3D
        CHARACTER*20   VBUF       ! for NVARS3D
        CHARACTER*20   TBUF       ! for NTHIK3D  
        CHARACTER*20   KBUF       ! for KF records
        CHARACTER*20   SBUF       ! for file status
                                                 
        CHARACTER*16, PARAMETER :: SNAME( 0:6 ) =
     &     (/ CMISS3,                   !  0:  bad status
     &        'OLD:READ-ONLY   ',       !  1
     &        'OLD:READ-WRITE  ',       !  2
     &        'NEW(READ-WRITE )',       !  3
     &        'UNKNOWN(R-W)    ',       !  4
     &        'NEW(CREAT/TRUNC)',       !  5
     &        CMISS3 /)                 !  6:  bad status

        CHARACTER*8, PARAMETER :: TYPNAME( -4 : 7 ) =
     &     (/ 'UNKNOWN',        !  -4:  file type error
     &        'KFEVNT3',        !  -3:  known file types
     &        'DGRAPH3',        !  -2:    "
     &        'CUSTOM3',        !  -1:    "
     &        'DCTNRY3',        !   0:    "
     &        'GRDDED3',        !   1:    "
     &        'BNDARY3',        !   2:    "
     &        'IDDATA3',        !   3:    "
     &        'PROFIL3',        !   4:    "
     &        'GRNEST3',        !   5:    "
     &        'SMATRX3',        !   6:    "
     &        'UNKNOWN'  /)     !   7:    "


C.............................................................................
C   begin body of subroutine  OPEN3

C.......   Log the file description:
        
        FNUM  = CDFID3( FID )
                
!$OMP   CRITICAL( S_LOGOUT )

C.......   Get execution-ID for the program which produced this file:

        IF( FNUM .GE. 0 ) THEN
!$OMP CRITICAL( S_NC )
            CALL NCAGTC( FNUM, NCGLOBAL, 
     &                   'EXEC_ID', EXECID, MXDLEN3, IERR )
!$OMP END CRITICAL( S_NC )
        ELSE            !  "BUFFERED" files
            IERR   = 0
            EXECID = EXECN3
        END IF
        IF ( IERR .NE. 0 ) THEN
            EXECID = CMISS3
            WRITE( LOGDEV,91010 )
     &          'Warning netCDF file header attribute EXEC_ID.',
     &          'Not available for file:  ' // FLIST3( FID ) ,
     &          'netCDF error number', IERR
            WRITE( LOGDEV,93000 ) 
     &  ' ',
     &  '"' // TRIM( FLIST3( FID ) ) // 
     &  '" opened as ' // SNAME( MAX( MIN( STATUS, 6 ), 0  ) ), 
     &  'File name "'  // TRIM( EQNAME ) // '"',
     &  'File type '   // TYPNAME( MIN( MAX( FTYPE3( FID ), -4 ), 7 ) ),
     &  'Grid name "'  // TRIM( GDNAM3( FID ) )
     &                 // '"'
        ELSE
            IF ( EXECID( 1:1 ) .EQ. CHAR( 0 ) ) EXECID = CMISS3
            WRITE( LOGDEV,93000 ) 
     &  ' ',
     &  '"' // TRIM( FLIST3( FID ) ) // 
     &  '" opened as ' // SNAME( MAX( MIN( STATUS, 6 ), 0 ) ), 
     &  'File name "'  // TRIM( EQNAME ) // '"',
     &  'File type '   // TYPNAME( MIN( MAX( FTYPE3( FID ), -4 ), 7 ) ),
     &  'Execution ID "' // TRIM( EXECID ) // '"',
     &  'Grid name "'  // TRIM( GDNAM3( FID ) )
     &                 // '"'
        END IF          !  ierr nonzero:  NCAGT() failed
        
        
        WRITE( RBUF,94010 ) NROWS3( FID )
        WRITE( CBUF,94010 ) NCOLS3( FID )
        WRITE( LBUF,94010 ) NLAYS3( FID )
        WRITE( VBUF,94010 ) NVARS3( FID )
        IF ( FTYPE3( FID ) .EQ. BNDARY3 ) THEN
            WRITE( TBUF,94010 ) NTHIK3( FID )
            WRITE( LOGDEV,93000 ) 
     &          'Dimensions: '
     &          // RBUF( LBLANK( RBUF )+1 : 20 ) // ' rows, '
     &          // CBUF( LBLANK( CBUF )+1 : 20 ) // ' cols, '
     &          // LBUF( LBLANK( LBUF )+1 : 20 ) // ' lays, '
     &          // VBUF( LBLANK( VBUF )+1 : 20 ) // ' vbles, '
     &          // TBUF( LBLANK( TBUF )+1 : 20 ) // ' cells thick'
        ELSE IF ( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN
            WRITE( TBUF,94010 ) NTHIK3( FID )
            WRITE( KBUF,94010 ) MXREC3( FID )
            WRITE( LOGDEV,93000 ) 
     &          'Dimensions: '
     &          // RBUF( LBLANK( RBUF )+1 : 20 ) // ' rows, '
     &          // CBUF( LBLANK( CBUF )+1 : 20 ) // ' cols, '
     &          // LBUF( LBLANK( LBUF )+1 : 20 ) // ' lays, '
     &          // VBUF( LBLANK( VBUF )+1 : 20 ) // ' vbles, '
     &          // TBUF( LBLANK( TBUF )+1 : 20 ) // ' max recs/cell, '
     &          // KBUF( LBLANK( KBUF )+1 : 20 ) // ' total recs'
        ELSE
            WRITE( LOGDEV,93000 ) 
     &          'Dimensions: '
     &          // RBUF( LBLANK( RBUF )+1 : 20 ) // ' rows, '
     &          // CBUF( LBLANK( CBUF )+1 : 20 ) // ' cols, '
     &          // LBUF( LBLANK( LBUF )+1 : 20 ) // ' lays, '
     &          // VBUF( LBLANK( VBUF )+1 : 20 ) // ' vbles'
        END IF
        
        IF ( VOLAT3( FID ) ) THEN
            IF( RONLY3( FID ) ) THEN
                SBUF = 'VOLATILE READONLY'
            ELSE
                SBUF = 'VOLATILE READWRITE'
            END IF
        ELSE
            IF( RONLY3( FID ) ) THEN
                SBUF = 'READONLY'
            ELSE
                SBUF = 'READWRITE'
            END IF
        END IF

        IF ( FNUM .GE. 0 ) THEN
            WRITE( LOGDEV,93030 ) 
     &          'NetCDF ID: ', CDFID3( FID ), 'opened as ', SBUF
        ELSE IF ( FNUM .EQ. BUFFIL3 ) THEN
            WRITE( LOGDEV, '( 5X, 2A )' )
     &          'BUFFERED "file" opened as ', SBUF
        ELSE IF ( FNUM .EQ. VIRFIL3 ) THEN
            WRITE( LOGDEV, '( 5X, 2A )' )
     &          'VIRTUAL "file" opened as ', SBUF
        ELSE IF ( FNUM .EQ. LSTFIL3 ) THEN
            WRITE( LOGDEV, '( 5X, 2 A )' ) 
     &          'LIST-FILE SEQUENCE opened as ', SBUF
        ELSE IF ( FNUM .EQ. BINFIL3 ) THEN
            WRITE( LOGDEV, '( 5X, 2A )' ) 
     &          'Native-binary file opened as ', SBUF
        END IF

        IF( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN

            TVALUE = DT2STR( SDATE3( FID ), STIME3( FID ) ) 
            WRITE( LOGDEV,93020 ) 
     &      'Starting date and time', SDATE3( FID ),  STIME3( FID ),
     &      ' (' // TRIM( TVALUE ) //  ')',
     &      'Maximum current record number', MXREC3( FID )

        ELSE IF( TSTEP3( FID ) .EQ. 0 ) THEN

            WRITE( LOGDEV,93000 ) 'Time-independent data.'

        ELSE

            TVALUE = DT2STR( SDATE3( FID ), STIME3( FID ) ) 
            SVALUE = HHMMSS( ABS( TSTEP3( FID ) ) ) 
            WRITE( LOGDEV,93020 ) 
     &      'Starting date and time', SDATE3( FID ),  STIME3( FID ),
     &      ' (' // TRIM( TVALUE ) //  ')',
     &      'Timestep                     ', TSTEP3( FID ),
     &      ' (' // TRIM( SVALUE ) //  ' hh:mm:ss)',
     &      'Maximum current record number', MXREC3( FID )

        END IF          !  if kf-file, or time step zero, or not

	CALL M3FLUSH( LOGDEV )
        
!$OMP   END CRITICAL( S_LOGOUT )


        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine OPNLOG3 <<<',
     &            3 ( /5X , A , : ) , I5, // )


C...........   Informational log message formats..... 93xxx

93000   FORMAT ( 5X, A )

93010   FORMAT ( 5X, A, I9, ':', I6.6, A,
     &          /5X, A, I8 )

93020   FORMAT ( 5X, A, I9, ':', I6.6, A, /5X, A, I11.6, A,
     &          /5X, A, I10 )

93030   FORMAT ( 5X, A, :, I9, 2X, A, A )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( I20 )

        END  SUBROUTINE OPNLOG3

