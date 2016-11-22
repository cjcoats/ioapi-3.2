
      PROGRAM M3HDR

C***********************************************************************
C Version "$Id: m3hdr.f 435 2016-11-22 18:10:58Z coats $"
C EDSS/Models-3 M3TOOLS.
C   Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C   (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
C   (C) 2014-2016 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  94
C
C  DESCRIPTION:
C       display header information from M3IO files
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Adapted 05/2003 by Carlie J. Coats, Jr., BAMS, from M3CPLE
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C       Version  06/2016 by CJC:  Log CMAQ or SMOKE metadata, if present
C***********************************************************************

      USE M3UTILIO
      USE MODATTS3
      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

      INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER(LEN=4), PARAMETER :: VTYPES( 0:7 ) =
     & (/
     & 'unkn', 'BYTE', 'CHAR', 'SHRT', 'INT ',  'REAL', 'DBLE',
     & 'unkn' /)

        CHARACTER(LEN=24), PARAMETER :: FTYPES( -4:9 ) =
     & (/
     & 'Unknown/unrecognized    ', 'KF (cloud) Event Data   ',
     & 'Directed-Graph          ', 'CUSTOM                  ',
     & 'File-Dictionary         ', 'GRIDDED Data            ',
     & 'Grid-BOUNDARY Data      ', 'ID-Referenced Data      ',
     & 'Vertical-PROFILE Data   ', 'Nested-GRID Data        ',
     & 'Sparse-MATRIX           ', 'Hydro TIME SERIES Data  ',
     & 'POINTER-FLYER Data      ', 'Unknown/unrecognized    '
     & /)

        CHARACTER(LEN=24), PARAMETER :: GTYPES( 0:9 ) =
     & (/
     & 'Unknown/unrecognized    ', 'Lat-Lonatic SIGMA-P     ',
     & 'Lambert Conformal Conic ', 'Mercator (general)      ',
     & 'Stereographic (general) ', 'UTM                     ',
     & 'Polar Stereographic     ', 'Equatorial Mercator     ',
     & 'Transverse Mercator     ', 'Unknown/unrecognized    '
     & /)

        CHARACTER(LEN=24), PARAMETER :: VGTYPES( 0:9 ) =
     & (/
     & 'Unknown/unrecognized    ', 'Hydrostatic SIGMA-P     ',
     & 'Non_Hydrostatic SIGMA-P ', 'SIGMA-Z                 ',
     & 'Pressure (mb)           ', 'Z (m) above sea level   ',
     & 'H (m) above ground      ', 'WRF EM ("dry" sigma-P)  ',
     & 'WRF NMM                 ', 'Unknown/unrecognized    '
     & /)

        CHARACTER*16, PARAMETER :: PNAME = 'M3HDR'
        CHARACTER*24, PARAMETER :: BLANK = ' '
        CHARACTER*16, PARAMETER :: BAR16 = '----------------'
        CHARACTER*64, PARAMETER :: BAR =
     &'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


C...........   LOCAL VARIABLES and their descriptions:

      CHARACTER*512 ENAME
      CHARACTER*16  FNAME
      CHARACTER*124 OUTLIN
      INTEGER       I, J, K, NARG, ISTAT, LDEV, LENF, LENR
      INTEGER       NLINE,LLB,NBL,IZONE,MXLLEN
      LOGICAL       AFLAG, EFLAG


C***********************************************************************
C   begin body of program M4CPLE

        EFLAG = .FALSE.
        LDEV  = INIT3()
        WRITE( *, '( 5X, A )' )
     &'Program M3HDR to display the header description of a Models-3',
     &'I/O API file in "human-readable" format.',
     &' ',
     &'USAGE',
     &'     m3hdr [filename]',
     &' ',
     &'PRECONDITIONS REQUIRED:',
     &'     Optionally:  setenv <logical name>  <path name>',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for file name, if it is not',
     &'supplied on the command line',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., (C) 2003-2010 Baron Advanced',
     &'Meteorological Systems, LLC., and (C) 2014-2016 UNC Institute',
     &'for the the Environment.',
     &'Released under Version 2 of the GNU General Public License,',
     &'Version 2. See enclosed GPL.txt, or URL',
     &' ',
     &'    http://www.gnu.org/copyleft/gpl.html',
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
     &'$Id:: m3hdr.f 435 2016-11-22 18:10:58Z coats                  $',
     &' '

        NARG  = IARGC()
        ENAME = BLANK
        IF ( NARG .EQ. 0 ) THEN
            CALL GETSTR( 'ENTER NAME OF I/O API FILE', 'INFILE', ENAME )
        ELSE
            CALL GETARG(1,ENAME)
        END IF

        !!  If ENAME exists, assume it is a physical file name;
        !!  else assume it is a logical name:

        INQUIRE( FILE=ENAME, EXIST=AFLAG, IOSTAT=ISTAT )
        IF ( AFLAG ) THEN
            FNAME = 'INFILE'
            IF( .NOT.SETENVVAR( FNAME, ENAME ) ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'SETENVVAR() ERROR', 2 )
            END IF
        ELSE IF ( LEN_TRIM( ENAME ) .GT. NAMLEN3 ) THEN
            OUTLIN = 'Bad input file "' // TRIM( ENAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, OUTLIN, 2 )
        ELSE
            FNAME = ENAME
        END IF

        IF (.NOT.OPEN3(FNAME, FSREAD3, PNAME) ) THEN

           CALL M3EXIT( PNAME, 0, 0, 'Could not open file', 2 )

        ELSE IF ( .NOT. DESC3(FNAME) ) THEN

           CALL M3EXIT( PNAME, 0, 0,
     &                 'Could not get file description file', 2 )

        ELSE    !  fix-up ASCII nulls, etc, in file description:

            FTYPE3D = MAX( -4, MIN( 9, FTYPE3D ) )
            GDTYP3D = MAX(  0, MIN( 9, GDTYP3D ) )
            VGTYP3D = MAX(  0, MIN( 9, VGTYP3D ) )

            DO I = 1, MXDLEN3
                K = ICHAR( EXECN3D(I:I) )
                IF ( K .EQ. 0 ) EXECN3D(I:I) = ' '
            END DO

            DO J = 1, NVARS3D
                VTYPE3D( J ) = MAX( 0, MIN( 7, VTYPE3D( J ) ) )
                DO I = 1, MXDLEN3
                    K = ICHAR( VDESC3D(J)(I:I) )
                    IF ( K .EQ. 0 ) VDESC3D(J)(I:I) = ' '
                END DO
                DO I = 1, NAMLEN3
                    K = ICHAR( VDESC3D(J)(I:I) )
                    IF ( K .EQ. 0 ) VDESC3D(J)(I:I) = ' '
                    K = ICHAR( VDESC3D(J)(I:I) )
                    IF ( K .EQ. 0 ) VDESC3D(J)(I:I) = ' '
                END DO
            END DO

            DO J = 1, MXDESC3
            DO I = 1, MXDLEN3
                K = ICHAR( FDESC3D(J)(I:I) )
                IF ( K .EQ. 0 ) FDESC3D(J)(I:I) = ' '
                K =ICHAR( UPDSC3D(J)(I:I) )
                IF ( K .EQ. 0 ) UPDSC3D(J)(I:I) = ' '
            END DO
            END DO

        END IF


        CALL PUT( BLANK )
        CALL PUT( BAR )
        CALL PUT( BAR )
        CALL PUT( BAR16 //'  I/O API Header Information  '// BAR16 )
        CALL PUT( BLANK )
        CALL PUT( 'File: '//ENAME )
        CALL PUT( BLANK )
        CALL PUT( 'File type:  ' // FTYPES( FTYPE3D ) )
        CALL PUT( BLANK  )

        CALL PUT( BAR16 // '  Variables ' // BAR16 // BAR16 // BAR16 )
        CALL PUT(
     &'#    |   Name         |      Units     |Type|'//
     &'            Description' )
        CALL PUT(
     &'-----|----------------|----------------|----|'//
     *'------------------------------------' )
        do i=1,nvars3d
            OUTLIN = BLANK
            WRITE(OUTLIN,'(A, I4)') '|', I
            OUTLIN(6:) =
     &       '|' // VNAME3D(I) // '|' // UNITS3D(I) // '|' //
     &       VTYPES( VTYPE3D( I ) ) // '|' // VDESC3D(I)
            CALL PUT( OUTLIN )
        END DO

        CALL PUT( BLANK )
        CALL PUT( BAR16 // '  DATE&TIME Info ' // BAR16 )
        CALL PUT( 'Last program writing to file:  '//upnam3d )
        WRITE(OUTLIN, '( A, I9.7, A, I6.6, 2X, 3A )' )
     &        'File-creation date/time:  ', cdate3d, ':', ctime3d,
     &        '(', DT2STR ( CDATE3D , CTIME3D ), ')'
        CALL PUT( OUTLIN  )
        CALL PUT( BLANK )

        IF ( TSTEP3D .EQ. 0 ) THEN
            CALL PUT( 'TIME-INDEPENDENT Data' )
        ELSE
            WRITE( OUTLIN,'( A, I9.7, A, I6.6, 2X, 3A )')
     &            'Initial date/time:  ', sdate3d, ':', stime3d,
     &            '(', DT2STR ( SDATE3D , STIME3D ), ')'
            CALL PUT( OUTLIN  )
            CALL PUT( 'Time step '// HHMMSS( TSTEP3D )// ' (hh:mm:ss)' )
        END IF
        WRITE(OUTLIN,*) 'Maximum time step record number:  ', MXREC3D
        CALL PUT( OUTLIN  )
        CALL PUT( BLANK )

        CALL PUT( BAR16 // '  Horizontal Coordinates/Grid  '// BAR16 )
        CALL PUT( 'Coordinate Projection type:  '//GTYPES( GDTYP3D ) )
        WRITE(OUTLIN,*) 'Cartesian center:  ',xcent3d,ycent3d
        CALL PUT( OUTLIN  )

        IF ( GDTYP3D .EQ. LATGRD3) THEN
            CALL PUT( 'Coordinate units are degrees' )
        ELSE IF ( GDTYP3D .EQ. LAMGRD3) THEN
            WRITE(OUTLIN,*) 'Secant latitudes ',p_alp3d,p_bet3d
            CALL PUT( OUTLIN  )
            WRITE(OUTLIN,*) 'Central meridian ',p_gam3d
            CALL PUT( OUTLIN  )
            CALL PUT( 'Coordinate units are meters' )
        ELSE IF ( GDTYP3D .EQ. MERGRD3) THEN
            WRITE(OUTLIN,*) 'Center lat-lon ',p_alp3d,p_bet3d,
     &       'and cyl-N. Pole angle ',p_gam3d
            CALL PUT( OUTLIN  )
            CALL PUT( 'Coordinate units are meters' )
        ELSE IF ( GDTYP3D .EQ. STEGRD3) THEN
            WRITE(OUTLIN,*) 'Tangent lat-lon ',p_alp3d,p_bet3d,
     &       'and true north-Y axis angle ',p_gam3d
            CALL PUT( OUTLIN  )
            CALL PUT( 'Coordinate units are meters' )
        ELSE IF ( GDTYP3D .EQ. UTMGRD3) THEN
            WRITE(OUTLIN,*) 'Zone ', NINT( P_ALP3D )
            CALL PUT( OUTLIN  )
            WRITE(OUTLIN,*) 'Offset x,y ',xcent3d,ycent3d
            CALL PUT( OUTLIN  )
            CALL PUT( 'Coordinate units are meters' )
        ELSE IF ( GDTYP3D .EQ. POLGRD3) THEN
            IF ( P_ALP3D.GT.0.0) THEN
                CALL PUT( '(North Polar region)' )
            ELSE
                CALL PUT( '(South Polar region)' )
            END IF
            WRITE(OUTLIN,*) 'Secant latitude (of true scale) ', p_bet3d
            CALL PUT( OUTLIN  )
            WRITE(OUTLIN,*) 'Central meridian ', p_gam3d
            CALL PUT( OUTLIN  )
            WRITE(OUTLIN,*) 'Center lat-lon ',ycent3d,xcent3d
            CALL PUT( OUTLIN  )
            CALL PUT( 'Coordinate units are meters' )
        ELSE IF ( gdtyp3d .EQ. EQMGRD3) THEN
            CALL PUT( 'Coord type:  Equatorial mercator' )
            WRITE(OUTLIN,*) 'Latitude of true scale ',p_bet3d
            CALL PUT( OUTLIN  )
            WRITE(OUTLIN,*) 'Central meridian ',p_gam3d
            CALL PUT( OUTLIN  )
            WRITE(OUTLIN,*) 'Center lat-lon ',ycent3d,xcent3d
            CALL PUT( OUTLIN  )
            CALL PUT( 'Coordinate units are meters' )
        ELSE IF ( gdtyp3d .EQ. TRMGRD3) THEN
           WRITE(OUTLIN,*) 'Latitude of true scale ',p_bet3d
            CALL PUT( OUTLIN  )
           WRITE(OUTLIN,*) 'Central meridian ',p_gam3d
            CALL PUT( OUTLIN  )
           WRITE(OUTLIN,*) 'Center lat-lon ',ycent3d,xcent3d
            CALL PUT( OUTLIN  )
            CALL PUT( 'Coordinate units are meters' )
        END IF

        CALL PUT( BLANK )
        CALL PUT( 'Grid name:  ' // GDNAM3D )
        WRITE(OUTLIN,*) 'Grid dimensions (Nx,Ny,Nz):  ',
     &        ncols3d,nrows3d,nlays3d
        CALL PUT( OUTLIN  )
        WRITE(OUTLIN,*) 'Grid LL Corner:',xorig3d,yorig3d
        CALL PUT( OUTLIN  )
        WRITE(OUTLIN,*) 'X,Y cell size:  ',xcell3d, ',', ycell3d
        CALL PUT( OUTLIN  )
        CALL PUT( BLANK )

        CALL PUT( BAR16 // '  Vertical Coordinates  '// BAR16 )
        CALL PUT( 'Vertical coordinate type:  '// VGTYPES( VGTYP3D ) )
        WRITE(OUTLIN,*) 'Model top:  ', vgtop3d
        CALL PUT( OUTLIN  )

        DO I = 1, NLAYS3D+1
            WRITE(OUTLIN,'( A, I4, A1, F12.6 )' )
     &            'Level', I, ':', VGLVS3D(I)
            CALL PUT( OUTLIN  )
        END DO
        CALL PUT( BLANK  )
        CALL PUT( BAR16//'  General description information  '//BAR16 )
        do i=1,mxdesc3
           IF ( FDESC3D(I) .NE. BLANK ) THEN
               CALL PUT( FDESC3D(I) )
           END IF
        end do
        CALL PUT( BLANK  )
        CALL PUT( BAR16//'  History/scenario information  '// BAR16 )
        do i=1,mxdesc3
           IF ( UPDSC3D(I) .NE. BLANK ) THEN
               CALL PUT( UPDSC3D(I) )
           END IF
        end do
        CALL PUT( BLANK  )

        IF ( ISCMAQ( FNAME ) ) THEN
            IF ( .NOT.GETCMAQ( FNAME ) ) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'Error reading CMAQ metadata' )
            ELSE IF ( .NOT.LOGCMAQ( LDEV, CMAQ_MDATA ) ) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'Error writing CMAQ metadata' )
            END IF
        END IF

        IF ( ISSMOKE( FNAME ) ) THEN
            IF ( .NOT.GETSMOKE( FNAME ) ) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'Error reading SMOKE metadata' )
            ELSE IF ( .NOT.LOGSMOKE( LDEV, SMOKE_MDATA ) ) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'Error writing SMOKE metadata' )
            END IF
        END IF

        CALL PUT( BAR  )


        CALL M3EXIT( PNAME, 0, 0, BLANK, 0 )


      CONTAINS


          SUBROUTINE PUT( LINE )
          IMPLICIT NONE
          CHARACTER(LEN=*), INTENT( IN ) :: LINE

          WRITE(LDEV,'(1X,A)') ADJUSTL( TRIM( LINE ) )

          RETURN
          END SUBROUTINE PUT

      END  PROGRAM M3HDR
