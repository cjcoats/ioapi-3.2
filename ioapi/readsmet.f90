
LOGICAL FUNCTION READSMET(FDEV  , JDATE , JTIME ,           &
                          NBORD , SBORD , EBORD , WBORD ,   &
                          MAXMET, NMET  , IMET  , RMET )    &
                  RESULT( RDFLAG )

    !***********************************************************************
    ! Version "$Id: readsmet.f90 212 2021-11-10 20:39:53Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !    function body starts at line 118
    !
    !  FUNCTION:
    !
    !    This routine reads one hour's data from a SURMET-type file
    !    opened on unit FDEV.  It handles file indexing and headers
    !    internally, requiring ONE READ PER HOUR.
    !    Returns .TRUE.  iff the read was successful.
    !    If  100 * JDATE + JTIME/1000 <= 0 , sets JDATE,JTIME to
    !    starting date  hour and rewinds the current file.
    !
    !  PRECONDITIONS:
    !    FDEV is the unit number for SURMET-format file already opened
    !    FDEV is in the range 1:100
    !    JDATE:JTIME represents GMT expressed according to EDSS/Models-3
    !    date and time conventions (YYYYDDD,HHMMSS)
    !    Set environment variable SURMET_INT_IDS to TRUE iff station IDs are
    !    WBAN integers (instead of character-string IDs)
    !
    !  REVISION HISTORY:
    !       Adapted 12/95 by CJC from ROM-descended UAM-BEIS2 RDSMET()
    !       for EDSS/Models-3 date and time conventions
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !...........   ARGUMENTS:

    INTEGER, INTENT(IN   ) :: FDEV                  !  unit number for input file
    INTEGER, INTENT(INOUT) :: JDATE , JTIME
    REAL   , INTENT(IN   ) :: NBORD , SBORD , EBORD , WBORD
    INTEGER, INTENT(IN   ) :: MAXMET                !  max number of stations allowed
    INTEGER, INTENT(  OUT) :: NMET                  !  number of stations found
    INTEGER, INTENT(  OUT) :: IMET ( MAXMET )       !  array of station ID's
    REAL   , INTENT(  OUT) :: RMET ( 16 , MAXMET )  !  station data

    !.......   Station data: has the following structure for SURMET1,
    !.......   similar structures for other SURMET files
    !                   RMET (  1, * )  --  latitude
    !                   RMET (  2, * )  --  longitude
    !                   RMET (  3, * )  --  opaque sky cover
    !                   RMET (  4, * )  --  total sky cover
    !                   RMET (  5, * )  --  lowest cloud cover
    !                   RMET (  6, * )  --  lowest cloud height
    !                   RMET (  7, * )  --  2nd lowest cloud cover
    !                   RMET (  8, * )  --  2nd lowest cloud height
    !                   RMET (  9, * )  --  3rd lowest cloud cover
    !                   RMET ( 10, * )  --  3rd lowest cloud height
    !                   RMET ( 11, * )  --  sea level pressure
    !                   RMET ( 12, * )  --  wind direction
    !                   RMET ( 13, * )  --  wind speed
    !                   RMET ( 14, * )  --  temperature
    !                   RMET ( 15, * )  --  dew point
    !                   RMET ( 16, * )  --  station pressure

    !...........   PARAMETERS:

    INTEGER, PARAMETER :: IMAX = 100


    !...........   LOCAL VARIABLES:

    INTEGER         IOST
    INTEGER         ITIME  , KTIME
    INTEGER         YEAR   , MONTH , INDAY , INHR , NSTAT
    INTEGER         DATE
    INTEGER         ISTAT  , N , P
    CHARACTER*240   MESG
    LOGICAL         REWFLAG


    !.......   Input buffer variables:

    CHARACTER*5     CID
    INTEGER         IID
    EQUIVALENCE   ( IID , CID )

    REAL            RTEMP ( 16 )
    REAL            ALAT , ALON

    EQUIVALENCE   ( ALAT , RTEMP ( 1 ) )
    EQUIVALENCE   ( ALON , RTEMP ( 2 ) )


    !...........   STATE VARIABLES:

    !.......   End-times, record counts for the files:

    LOGICAL, SAVE :: FIRSTIME = .TRUE.
    LOGICAL, SAVE :: IFLAG                 !  true iff integer ID's
    INTEGER, SAVE :: ETIME ( IMAX ) = 99999999
    INTEGER, SAVE :: RECCNT( IMAX ) =        0

    !..........................................................................
    !.......   Begin body of routine READSMET:

    IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.
        IFLAG    = ENVYN( 'SURMET_INT_IDS', 'SURMETs use INT WBAN IDs, not CHAR', .TRUE., ISTAT )
        IF ( ISTAT .EQ. -2 ) THEN
            CALL M3EXIT( 'RDSMET', JDATE, JTIME, 'Environment variable SURMET_INT_IDS not set', 2 )
        ELSE IF ( ISTAT .GT. 0 ) THEN
            CALL M3EXIT( 'RDSMET', JDATE, JTIME,        &
                         'Environment variable SURMET_INT_IDS ' // 'has invalid value', 2 )
        END IF
    END IF

    !.......   KTIME is Julian-hour YYDDDHH format

    KTIME =  100 * MOD( JDATE , 100000 ) +  JTIME / 10000
    IF  ( KTIME .LE. 0 ) THEN       !  find starting date  hour

        REWIND( FDEV )
        RECCNT( FDEV ) = 0

    ELSE IF  ( KTIME .GT. ETIME( FDEV ) )  THEN   ! file's ending date&hour
    ! before request
        RDFLAG =  .FALSE.
        RETURN

    END IF

    REWFLAG   =  .FALSE.


    !.............   Read time step header

100 READ ( FDEV , 93001, IOSTAT=IOST )&
               YEAR   ,&
               MONTH  ,&
               INDAY  ,&
               INHR   ,&
               NSTAT

    RECCNT ( FDEV ) = RECCNT ( FDEV ) +  1

    IF ( IOST .GT. 0 ) THEN

        WRITE ( MESG,94010 )            &
            'I/O error', IOST, 'at record', RECCNT( FDEV ), 'in file', FDEV
        CALL M3WARN( 'READSMET', JDATE, JTIME, MESG )
        RDFLAG  =  .FALSE.
        RETURN

    ELSE IF ( IOST .EQ. -1 )  THEN    !  end-of-file for file IF

        IF  ( REWFLAG )  THEN    ! second pass thru file -- still not found

            REWIND( FDEV )
            RECCNT( FDEV )  =  0
            ETIME ( FDEV )  =  ITIME + 1
            RDFLAG  =  .FALSE.

            RETURN

        ELSE                        !  first pass thru file:

            REWIND( FDEV )
            RECCNT( FDEV )   =  0
            REWFLAG  =  .TRUE.
            GO TO  100              ! rewind and re-read from start of file

        END IF               !  REWFLAG or not

    END IF             !  IOST check


    !....... Convert time representations:

    DATE  = 1000 * YEAR   +  JULIAN( YEAR, MONTH, INDAY )
    ITIME =  100 * DATE   +  INHR	!  is Julian-hour YYDDDHH format

    IF  ( KTIME .LE. 0 ) THEN  !  special case:  return starting date  hour

        JDATE    =  DATE
        JTIME    =  10000 * INHR
        RDFLAG =  .TRUE.

        REWIND( FDEV )
        RECCNT( FDEV ) = 0
        RETURN              !  end JDATE = JTIME = 0 case

    ELSE IF  ( ITIME .LT. KTIME ) THEN

    !.........   Read (and skip) all stations for this file and hour --
    !...........   Case of dates  hours in file before requested date  hour

        DO 200 N = 1, NSTAT

            READ ( FDEV , 93002, IOSTAT = IOST )   CID
            RECCNT( FDEV )  =  RECCNT( FDEV )  +  1

            IF  ( IOST .GT. 0 )  THEN

                WRITE ( MESG,94010 )            &
                    'I/O error', IOST, 'at record', RECCNT( FDEV ), 'in file', FDEV
                CALL M3WARN( 'READSMET', JDATE, JTIME, MESG )
                RDFLAG  =  .FALSE.
                RETURN

            ELSE IF ( IOST .EQ. -1 )  THEN

                WRITE ( MESG,94010 )        &
                    'Unexpected END OF FILE at line', RECCNT ( FDEV ), 'in file', FDEV
                CALL M3WARN( 'READSMET', JDATE, JTIME, MESG )
                ETIME ( FDEV )  =  ITIME + 1
                RECCNT( FDEV )  =  0
                REWIND( FDEV )
                RDFLAG  =  .FALSE.
                RETURN

            END IF           !  ...if nonzero I/O status

200     CONTINUE            !  end loop on this hour's stations

        GO TO  100          !  to read next header


    ELSE IF  ( ITIME .GT. KTIME )  THEN     ! file's date&hour past request

        IF  ( REWFLAG )  THEN    ! second pass thru file -- still not found

            REWIND( FDEV )
            RECCNT( FDEV )  =  0
            RDFLAG  =  .FALSE.
            RETURN

        ELSE                        !  first pass thru file:

            REWIND( FDEV )
            RECCNT( FDEV )   =  0
            REWFLAG  =  .TRUE.
            GO TO  100              ! rewind and re-read from start of file

        END IF               !  REWFLAG or not


    ELSE            !  ready to read data for the correct date  hour:


        ISTAT  =  0

        DO  399  N = 1 , NSTAT      !  loop on this hour's stations

            IF  ( IFLAG ) THEN

                READ ( FDEV , 93004 , IOSTAT = IOST ) IID , ( RTEMP ( P ) , P = 1 , 16 )

            ELSE

                READ ( FDEV , 93003, IOSTAT = IOST ) CID , ( RTEMP ( P ) , P = 1 , 16 )

            END IF

            RECCNT( FDEV ) =  RECCNT( FDEV )  +  1

            IF  ( IOST .GT. 0 )  THEN

                WRITE ( MESG,94010 )                &
                    'I/O error', IOST,              &
                    'at record', RECCNT( FDEV ),    &
                    'in file', FDEV
                CALL M3WARN( 'READSMET', JDATE, JTIME, MESG )
                GO TO  900

            ELSE IF ( IOST .EQ. -1 )  THEN   !  end-of-file; no record read

                WRITE ( MESG,94010 )    &
                    'Unexpected END OF FILE at line', RECCNT ( FDEV ), 'in file', FDEV
                CALL M3WARN( 'READSMET', JDATE, JTIME, MESG )
                ETIME ( FDEV )  =  ITIME + 1
                RECCNT( FDEV )  =  0
                REWIND( FDEV )
                GO TO  900

            END IF          !  if nonzero I/O status


            ALON = -ALON	!  convert from "deg W" to signed degrees

            IF  ( ALAT .GE. SBORD  .AND.        &
                  ALAT .LE. NBORD  .AND.        &
                  ALON .LE. EBORD  .AND.        &
                  ALON .GE. WBORD       )  THEN

                IF ( ISTAT .LT. MAXMET )  THEN

                    ISTAT  =  ISTAT + 1

                    IMET( ISTAT )  =  IID

                    DO  300  P = 1 ,16
                        RMET ( P , ISTAT )  =  RTEMP ( P )
300                 CONTINUE

                ELSE IF ( IFLAG ) THEN     !  buffer full  IFLAG

                    WRITE( MESG,94030 )                         &
                       'Buffer overflow; station ', IID,        &
                       'at lat-lon', ALAT, ALON, 'omitted',     &
                       'in file', FDEV
                    CALL M3MESG( MESG )

                ELSE                       !  buffer full  NOT IFLAG

                    WRITE( MESG,94031 )                         &
                       'Buffer overflow; station ', CID,        &
                       'at lat-lon', ALAT, ALON, 'omitted',     &
                       'in file', FDEV
                    CALL M3MESG( MESG )

                END IF      !  ISTAT < MAXMET or not

            END IF          !  XLON,YLAT inside the window or not

399     CONTINUE    !  end loop on this hour's stations


    END IF           !  ITIME < KTIME  or ITIME > KTIME  or  ITIME = KTIME


900 NMET   =  ISTAT


    RDFLAG = ( ISTAT .GT. 0 )

    RETURN


    !*************************  FORMAT STATEMENTS  **************************

    !...........   Formatted file I/O formats............ 93xxx

93001 FORMAT ( 4( I2 ) , 5X , I3 )

93002 FORMAT ( A4 )

93003 FORMAT ( A5  ,  F6.2 ,  F7.2 ,  F5.0 ,  F4.0 ,    &
           3 ( F5.0 , F6.0 ) ,                          &
           F8.1 ,  4(F7.1) ,  F8.1)

93004 FORMAT ( I5.5,  F6.2 ,  F7.2 ,  F5.0 ,  F4.0 ,    &
           3 ( F5.0 , F6.0 ) ,  F8.1 ,  4(F7.1) , F8.1)


    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( A, :, I5, :, 2X )

94020 FORMAT( A, :, I9.7, ':', I6.6, :, 2X )

94030 FORMAT( A, I6,    2X, A, F7.2, ':', F7.2, 2X, A, I5 )

94031 FORMAT( A, 2X, A, 2X, A, F7.2, ':', F7.2, 2X, A, I5 )

END FUNCTION READSMET

