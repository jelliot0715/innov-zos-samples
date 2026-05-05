       Identification Division.
       Program-Id. DB2ADDC.
      *****************************************************************
      * Add a Free Throw record using DB2
      *****************************************************************
       Data Division.
       Working-Storage Section.
           copy DFHAID.
           copy DFHBMSCA.
           copy DB2UMSD.
           copy DB2CONST.
       01  Free-Throw-Record.
           copy DB2THROW.
       01  FT-Container-Data.
           02  FT-First-Time                  pic x.
               88  First-Time                 value "Y".
           02  FT-Record.
           copy DB2THROW.
           02  Validation-Errors              pic x(79).
       01  CICS-Response-Code                 pic s9(9) binary.
       01  Display-Messages.
           05  Highlight-Control              pic x.
               88  Highlight-Error            value 'Y'.
           05  MSG-Out                        pic x(79).
           05  MSG-Undefined-PF-Key           pic x(16)
               value 'Undefined PF key'.
           05  MSG-Initial-Prompt.
               10  filler                     pic x(79)
               value 'Fill in values for the new record to be added'.
           05  MSG-Record-Added               pic x(79)
               value "Record successfully added".
           05  MSG-Player-Exists              pic x(79)
               value "Player already exists for that team".
           05  MSG-Container-Error.
               10  filler                     pic x(14)
               value 'GET CONTAINER('.
               10  ERR-Container-Name         pic x(16).
               10  filler                     pic x(10).
               10  ERR-Channel-Name           pic x(16).
               10  filler                     pic x(2) value ') '.
               10  ERR-Container-EIBRESP      pic 9(8).
               10  filler                     pic x value space.
               10  ERR-Container-EIBRESP2     pic 9(8).
           05  MSG-Sql-Error.
               10  filler                     pic x(15)
                   value 'DB2 ERROR SQL='.
               10  ERR-Sqlcode                pic -9(9).
       01  DB2-Host-Variables.
           05  HV-Team-Id                     pic s9(9) comp.
           05  HV-Player-Id                   pic s9(9) comp.
           05  HV-Throw-Id                    pic s9(9) comp.
           05  HV-Team-Name                   pic x(100).
           05  HV-Player-Name                 pic x(100).
           05  HV-Games                       pic s9(9) comp.
           05  HV-Attempts                    pic s9(9) comp.
           05  HV-Completed                   pic s9(9) comp.
           05  HV-Three-Pointers              pic s9(9) comp.
           05  HV-Pct-Completed               pic s9(3)v9 comp-3.
           05  HV-Avg-Points                  pic s9(4)v9 comp-3.
           05  HV-Last-Update                 pic x(8).
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.
       Procedure Division.
           perform 7000-Get-Container
           evaluate CICS-Response-Code
               when DFHRESP(NORMAL)
                   if First-Time
                       perform 0000-First-Time
                       move spaces to FT-First-Time
                   else
                       perform 1000-Process-User-Input
                   end-if
               when DFHRESP(CHANNELERR)
               when DFHRESP(CONTAINERERR)
                   perform 0000-First-Time
               when other
                   perform 8100-Container-Error
           end-evaluate
           .
       0000-First-Time.
           initialize FT-Container-Data
           move low-values to DB2UMAPO
           move FT-Add-TransId to TRANIDO
           perform 7100-Put-Container
           move MSG-Initial-Prompt to MSGO
           perform 9100-Display-and-Return
           .
       1000-Process-User-Input.
           perform 1100-Receive-Map
           perform 1200-Check-Attention-Id-Keys
           perform 7100-Put-Container
           perform 9100-Display-and-Return
           .
       1100-Receive-Map.
           EXEC CICS RECEIVE
               MAP(FT-Add-Map)
               MAPSET(FT-Add-Mapset)
               INTO(DB2UMAPI)
               ASIS
           END-EXEC
           .
       1200-Check-Attention-Id-Keys.
           evaluate EIBAID
               when DFHENTER
                   perform 2000-Validate-Input
               when DFHPF5
                   perform 5000-Save-Changes
               when DFHPF12
                   perform 9500-Transfer-to-View
               when other
                   move MSG-Undefined-PF-Key to MSGO
                   perform 7100-Put-Container
                   perform 9100-Display-and-Return
           end-evaluate
           .
       2000-Validate-Input.
           if TEAML greater than 0
               move TEAMI to FT-Team-Name in FT-Container-Data
           end-if
           if NAMEL greater than 0
               move NAMEI to FT-Player-Name in FT-Container-Data
           end-if
           if GAMESL greater than 0
               EXEC CICS BIF DEEDIT
                   FIELD(GAMESI)
                   LENGTH(length of GAMESI)
               END-EXEC
               move GAMESI to FT-Games in FT-Container-Data
           end-if
           if ATTSL greater than 0
               EXEC CICS BIF DEEDIT
                   FIELD(ATTSI)
                   LENGTH(length of ATTSI)
               END-EXEC
               move ATTSI to FT-Attempts in FT-Container-Data
           end-if
           if COMPL greater than 0
               EXEC CICS BIF DEEDIT
                   FIELD(COMPI)
                   LENGTH(length of COMPI)
               END-EXEC
               move COMPI to FT-Completed in FT-Container-Data
           end-if
           if THREEL greater than 0
               EXEC CICS BIF DEEDIT
                   FIELD(THREEI)
                   LENGTH(length of THREEI)
               END-EXEC
               move THREEI to FT-Three-Pointers in FT-Container-Data
           end-if
           perform 7100-Put-Container
           EXEC CICS LINK
               PROGRAM(FT-Validation-Program)
               CHANNEL(FT-Channel-Name)
           END-EXEC
           perform 7000-Get-Container
           if Validation-Errors greater than spaces
               move Validation-Errors to MSGO
           end-if
           perform 4000-Copy-from-Record-to-Map
           .
       4000-Copy-from-Record-to-Map.
           move FT-Team-Name in FT-Container-Data to TEAMO
           move FT-Player-Name in FT-Container-Data to NAMEO
           move FT-Games in FT-Container-Data to GAMESO
           move FT-Attempts in FT-Container-Data to ATTSO
           move FT-Completed in FT-Container-Data to COMPO
           move FT-Three-Pointers in FT-Container-Data to THREEO
           move FT-Pct-Completed in FT-Container-Data to PCTO
           move FT-Avg-Points in FT-Container-Data to AVGO
           move FT-Last-Update in FT-Container-Data to UPDO
           .
       5000-Save-Changes.
           perform 4000-Copy-from-Record-to-Map
           if Validation-Errors greater than spaces
               move Validation-Errors to MSGO
           else
               move function CURRENT-DATE
                   to FT-Last-Update in FT-Container-Data
               perform 5050-Copy-Record-To-Host
               perform 5100-Resolve-Team
               if SQLCODE = 0
                   perform 5200-Insert-Player
               end-if
               if SQLCODE = 0
                   perform 5300-Insert-Throw
               end-if
               if SQLCODE = 0
                   move MSG-Record-Added to MSGO
                   move FT-Last-Update in FT-Container-Data to UPDO
               else
                   if SQLCODE not = 0 and SQLCODE not = 100
                       perform 8300-Sql-Error
                   end-if
               end-if
           end-if
           .
       5050-Copy-Record-To-Host.
           initialize DB2-Host-Variables
           move FT-Team-Name in FT-Container-Data to HV-Team-Name
           move FT-Player-Name in FT-Container-Data to HV-Player-Name
           move FT-Games in FT-Container-Data to HV-Games
           move FT-Attempts in FT-Container-Data to HV-Attempts
           move FT-Completed in FT-Container-Data to HV-Completed
           move FT-Three-Pointers in FT-Container-Data to HV-Three-Pointers
           move FT-Pct-Completed in FT-Container-Data to HV-Pct-Completed
           move FT-Avg-Points in FT-Container-Data to HV-Avg-Points
           move FT-Last-Update in FT-Container-Data to HV-Last-Update
           .
       5100-Resolve-Team.
           EXEC SQL
               SELECT TEAM_ID
                 INTO :HV-Team-Id
                 FROM MATEGC.TEAMS
                WHERE TEAM_NAME = :HV-Team-Name
           END-EXEC
           if SQLCODE = 100
               EXEC SQL
                   SELECT TEAM_ID
                     INTO :HV-Team-Id
                     FROM FINAL TABLE
                     (INSERT INTO MATEGC.TEAMS
                             (TEAM_NAME)
                      VALUES (:HV-Team-Name))
               END-EXEC
           end-if
           if SQLCODE not = 0
               perform 8300-Sql-Error
           end-if
           .
       5200-Insert-Player.
           EXEC SQL
               SELECT PLAYER_ID
                 INTO :HV-Player-Id
                 FROM MATEGC.PLAYERS
                WHERE PLAYER_NAME = :HV-Player-Name
                  AND TEAM_ID = :HV-Team-Id
           END-EXEC
           if SQLCODE = 0
               move MSG-Player-Exists to MSGO
               move 100 to SQLCODE
           else
               if SQLCODE = 100
                   EXEC SQL
                       SELECT PLAYER_ID
                         INTO :HV-Player-Id
                         FROM FINAL TABLE
                         (INSERT INTO MATEGC.PLAYERS
                                 (PLAYER_NAME, TEAM_ID)
                          VALUES (:HV-Player-Name, :HV-Team-Id))
                   END-EXEC
               end-if
           end-if
           if SQLCODE not = 0 and SQLCODE not = 100
               perform 8300-Sql-Error
           end-if
           .
       5300-Insert-Throw.
           EXEC SQL
               SELECT THROW_ID
                 INTO :HV-Throw-Id
                 FROM FINAL TABLE
                 (INSERT INTO MATEGC.THROWS
                         (PLAYER_ID,
                          GAMES,
                          ATTEMPTS,
                          COMPLETED,
                          THREE_POINTERS,
                          PCT_COMPLETED,
                          AVG_POINTS,
                          LAST_UPDATE)
                  VALUES (:HV-Player-Id,
                          :HV-Games,
                          :HV-Attempts,
                          :HV-Completed,
                          :HV-Three-Pointers,
                          :HV-Pct-Completed,
                          :HV-Avg-Points,
                          :HV-Last-Update))
           END-EXEC
           if SQLCODE not = 0
               perform 8300-Sql-Error
           end-if
           .
       7000-Get-Container.
           EXEC CICS GET CONTAINER(FT-Container-Name)
               CHANNEL(FT-Channel-Name)
               INTO(FT-Container-Data)
               RESP(CICS-Response-Code)
           END-EXEC
           .
       7100-Put-Container.
           EXEC CICS PUT CONTAINER(FT-Container-Name)
               CHANNEL(FT-Channel-Name)
               FROM(FT-Container-Data)
               FLENGTH(length of FT-Container-Data)
               RESP(CICS-Response-Code)
           END-EXEC
           if CICS-Response-Code equal DFHRESP(NORMAL)
               continue
           else
               perform 8100-Container-Error
           end-if
           .
       8100-Container-Error.
           move FT-Channel-Name to ERR-Channel-Name
           move FT-Container-Name to ERR-Container-Name
           move EIBRESP to ERR-Container-EIBRESP
           move EIBRESP2 to ERR-Container-EIBRESP2
           move MSG-Container-Error to MSGO
           perform 9100-Display-and-Return
           .
       8300-Sql-Error.
           move SQLCODE to ERR-Sqlcode
           move MSG-Sql-Error to MSG-Out
           set Highlight-Error to true
           .
       9100-Display-and-Return.
           move "ADD" to SCRTITLO
           if Highlight-Error
               move DFHRED to MSGC
               move space to Highlight-Control
           end-if
           EXEC CICS SEND
               MAP(FT-Add-Map)
               MAPSET(FT-Add-Mapset)
               FROM(DB2UMAPO)
               ERASE
               FREEKB
           END-EXEC
           EXEC CICS RETURN
               TRANSID(FT-Add-TransId)
               CHANNEL(FT-Channel-Name)
           END-EXEC
           .
       9500-Transfer-to-View.
           EXEC CICS XCTL
               PROGRAM(FT-View-Program)
           END-EXEC
           .