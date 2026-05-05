       Identification Division.
       Program-Id. DB2VIEWC.
      *****************************************************************
      * View Free Throw statistics - DB2 version
      *****************************************************************
       Data Division.
       Working-Storage Section.
           copy DFHAID.
           copy DFHBMSCA.
           copy DB2VMSD2.
           copy DB2CONST.
       01  Free-Throw-Record.
           copy DB2THROW.
       01  FT-Container-Data.
           05  CON-Page-Number                pic 9(04).
           05  CON-End-of-File-Reached        pic x.
               88  End-of-File-Reached        value 'Y'.
           05  CON-First-Key                  pic x(40).
           05  CON-Last-Key                   pic x(40).
       01  Container-to-Pass.
           05  First-Time-Flag                pic x.
           05  Record-to-Pass                 pic x(77).
           05  filler                         pic x(79).
       01  Pagination-Fields.
           05  PAG-Start-Key.
               10  filler                     pic x(39).
               10  PAG-Key-Bump               pic x.
           05  PAG-Subscript                  pic s9(4) binary.
           05  PAG-End-of-Data                pic x.
               88  End-of-Data                value 'Y'.
           05  Max-Rows-per-Page              pic s9(4) binary
                                              value +3.
       01  CICS-Response-Code                 pic s9(9) binary.
       01  Transaction-Id-to-Return           pic x(4).
       01  Transfer-to-Program                pic x(8).
       01  Display-Messages.
           05  Highlight-Control              pic x.
               88  Highlight-Error            value 'Y'.
           05  MSG-Out                        pic x(79).
           05  MSG-Undefined-PF-Key           pic x(16)
               value 'Undefined PF key'.
           05  MSG-Initial-Prompt             pic x(79)
               value 'Press ENTER to browse records. PF7=Prev PF8=Next'.
           05  MSG-Top-of-File                pic x(11)
               value 'Top of file'.
           05  MSG-No-More-Records            pic x(26)
               value 'No more records to display'.
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
      *****************************************************************
      * DB2 host variables
      *****************************************************************
       01  Db2-Host-Variables.
           05  Hv-Team-Name                   pic x(100).
           05  Hv-Player-Name                 pic x(100).
           05  Hv-Games                       pic s9(9) comp.
           05  Hv-Attempts                    pic s9(9) comp.
           05  Hv-Completed                   pic s9(9) comp.
           05  Hv-Three-Pointers              pic s9(9) comp.
           05  Hv-Pct-Completed               pic s9(3)v9 comp-3.
           05  Hv-Avg-Points                  pic s9(4)v9 comp-3.
           05  Hv-Last-Update                 pic x(8).
       01  Ws-Db2-Control.
           05  Ws-Fetch-Count                 pic 9 value 0.
           05  Ws-Saved-Count                 pic 9 value 0.
       01  Ws-Current-Keys.
           05  Ws-Curr-Team-Name              pic x(100).
           05  Ws-Curr-Player-Name            pic x(100).
       01  Ws-Saved-Rows.
           05  Ws-Saved-Row occurs 3 times.
               10  Ws-Saved-Record            pic x(77).
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.
       Procedure Division.
           EXEC CICS GET CONTAINER(FT-Container-Name)
               CHANNEL(FT-Channel-Name)
               INTO(FT-Container-Data)
               RESP(CICS-Response-Code)
           END-EXEC
           evaluate CICS-Response-Code
               when DFHRESP(NORMAL)
                   perform 1000-Process-User-Input
               when DFHRESP(CHANNELERR)
               when DFHRESP(CONTAINERERR)
                   perform 0000-First-Time
               when other
                   perform 8100-Container-Error
           end-evaluate
           .
       0000-First-Time.
      *****************************************************************
      * First entry into this program in a conversation.
      *****************************************************************
           initialize FT-Container-Data
           move zero to CON-Page-Number
           move "N" to CON-End-of-File-Reached
           move "N" to PAG-End-of-Data
           move low-values to PAG-Start-Key
           perform 2000-Browse-Forward-Fill-Map
           perform 7100-Put-Container
           perform 9100-Display-and-Return
           .
       1000-Process-User-Input.
      *****************************************************************
      * Route control to the appropriate paragraph based on transid.
      *****************************************************************
           perform 1100-Receive-Map
           perform 1200-Check-Attention-Id-Keys
           perform 7100-Put-Container
           perform 9100-Display-and-Return
           .
       1100-Receive-Map.
      *****************************************************************
      * Receive mapped data from the terminal.
      *****************************************************************
           EXEC CICS RECEIVE
               MAP(FT-View-Map)
               MAPSET(FT-View-Mapset)
               INTO(DB2VMAPI)
               ASIS
           END-EXEC
           .
       1200-Check-Attention-Id-Keys.
      *****************************************************************
      * Handle AID keys that trigger special action.
      *****************************************************************
           evaluate EIBAID
               when DFHPF12
                   perform 9900-End-Transaction
               when DFHPF8
                   if End-of-File-Reached
                       move MSG-No-More-Records to MSGO
                   else
                       move CON-Last-Key to PAG-Start-Key
                       perform 2000-Browse-Forward-Fill-Map
                   end-if
               when DFHPF7
                   if CON-Page-Number less than 2
                       move zero to CON-Page-Number
                       move low-values to PAG-Start-Key
                       perform 2000-Browse-Forward-Fill-Map
                   else
                       move CON-First-Key to PAG-Start-Key
                       move "N" to CON-End-of-File-Reached
                       perform 2500-Browse-Backward-Fill-Map
                   end-if
               when DFHENTER
                   perform varying PAG-Subscript from 1 by 1
                           until PAG-Subscript
                               greater than Max-Rows-per-Page
                       evaluate ACTI(PAG-Subscript)
                           when "A"
                               move FT-Add-Program
                                    to Transfer-to-Program
                               perform 9400-Transfer
                           when "C"
                               perform 1300-Copy-Selected-Record
                               move FT-Update-Program
                                    to Transfer-to-Program
                               perform 9400-Transfer
                           when "D"
                               perform 1300-Copy-Selected-Record
                               move FT-Delete-Program
                                    to Transfer-to-Program
                               perform 9400-Transfer
                           when other
                               continue
                       end-evaluate
                   end-perform
               when other
                   move MSG-Undefined-PF-Key to MSGO
           end-evaluate
           .
       1300-Copy-Selected-Record.
      *****************************************************************
      * Copy the selected record fields from the input map to the
      * container to pass to the update or delete program.
      *****************************************************************
           move spaces to Container-to-Pass
           move "Y" to First-Time-Flag
           move TEAMI(PAG-Subscript) to FT-Team-Name
           move NAMEI(PAG-Subscript) to FT-Player-Name
           EXEC CICS BIF DEEDIT
               FIELD(GAMESI(PAG-Subscript))
               LENGTH(length of GAMESI(PAG-Subscript))
           END-EXEC
           move GAMESI(PAG-Subscript) to FT-Games
           EXEC CICS BIF DEEDIT
               FIELD(ATTSI(PAG-Subscript))
               LENGTH(length of ATTSI(PAG-Subscript))
           END-EXEC
           move ATTSI(PAG-Subscript) to FT-Attempts
           EXEC CICS BIF DEEDIT
               FIELD(COMPI(PAG-Subscript))
               LENGTH(length of COMPI(PAG-Subscript))
           END-EXEC
           move COMPI(PAG-Subscript) to FT-Completed
           EXEC CICS BIF DEEDIT
               FIELD(THREEI(PAG-Subscript))
               LENGTH(length of THREEI(PAG-Subscript))
           END-EXEC
           move THREEI(PAG-Subscript) to FT-Three-Pointers
           EXEC CICS BIF DEEDIT
               FIELD(PCTI(PAG-Subscript))
               LENGTH(length of PCTI(PAG-Subscript))
           END-EXEC
           move PCTI(PAG-Subscript) to FT-Pct-Completed
           EXEC CICS BIF DEEDIT
               FIELD(AVGI(PAG-Subscript))
               LENGTH(length of AVGI(PAG-Subscript))
           END-EXEC
           move AVGI(PAG-Subscript) to FT-Avg-Points
           move UPDI(PAG-Subscript)(1:4) to FT-Last-Update(1:4)
           move UPDI(PAG-Subscript)(6:2) to FT-Last-Update(5:2)
           move UPDI(PAG-Subscript)(9:2) to FT-Last-Update(7:2)
           move Free-Throw-Record to Record-to-Pass
           move Max-Rows-per-Page to PAG-Subscript
           .
       2000-Browse-Forward-Fill-Map.
      *****************************************************************
      * Read forward from DB2 and populate the output map.
      *****************************************************************
           perform 2100-Initialize-View-Map
           move zero to Ws-Fetch-Count
           move "N" to PAG-End-of-Data
           if PAG-Start-Key equal low-values
               perform 2210-Select-First-Row
           else
               move PAG-Start-Key to FT-Record-Key
               move FT-Team-Name to Ws-Curr-Team-Name
               move FT-Player-Name to Ws-Curr-Player-Name
               perform 2220-Select-Next-Row
           end-if

           evaluate SQLCODE
               when 0
                   add 1 to CON-Page-Number
                   move 1 to PAG-Subscript
                   perform 2230-Store-Current-Row

                   perform varying PAG-Subscript from 2 by 1
                           until PAG-Subscript
                               greater than Max-Rows-per-Page
                               or End-of-Data
                       perform 2220-Select-Next-Row
                       evaluate SQLCODE
                           when 0
                               perform 2230-Store-Current-Row
                           when 100
                               set End-of-Data to true
                               set End-of-File-Reached to true
                           when other
                               perform 8300-Sql-Error
                       end-evaluate
                   end-perform
               when 100
                   move MSG-No-More-Records to MSGO
               when other
                   perform 8300-Sql-Error
           end-evaluate
           .
       2100-Initialize-View-Map.
      *****************************************************************
      * Clear the dynamic map areas before loading rows.
      *****************************************************************
           move low-values to DB2VMAPO
           move FT-View-TransId to TRANIDO
           move spaces to MSGO
           move spaces to CON-First-Key
           move spaces to CON-Last-Key
           .
       2210-Select-First-Row.
      *****************************************************************
      * Select the first row in logical order.
      *****************************************************************
           EXEC SQL
               SELECT T.TEAM_NAME,
                      P.PLAYER_NAME,
                      H.GAMES,
                      H.ATTEMPTS,
                      H.COMPLETED,
                      H.THREE_POINTERS,
                      H.PCT_COMPLETED,
                      H.AVG_POINTS,
                      H.LAST_UPDATE
                 INTO :Hv-Team-Name,
                      :Hv-Player-Name,
                      :Hv-Games,
                      :Hv-Attempts,
                      :Hv-Completed,
                      :Hv-Three-Pointers,
                      :Hv-Pct-Completed,
                      :Hv-Avg-Points,
                      :Hv-Last-Update
                 FROM MATEGC.TEAMS   T,
                      MATEGC.PLAYERS P,
                      MATEGC.THROWS  H
                WHERE P.TEAM_ID = T.TEAM_ID
                  AND H.PLAYER_ID = P.PLAYER_ID
                ORDER BY T.TEAM_NAME,
                         P.PLAYER_NAME
                FETCH FIRST 1 ROW ONLY
           END-EXEC
           .
       2220-Select-Next-Row.
      *****************************************************************
      * Select the next row after the current key.
      *****************************************************************
           EXEC SQL
               SELECT T.TEAM_NAME,
                      P.PLAYER_NAME,
                      H.GAMES,
                      H.ATTEMPTS,
                      H.COMPLETED,
                      H.THREE_POINTERS,
                      H.PCT_COMPLETED,
                      H.AVG_POINTS,
                      H.LAST_UPDATE
                 INTO :Hv-Team-Name,
                      :Hv-Player-Name,
                      :Hv-Games,
                      :Hv-Attempts,
                      :Hv-Completed,
                      :Hv-Three-Pointers,
                      :Hv-Pct-Completed,
                      :Hv-Avg-Points,
                      :Hv-Last-Update
                 FROM MATEGC.TEAMS   T,
                      MATEGC.PLAYERS P,
                      MATEGC.THROWS  H
                WHERE P.TEAM_ID = T.TEAM_ID
                  AND H.PLAYER_ID = P.PLAYER_ID
                  AND (T.TEAM_NAME > :Ws-Curr-Team-Name
                   OR (T.TEAM_NAME = :Ws-Curr-Team-Name
                   AND  P.PLAYER_NAME > :Ws-Curr-Player-Name))
                ORDER BY T.TEAM_NAME,
                         P.PLAYER_NAME
                FETCH FIRST 1 ROW ONLY
           END-EXEC
           .
       2230-Store-Current-Row.
      *****************************************************************
      * Copy current DB2 row to the working record and map.
      *****************************************************************
           perform 7400-Copy-Host-To-Record
           if PAG-Subscript = 1
               move FT-Record-Key to CON-First-Key
           end-if
           move FT-Record-Key to CON-Last-Key
           move FT-Team-Name to Ws-Curr-Team-Name
           move FT-Player-Name to Ws-Curr-Player-Name
           perform 4000-Copy-from-Record-to-Map
           .
       2500-Browse-Backward-Fill-Map.
      *****************************************************************
      * Read backward from DB2 and populate the output map.
      *****************************************************************
           perform 2100-Initialize-View-Map
           move zero to Ws-Saved-Count
           move "N" to PAG-End-of-Data
           move PAG-Start-Key to FT-Record-Key
           move FT-Team-Name to Ws-Curr-Team-Name
           move FT-Player-Name to Ws-Curr-Player-Name
           perform varying PAG-Subscript from 1 by 1
                   until PAG-Subscript
                       greater than Max-Rows-per-Page
                       or End-of-Data
               perform 2810-Select-Previous-Row
               evaluate SQLCODE
                   when 0
                       add 1 to Ws-Saved-Count
                       perform 7400-Copy-Host-To-Record
                       move Free-Throw-Record
                            to Ws-Saved-Record(Ws-Saved-Count)
                       move FT-Team-Name to Ws-Curr-Team-Name
                       move FT-Player-Name to Ws-Curr-Player-Name
                   when 100
                       set End-of-Data to true
                   when other
                       perform 8300-Sql-Error
               end-evaluate
           end-perform
           if Ws-Saved-Count = 0
               move zero to CON-Page-Number
               move low-values to PAG-Start-Key
               perform 2000-Browse-Forward-Fill-Map
           else
               subtract 1 from CON-Page-Number
               perform 2850-Load-Saved-Rows-To-Map
           end-if
           .
       2810-Select-Previous-Row.
      *****************************************************************
      * Select the previous row before the current key.
      *****************************************************************
           EXEC SQL
               SELECT T.TEAM_NAME,
                      P.PLAYER_NAME,
                      H.GAMES,
                      H.ATTEMPTS,
                      H.COMPLETED,
                      H.THREE_POINTERS,
                      H.PCT_COMPLETED,
                      H.AVG_POINTS,
                      H.LAST_UPDATE
                 INTO :Hv-Team-Name,
                      :Hv-Player-Name,
                      :Hv-Games,
                      :Hv-Attempts,
                      :Hv-Completed,
                      :Hv-Three-Pointers,
                      :Hv-Pct-Completed,
                      :Hv-Avg-Points,
                      :Hv-Last-Update
                 FROM MATEGC.TEAMS   T,
                      MATEGC.PLAYERS P,
                      MATEGC.THROWS  H
                WHERE P.TEAM_ID = T.TEAM_ID
                  AND H.PLAYER_ID = P.PLAYER_ID
                  AND (T.TEAM_NAME < :Ws-Curr-Team-Name
                   OR (T.TEAM_NAME = :Ws-Curr-Team-Name
                   AND  P.PLAYER_NAME < :Ws-Curr-Player-Name))
                ORDER BY T.TEAM_NAME DESC,
                         P.PLAYER_NAME DESC
                FETCH FIRST 1 ROW ONLY
           END-EXEC
           .
       2850-Load-Saved-Rows-To-Map.
      *****************************************************************
      * Reverse the saved rows into map order.
      *****************************************************************
           move spaces to CON-First-Key
           move spaces to CON-Last-Key
           move 1 to PAG-Subscript
           perform varying Ws-Fetch-Count
                   from Ws-Saved-Count by -1
                   until Ws-Fetch-Count less than 1
               move Ws-Saved-Record(Ws-Fetch-Count)
                    to Free-Throw-Record
               if PAG-Subscript = 1
                   move FT-Record-Key to CON-First-Key
               end-if
               move FT-Record-Key to CON-Last-Key
               perform 4000-Copy-from-Record-to-Map
               add 1 to PAG-Subscript
           end-perform
           .
       4000-Copy-from-Record-to-Map.
      *****************************************************************
      * Populate a line in the output map from the current record.
      *****************************************************************
           move FT-Team-Name to TEAMO(PAG-Subscript)
           move FT-Player-Name to NAMEO(PAG-Subscript)
           move FT-Games to GAMESO(PAG-Subscript)
           move FT-Attempts to ATTSO(PAG-Subscript)
           move FT-Completed to COMPO(PAG-Subscript)
           move FT-Three-Pointers to THREEO(PAG-Subscript)
           move FT-Pct-Completed to PCTO(PAG-Subscript)
           move FT-Avg-Points to AVGO(PAG-Subscript)
           move FT-Last-Update to UPDO(PAG-Subscript)
           .
       7100-Put-Container.
      *****************************************************************
      * Copy working storage data to the container.
      *****************************************************************
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
       7400-Copy-Host-To-Record.
      *****************************************************************
      * Copy DB2 host variables to the working record.
      *****************************************************************
           initialize Free-Throw-Record
           move Hv-Team-Name to FT-Team-Name
           move Hv-Player-Name to FT-Player-Name
           move Hv-Games to FT-Games
           move Hv-Attempts to FT-Attempts
           move Hv-Completed to FT-Completed
           move Hv-Three-Pointers to FT-Three-Pointers
           move Hv-Pct-Completed to FT-Pct-Completed
           move Hv-Avg-Points to FT-Avg-Points
           move Hv-Last-Update to FT-Last-Update
           .
       8100-Container-Error.
      *****************************************************************
      * Display response codes after unexpected condition when
      * getting a container.
      *****************************************************************
           move FT-Channel-Name to ERR-Channel-Name
           move FT-Container-Name to ERR-Container-Name
           move EIBRESP to ERR-Container-EIBRESP
           move EIBRESP2 to ERR-Container-EIBRESP2
           move MSG-Container-Error to MSGO
           perform 9100-Display-and-Return
           .
       8300-Sql-Error.
      *****************************************************************
      * Display DB2 SQLCODE on the screen.
      *****************************************************************
           move SQLCODE to ERR-Sqlcode
           move MSG-Sql-Error to MSGO
           set Highlight-Error to true
           perform 9100-Display-and-Return
           .
      *****************************************************************
      * Display the output map and do a pseudoconversational return.
      *****************************************************************
       9100-Display-and-Return.
           move CON-Page-Number to PAGEO
           if Highlight-Error
               move DFHRED to MSGC
               move space to Highlight-Control
           end-if
           if End-of-Data
               move DFHPROTN to HLPPF8A
               if MSGO = spaces
                   move MSG-No-More-Records to MSGO
               end-if
           end-if
           if CON-Page-Number less than 2
               move DFHPROTN to HLPPF7A
               if MSGO = spaces
                   move MSG-Top-of-File to MSGO
               end-if
           end-if
           EXEC CICS SEND
               MAP(FT-View-Map)
               MAPSET(FT-View-Mapset)
               FROM(DB2VMAPO)
               ERASE
               FREEKB
           END-EXEC
           EXEC CICS RETURN
               TRANSID(FT-View-TransId)
               CHANNEL(FT-Channel-Name)
           END-EXEC
           .
       9400-Transfer.
           EXEC CICS PUT CONTAINER(FT-Container-Name)
               CHANNEL(FT-Channel-Name)
               FROM(Container-to-Pass)
               FLENGTH(length of Container-to-Pass)
           END-EXEC
           EXEC CICS XCTL
               PROGRAM(Transfer-to-Program)
               CHANNEL(FT-Channel-Name)
           END-EXEC
           .
       9900-End-Transaction.
           EXEC CICS SEND CONTROL
               ERASE FREEKB
           END-EXEC
           EXEC CICS RETURN END-EXEC
           .