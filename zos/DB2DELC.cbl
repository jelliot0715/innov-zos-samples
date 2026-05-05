       Identification Division.
       Program-Id. DB2DELC.
      *****************************************************************
      * Delete a Free Throw record using DB2
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
           02  CON-Status                     pic x.
               88  First-Time                 value "Y".
               88  Confirm-Deletion           value "C".
           02  FT-Record.
           copy DB2THROW.
           02  Validation-Errors              pic x(79).
       01  CICS-Response-Code                 pic s9(9) binary.
       01  Display-Messages.
           05  Highlight-Control              pic x.
               88  Highlight-Error            value "Y".
           05  MSG-Out                        pic x(79).
           05  MSG-Undefined-PF-Key           pic x(16)
               value 'Undefined PF key'.
           05  MSG-Initial-Prompt.
               10  filler                     pic x(79)
               value "Press PF5 to delete".
           05  MSG-Confirm-Deletion           pic x(79)
               value "Press PF5 again to confirm, PF12 to cancel".
           05  MSG-Record-Deleted             pic x(79)
               value "Record successfully deleted".
           05  MSG-Record-Not-Found           pic x(79)
               value "Record not found for delete".
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
           05  HV-Throw-Count                 pic s9(9) comp.
           05  HV-Player-Count                pic s9(9) comp.
           05  HV-Team-Count                  pic s9(9) comp.
           05  HV-Team-Name                   pic x(100).
           05  HV-Player-Name                 pic x(100).
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.
       Procedure Division.
           perform 7000-Get-Container
           evaluate CICS-Response-Code
               when DFHRESP(NORMAL)
                   if First-Time
                       perform 0000-First-Time
                   else
                       perform 1000-Process-User-Input
                   end-if
               when DFHRESP(CHANNELERR)
               when DFHRESP(CONTAINERERR)
                   perform 9800-Start-Initial-Trans
               when other
                   perform 8100-Container-Error
           end-evaluate
           .
       0000-First-Time.
           move spaces to CON-Status
           move low-values to DB2UMAPO
           perform 4000-Copy-from-Record-to-Map
           move FT-Delete-TransId to TRANIDO
           move MSG-Initial-Prompt to MSGO
           perform 7100-Put-Container
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
               MAP(FT-Delete-Map)
               MAPSET(FT-Delete-Mapset)
               INTO(DB2UMAPI)
               ASIS
           END-EXEC
           .
       1200-Check-Attention-Id-Keys.
           evaluate EIBAID
               when DFHENTER
                   continue
               when DFHPF5
                   if Confirm-Deletion
                       perform 5000-Save-Changes
                       move spaces to CON-Status
                   else
                       perform 6000-Confirm
                       set Confirm-Deletion to true
                   end-if
               when DFHPF12
                   perform 9500-Transfer-to-View
               when other
                   move MSG-Undefined-PF-Key to MSGO
                   perform 7100-Put-Container
                   perform 9100-Display-and-Return
           end-evaluate
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
           perform 5050-Copy-Record-To-Host
           perform 5100-Locate-Player
           if SQLCODE = 0
               EXEC SQL
                   DELETE FROM MATEGC.THROWS
                    WHERE PLAYER_ID = :HV-Player-Id
               END-EXEC
               if SQLCODE = 0
                   move MSG-Record-Deleted to MSGO
                   perform 5200-Cleanup-Parent-Rows
               else
                   if SQLCODE = 100
                       move MSG-Record-Not-Found to MSGO
                   else
                       perform 8300-Sql-Error
                   end-if
               end-if
           else
               if SQLCODE = 100
                   move MSG-Record-Not-Found to MSGO
               else
                   perform 8300-Sql-Error
               end-if
           end-if
           .
       5050-Copy-Record-To-Host.
           initialize DB2-Host-Variables
           move FT-Team-Name in FT-Container-Data to HV-Team-Name
           move FT-Player-Name in FT-Container-Data to HV-Player-Name
           .
       5100-Locate-Player.
           EXEC SQL
               SELECT P.PLAYER_ID,
                      P.TEAM_ID
                 INTO :HV-Player-Id,
                      :HV-Team-Id
                 FROM MATEGC.PLAYERS P,
                      MATEGC.TEAMS   T
                WHERE P.TEAM_ID = T.TEAM_ID
                  AND P.PLAYER_NAME = :HV-Player-Name
                  AND T.TEAM_NAME   = :HV-Team-Name
           END-EXEC
           .
       5200-Cleanup-Parent-Rows.
           EXEC SQL
               SELECT COUNT(*)
                 INTO :HV-Throw-Count
                 FROM MATEGC.THROWS
                WHERE PLAYER_ID = :HV-Player-Id
           END-EXEC
           if SQLCODE = 0 and HV-Throw-Count = 0
               EXEC SQL
                   DELETE FROM MATEGC.PLAYERS
                    WHERE PLAYER_ID = :HV-Player-Id
               END-EXEC
           end-if
           EXEC SQL
               SELECT COUNT(*)
                 INTO :HV-Player-Count
                 FROM MATEGC.PLAYERS
                WHERE TEAM_ID = :HV-Team-Id
           END-EXEC
           if SQLCODE = 0 and HV-Player-Count = 0
               EXEC SQL
                   DELETE FROM MATEGC.TEAMS
                    WHERE TEAM_ID = :HV-Team-Id
               END-EXEC
           end-if
           .
       6000-Confirm.
           perform 4000-Copy-from-Record-to-Map
           move MSG-Confirm-Deletion to MSGO
           .
       7000-Get-Container.
           EXEC CICS GET CONTAINER(FT-Container-Name)
               CHANNEL(FT-Channel-Name)
               INTO(FT-Container-Data)
               FLENGTH(length of FT-Container-Data)
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
           move "DELETE" to SCRTITLO
           move DFHBMASK to TEAMA
           move DFHBMASK to NAMEA
           if Highlight-Error
               move DFHRED to MSGC
               move space to Highlight-Control
           end-if
           EXEC CICS SEND
               MAP(FT-Delete-Map)
               MAPSET(FT-Delete-Mapset)
               FROM(DB2UMAPO)
               ERASE
               FREEKB
           END-EXEC
           EXEC CICS RETURN
               TRANSID(FT-Delete-TransId)
               CHANNEL(FT-Channel-Name)
           END-EXEC
           .
       9500-Transfer-to-View.
           EXEC CICS XCTL
               PROGRAM(FT-View-Program)
           END-EXEC
           .
       9800-Start-Initial-Trans.
           EXEC CICS START
               TRANSID(FT-View-TransId)
               TERMID(EIBTRMID)
           END-EXEC
           EXEC CICS RETURN
           END-EXEC
           .