       Identification Division.
       Program-Id. DB2VALC.
      *****************************************************************
      * Validate record field values and calculate computed values
      * for Free Throw Statistics add and update functions.
      *****************************************************************
       Data Division.
       Working-Storage Section.
           copy DB2CONST.
       01  FT-Container-Data.
           02  filler                         pic x.
           02  FT-Container-Record.
           copy DB2THROW.
           02  Validation-Errors              pic x(79).
       01  Error-Message-Work-Area.
           05  filler                         pic x(18)
               value "Missing value(s): ".
           05  Missing-Field-Names            pic x(79).
       01  Delimiter-Value                    pic x(1).
       01  Points-Scored                      pic s9(7) comp-3.
       01  Ws-Error-Message                   pic x(79).
       Procedure Division.
           perform 1000-Initialize
           perform 2000-Check-Required-Fields
           if Validation-Errors equal spaces
               perform 2500-Validate-Numeric-Relationships
           end-if
           if Validation-Errors equal spaces
               perform 3000-Calculate-Statistics
           end-if
           perform 4000-Return-to-Caller
           .
       1000-Initialize.
           EXEC CICS GET CONTAINER(FT-Container-Name)
               CHANNEL(FT-Channel-Name)
               INTO(FT-Container-Data)
           END-EXEC
           move spaces to Validation-Errors
           move spaces to Missing-Field-Names
           move space to Delimiter-Value
           .
       2000-Check-Required-Fields.
           if FT-Team-Name not greater than spaces
               move "Team:Name" to Missing-Field-Names
               move "," to Delimiter-Value
           end-if
           if FT-Player-Name not greater than spaces
               string
                   Missing-Field-Names delimited by space
                   Delimiter-Value delimited by size
                   space delimited by size
                   "Player:Name" delimited by size
                   into Missing-Field-Names
               end-string
               move "," to Delimiter-Value
           end-if
           if Missing-Field-Names greater than spaces
               inspect Missing-Field-Names replacing all ":" by space
               move Error-Message-Work-Area to Validation-Errors
           end-if
           .
       2500-Validate-Numeric-Relationships.
           if FT-Games <= 0
               move "Games must be greater than zero" to Validation-Errors
           else
               if FT-Attempts <= 0
                   move "Attempts must be greater than zero" to Validation-Errors
               else
                   if FT-Completed > FT-Attempts
                       move "Completed cannot exceed Attempts" to Validation-Errors
                   else
                       if FT-Three-Pointers > FT-Completed
                           move "3-Pointers cannot exceed Completed" to Validation-Errors
                       end-if
                   end-if
               end-if
           end-if
           .
       3000-Calculate-Statistics.
           compute FT-Pct-Completed rounded =
               (FT-Completed / FT-Attempts) * 100
           end-compute
           compute Points-Scored =
               ((FT-Completed - FT-Three-Pointers) * 2)
               + (FT-Three-Pointers * 3)
           end-compute
           compute FT-Avg-Points rounded =
               Points-Scored / FT-Games
           end-compute
           .
       4000-Return-to-Caller.
           EXEC CICS PUT CONTAINER(FT-Container-Name)
               CHANNEL(FT-Channel-Name)
               FROM(FT-Container-Data)
           END-EXEC
           EXEC CICS RETURN
           END-EXEC
           .