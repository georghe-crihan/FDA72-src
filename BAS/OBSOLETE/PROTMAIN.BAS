      IF INSTR(COMMAND$, "/M") THEN
       IF DIR$(spath + "FDA.INS") <> "" GOTO almi
       IF xe THEN xe = 0: GOTO derm
       x = keydisk(noc, 0)
       IF x = &HFFFF GOTO derr
       IF x = &HFFFE GOTO outm
       IF x GOTO kder
       IF noc = 0 GOTO exhs
       dosprin "Number of machine-installations left:" + STR$(noc) + scrlf + "Do you wish to proceed (Y/N)?"
       s = doslinp
       IF UCASE$(LEFT$(s, 1)) <> "Y" GOTO abtm
       x = keydisk(noc, 1)
       IF x = &HFFFF GOTO derr
       IF x = &HFFFE GOTO outm
       IF x = 1 GOTO exhs
       IF x = 2 GOTO kdwp
       IF x GOTO kder
       smachins = sminst
       OPEN spath + "FDA.INS" FOR BINARY ACCESS WRITE AS #1
       IF xe THEN xe = 0: GOTO derm
       PUT #1, , smachins
       IF xe THEN xe = 0: GOTO derm
       CLOSE #1
       GOTO abtm
      END IF

      OPEN spath + "FDA.INS" FOR BINARY ACCESS READ AS #1
      IF xe THEN xe = 0: GOTO mire
      IF LOF(1) = 0 THEN CLOSE #1: KILL spath + "FDA.INS": GOTO coff
      IF LOF(1) <> 28 THEN CLOSE #1: GOTO wrmi
      s = STRING$(28, 0)
      GET #1, , s
      IF xe THEN xe = 0: GOTO mire
      CLOSE #1
      smachins = ""
      FOR i = 1 TO 28
       smachins = smachins + MID$(s, i, 1)
      NEXT i
      s = ""

coff: IF smachins = "" THEN
       x = keydisk(noc, 0)
       IF x = &HFFFF GOTO derr
       IF x = &HFFFE GOTO outm
       IF x GOTO kder
      ELSE
       sf = ""
       FOR i = 1 TO 28
        sf = sf + MID$(smachins, i, 1)
       NEXT i
       x = micheck(sf)
       IF x GOTO mier
      END IF
