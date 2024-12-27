'$INCLUDE: 'FDA72-G.BI'
DIM la(9), ll(10), dens(9)

      DEF SEG = &H40
      ON ERROR GOTO er                       '**************************
      q = SETMEM(-65600)
      dgroup = VARSEG(sid): bsid = VARPTR(sid)
      temppath
      bioserr = 255
      scrlf = CHR$(13) + CHR$(10)
      LSET sid = stemp + "~UNTITLD" + CHR$(0)
      regs.ax = &H3900
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      LSET sid = stemp + "~DOSFORM" + CHR$(0)
      regs.ax = &H3900
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      LSET sid = stemp + "~TRACKAR" + CHR$(0)
      regs.ax = &H3900
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs

      dens(0) = 4: dens(1) = 1: dens(2) = 2: dens(3) = 3: dens(4) = 4
      dens(5) = 0: dens(6) = 1: dens(7) = 2: dens(8) = 3: dens(9) = 4

      FOR i = 0 TO 7
       x = power(i) * 128
       lensec(i) = x
       gpltab(i) = x + 62
      NEXT i

      FOR i = 0 TO 1
       FOR j = 0 TO 4
        SELECT CASE j
         CASE 0: dr = 2: a = 6
         CASE 1: dr = 2: a = 5
         CASE 2: dr = 1: a = 5
         CASE 3: dr = 0: a = 6
         CASE 4: dr = 0: a = 5
        END SELECT
        nomcalc dr, a, qntl, qndrt
        qcaltim(i, j) = qndrt
        qcalabs(i, j) = qntl
      NEXT j, i
      ctdop(0) = 12: ctabtkid(0) = -4: ctabtcrc(0) = -4: ctbdmpos(0) = 55
      ctdop(1) = 8: ctabtkid(1) = -3: ctabtcrc(1) = -3: ctbdmpos(1) = 55
      ctdop(2) = 6: ctabtkid(2) = -2: ctabtcrc(2) = -2: ctbdmpos(2) = 55
      ctchid = 2: ctlssf = 1: ctenao = 1: ctenww = 0
      nobug = 0
      ctmingpl = 8
      doptbl(0) = 20: doptbl(1) = 24: doptbl(2) = 29: doptbl(3) = 40: doptbl(4) = 48
      dboxflg = -1: oboxflg = -1
      srepcrc = "x.?"
      spath = runspec("C:\FDA72\")
      regs.ax = &H1600
      interruptx &H2F, regs, regs
      x = e121(regs.ax)
      IF x AND x <> 128 GOTO w386
      regs.ax = &H800
      regs.bx = 0
      regs.dx = 0
      interruptx &H13, regs, regs
      IF regs.flags AND 1 GOTO ncfd
      drvatyp = e121(regs.bx)
      regs.ax = &H800
      regs.bx = 0
      regs.dx = 1
      interruptx &H13, regs, regs
      IF regs.flags AND 1 GOTO ncfd
      drvbtyp = e121(regs.bx)
      IF drvatyp = 0 GOTO ncfd

      IF DIR$(spath + "FDA.INI") <> "" THEN
       OPEN spath + "FDA.INI" FOR INPUT AS #1
       IF xe GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 1
        FOR j = 0 TO 4
         LINE INPUT #1, v: IF xe THEN EXIT FOR
         x = INSTR(v, "."): IF x = 0 GOTO wrif
         q = VAL(MID$(v, 1, x - 1))
         b = VAL(MID$(v, x + 1))
         qcaltim(i, j) = q * 16& + b
        NEXT j
        IF xe THEN EXIT FOR
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 1
        FOR j = 0 TO 4
         LINE INPUT #1, v: IF xe THEN EXIT FOR
         x = INSTR(v, "."): IF x = 0 GOTO wrif
         y = INSTR(x + 2, v, "."): IF x = 0 GOTO wrif
         a = VAL(MID$(v, 1, x - 1))
         b = VAL(MID$(v, x + 1, 1))
         c = VAL(MID$(v, y + 1))
         qcalabs(i, j) = a * 256& + b * 32& + c
        NEXT j
        IF xe THEN EXIT FOR
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 1
        FOR j = 0 TO 4
         LINE INPUT #1, v: IF xe THEN EXIT FOR
         x = INSTR(v, "."): IF x = 0 GOTO wrif
         y = INSTR(x + 2, v, "."): IF x = 0 GOTO wrif
         a = VAL(MID$(v, 1, x - 1))
         b = VAL(MID$(v, x + 1, 1))
         c = VAL(MID$(v, y + 1))
         dcalfwd(i, j) = a * 256& + b * 32& + c
        NEXT j
        IF xe THEN EXIT FOR
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 1
        FOR j = 0 TO 4
         LINE INPUT #1, v: IF xe THEN EXIT FOR
         dcalpos(i, j) = VAL(v)
        NEXT j
        IF xe THEN EXIT FOR
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 1
        FOR j = 0 TO 4
         LINE INPUT #1, v: IF xe THEN EXIT FOR
         dcalposid(i, j) = VAL(v)
        NEXT j
        IF xe THEN EXIT FOR
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 2
        LINE INPUT #1, v: IF xe THEN EXIT FOR
        ctdop(i) = VAL(v)
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 2
        LINE INPUT #1, v: IF xe THEN EXIT FOR
        ctabtkid(i) = VAL(v)
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 2
        LINE INPUT #1, v: IF xe THEN EXIT FOR
        ctabtcrc(i) = VAL(v)
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 2
        LINE INPUT #1, v: IF xe THEN EXIT FOR
        ctbdmpos(i) = VAL(v)
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       ctmingpl = VAL(v)
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       FOR i = 0 TO 4
        LINE INPUT #1, v: IF xe THEN EXIT FOR
        doptbl(i) = VAL(v)
       NEXT i
       IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       s = UCASE$(LTRIM$(RTRIM$(v)))
       SELECT CASE s
        CASE "NONE": ctchid = 0
        CASE "FIRST": ctchid = 1
        CASE "ALL": ctchid = 2
        CASE ELSE: GOTO wrif
       END SELECT
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       s = UCASE$(LTRIM$(RTRIM$(v)))
       SELECT CASE s
        CASE "IGNORE": ctlssf = 0
        CASE "MOVE": ctlssf = 1
        CASE ELSE: GOTO wrif
       END SELECT
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       s = UCASE$(LTRIM$(RTRIM$(v)))
       SELECT CASE s
        CASE "NOCOMP": ctenao = 0
        CASE "COMP": ctenao = 1
        CASE ELSE: GOTO wrif
       END SELECT
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       s = UCASE$(LTRIM$(RTRIM$(v)))
       SELECT CASE s
        CASE "NOWARN": ctenww = 0
        CASE "WARN": ctenww = 1
        CASE ELSE: GOTO wrif
       END SELECT
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       s = UCASE$(LTRIM$(RTRIM$(v)))
       SELECT CASE s
        CASE "YES": nobug = 0
        CASE "NO": nobug = 1
        CASE ELSE: GOTO wrif
       END SELECT
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       stview = UCASE$(v)
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       stedit = UCASE$(v)
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       sbview = UCASE$(v)
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       s = UCASE$(LTRIM$(RTRIM$(v)))
       SELECT CASE s
        CASE "COLOR1": dboxflg = -1
        CASE "COLOR2": dboxflg = -2
        CASE ELSE: GOTO wrif
       END SELECT
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       s = UCASE$(LTRIM$(RTRIM$(v)))
       SELECT CASE s
        CASE "COLOR1": oboxflg = -1
        CASE "COLOR2": oboxflg = -2
        CASE ELSE: GOTO wrif
       END SELECT
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       LINE INPUT #1, v: IF xe THEN xe = 0: GOTO iff
       IF LEN(v) <> 3 GOTO wrif
       srepcrc = v
iff:   CLOSE #1
      END IF
      IF DIR$(spath + "FDA.DFD") <> "" THEN dfdflg = 1 ELSE dfdflg = 0
      IF xe THEN xe = 0: dfdflg = 0
      IF DIR$(spath + "FDA.HLP") <> "" THEN hlpflg = 1 ELSE hlpflg = 0
      IF xe THEN xe = 0: hlpflg = 0
      drive = 0: ncyl = 42: dfrd = 1: dfnos = 9: otrd = 1: diskattr = -1: dcrd = 1
      GOSUB ckmi
rst:  minit: curoff
      'setcol 0, 9, &H2A
      'setcol 0, 23, &H1B
      'setcol 0, 25, &H17
      'setcol 0, 33, &HB0
      'dboxflg = -2
      FOR i = 0 TO 23 STEP 3
       fprint i, 0, STRING$(80 * 3, 177), 2
      NEXT i
      mkwind 0, 0, 1, 80, 3: mkwind 24, 0, 1, 80, 3
      fprint 0, 70, "F1=Help", 3: fprint 24, 66, "³ FDA 7.2 Pro", 3
      blin
      enerr = 1
      IF men2flg GOTO top1
menu: moff
      wincre 3, 4, 18, 72, "Floppy Disk Analyser", dboxflg
      mkbord1 5, 42, 6, 31, 7: fprint 5, 48, " Drive A: Summary ", 19
      mkbord1 11, 42, 6, 31, 7: fprint 11, 48, " Drive B: Summary ", 19
      mkbord1 5, 7, 12, 33, 7: fprint 5, 17, " Procedures ", 19
      fprint 6, 39, CHR$(24), 5
      mkwind 7, 39, 8, 1, 5
      fprint 15, 39, CHR$(25), 5
      mkbut 18, 25, 13, "Proceed", 1, 14, 18
      mkbut 18, 41, 13, "Quit", 0, 16, 18
      FOR i = 0 TO 1
       IF i THEN x = 12: y = drvbtyp ELSE x = 6: y = drvatyp
       IF y THEN
        a = 46
        IF y < 3 THEN s = "5.25" + CHR$(34) + ",": b = 7 ELSE s = "3.5" + CHR$(34) + ",": b = 6
        fprint x, a, s, 7: a = a + b
        SELECT CASE y
         CASE 1: s = "360K,": b = 6
         CASE 2: s = "1.2M,": b = 6
         CASE 3: s = "720K,": b = 6
         CASE 4: s = "1.44M,": b = 7
        END SELECT
        fprint x, a, s, 7: a = a + b
        IF y = 1 OR y = 3 THEN s = "DD," ELSE s = "HD,"
        fprint x, a, s, 7: a = a + 4
        IF y = 2 THEN s = "360RPM" ELSE s = "300RPM"
        fprint x, a, s, 7
        fprint x + 1, 44, "Max Number of Cylinders:", 7
        SELECT CASE y
         CASE 1: s = "42"
         CASE 2: s = "84"
         CASE 3, 4: s = "82"
        END SELECT
        fprint x + 1, 69, s, 7
        fprint x + 2, 46, "Rec. Densities Supported:", 7
        a = 44
        IF y = 2 THEN fprint x + 3, a, "Low,", 7: a = a + 5
        fprint x + 3, a, "Double", 7: a = a + 6
        IF y <> 2 THEN fprint x + 3, a, ", Medium", 7: a = a + 8
        IF y = 2 THEN fprint x + 3, a, ", High", 7
        IF y = 4 THEN fprint x + 3, a, ", Quad", 7
       ELSE
        fprint x, 44, "Not installed", 7
       END IF
      NEXT i
      fprint 6, 8, " Analyse & Read Disk", 7
      fprint 7, 8, " Format & Write Disk", 7
      fprint 8, 8, " Track Operations", 7
      fprint 9, 8, " View Disk Report File", 7
      fprint 10, 8, " View Disk Data File", 7
      fprint 11, 8, " Viewer & Editor Command Lines", 7
      fprint 12, 8, " Open Operating Directory", 7
      fprint 13, 8, " Prepare for DOS Format", 7
      fprint 14, 8, " Calibrate", 7
      fprint 15, 8, " Save Settings", 7
men2: moff: menuflg = 0: men2flg = 0
      FOR i = 0 TO 9
       IF la(i) THEN chattr i + 6, 8, 31, 7 ELSE chattr i + 6, 8, 31, 11
      NEXT i
      IF proflg = 0 THEN
       wincre 7, 12, 10, 55, "", dboxflg
       fprint 9, 21, "*** Floppy Disk Analyser 7.2 Pro ***", 7
       fprint 11, 15, "Written by Michael and Alex Sokolov, MTC Medincom", 7
       fprint 12, 15, "E-mail: msokolov@ivan.Harhan.ORG", 7
       mkbut 14, 35, 10, "Ok", 1, 12, 18
       mon
prow1: f = 0: WHILE f = 0: mkey f, a, b, bs: WEND
       IF f = 1 THEN
        IF a = 13 OR a = 27 GOTO prow2
       ELSE
        IF a = 14 AND b > 34 AND b < 46 GOTO prow2
       END IF
       GOTO prow1
prow2: moff: winoff
       proflg = 1
      END IF
      mon
m1:   x = msel(6, 8, 10, 31, 10, 1, la(), 1, 7, 8, 9, cl)
      IF x < 3 GOTO m2
      IF x = 3 GOTO m1
      IF x = 4 GOTO hlp1
      IF x = 5 GOTO pr
      IF mpr = 18 AND mpc > 24 AND mpc < 39 GOTO pr
      IF mpr = 18 AND mpc > 40 AND mpc < 55 GOTO qu
      IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO hlp1
      GOTO m1
m2:   x = buttons(18, 25, 13, 2, 12, 14, 16)
      IF x < 2 GOTO m1
      IF x = 2 GOTO m2
      IF x = 3 GOTO hlp1
      IF x = 5 GOTO pr
      IF x = 6 GOTO qu
      IF mpr > 4 AND mpr < 17 AND mpc > 6 AND mpc < 40 THEN
       moff
       IF mpr <> 5 AND mpr <> 16 THEN
        IF la(mpr - 6) THEN
         chattr 6 + cl, 8, 31, 7
         cl = mpr - 6
        END IF
       END IF
       GOTO m1
      END IF
      IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO hlp1
      GOTO m2
pr:   ON cl + 1 GOTO rdid, wdat, trop, vrpt, vdat, vecl, oopf, pfdf, dcal, svcs
qu:   moff: closedisk: rvideos: enerr = 0
abtm: IF DIR$(stemp + "~UNTITLD\*.*") <> "" THEN KILL stemp + "~UNTITLD\*.*"
      LSET sid = stemp + "~UNTITLD" + CHR$(0)
      regs.ax = &H3A00
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF DIR$(stemp + "~DOSFORM\*.*") <> "" THEN KILL stemp + "~DOSFORM\*.*"
      LSET sid = stemp + "~DOSFORM" + CHR$(0)
      regs.ax = &H3A00
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF DIR$(stemp + "~TRACKAR\*.*") <> "" THEN KILL stemp + "~TRACKAR\*.*"
      LSET sid = stemp + "~TRACKAR" + CHR$(0)
      regs.ax = &H3A00
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF DIR$(stemp + "~COMPRES.RPT") <> "" THEN KILL stemp + "~COMPRES.RPT"
      END
hlp1: hlp cl: GOTO m1
rdid: x = command(0, 0): GOTO ret
wdat: x = command(1, 1): GOTO ret
trop: l = -1
      FOR i = 0 TO 10
       IF ll(i) THEN l = i: EXIT FOR
      NEXT i
      moff
      winoff
top1: wincre 3, 21, 19, 38, "Track Operations", dboxflg
      mkbord1 5, 24, 13, 32, 7: fprint 5, 34, " Procedures ", 19
      fprint 6, 26, "Decode Track Write Sequence", 7
      fprint 7, 26, "Edit Track Write Sequence", 7
      fprint 8, 26, "Encode Track Write Sequence", 7
      fprint 9, 26, "Make Full Track Image File", 7
      fprint 10, 26, "View Full Track Image File", 7
      fprint 11, 26, "Build Track Write Sequence", 7
      fprint 12, 26, "Compress Track", 7
      fprint 13, 26, "Analyse & Read Track", 7
      fprint 14, 26, "View Track Report File", 7
      fprint 15, 26, "View Track Data File", 7
      fprint 16, 26, "Execute Track Write Sequence", 7
      fprint 6, 55, CHR$(24), 5
      mkwind 7, 55, 9, 1, 5
      fprint 16, 55, CHR$(25), 5
      mkbut 19, 27, 11, "Proceed", 1, 14, 18
      mkbut 19, 41, 11, "Return", 0, 16, 18
top2: moff: menuflg = 0: men2flg = 1
      FOR i = 0 TO 10
       IF ll(i) = 0 THEN chattr i + 6, 25, 30, 11
      NEXT i
      mon
      IF mvflg THEN mvflg = 0: GOTO ctrk
tr1:  x = msel(6, 25, 11, 30, 11, 1, ll(), 1, 7, 8, 9, l)
      IF x = 6 THEN
       IF mpr = 19 AND mpc > 26 AND mpc < 39 GOTO tr3
       IF mpr = 19 AND mpc > 40 AND mpc < 53 GOTO tr4
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO hlp2
      END IF
      ON x + 1 GOTO tr2, tr2, tr2, tr4, hlp2, tr3, tr1
tr2:  x = buttons(19, 27, 11, 2, 12, 14, 16)
      IF x = 4 THEN
       IF mpr > 4 AND mpr < 18 AND mpc > 23 AND mpc < 56 THEN
        IF mpr > 5 AND mpr < 17 AND mpc > 24 AND mpc < 55 THEN
         IF ll(mpr - 6) THEN
          moff
          chattr l + 6, 25, 29, 7
          mon
          l = mpr - 6
         END IF
        END IF
        GOTO tr1
       END IF
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO hlp2
      END IF
      ON x + 1 GOTO tr1, tr1, tr4, hlp2, tr2, tr3, tr4
tr3:  ON l + 1 GOTO dtws, etws, ctws, absr, absv, btws, ctrk, rdtr, vtrf, vtdf, wrtr
tr4:  moff: winoff: GOTO menu
hlp2: hlp l + 10: GOTO tr1
dtws: x = command(2, 10): GOTO ret
etws: x = command(3, 11): GOTO ret
ctws: x = command(4, 12): GOTO ret
absr: x = command(5, 13): GOTO ret
absv: x = command(6, 14): GOTO ret
btws: x = command(7, 15): GOTO ret
ctrk: x = command(8, 16): GOTO ret
rdtr: x = command(9, 17): GOTO ret
vtrf: x = command(10, 18): GOTO ret
vtdf: x = command(11, 19): GOTO ret
wrtr: x = command(12, 20): GOTO ret
vrpt: x = command(13, 3): GOTO ret
vdat: x = command(14, 4): GOTO ret
vecl: x = command(15, 5): GOTO ret
oopf: x = command(16, 6): GOTO ret
pfdf: x = command(17, 7): GOTO ret
dcal: x = command(18, 8): GOTO ret
svcs: x = command(19, 9): GOTO ret
svs1: OPEN spath + "FDA.INI" FOR OUTPUT AS #1
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Disk revolution times (parameters of calibration):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 1
       FOR j = 0 TO 4
        PRINT #1, LTRIM$(STR$(qcaltim(i, j) \ 16)) + "." + LTRIM$(STR$(qcaltim(i, j) AND 15)): IF xe THEN EXIT FOR
       NEXT j
       IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Track lengths for FDC (parameters of calibration):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 1
       FOR j = 0 TO 4
        q = qcalabs(i, j)
        PRINT #1, LTRIM$(STR$(q \ 256)) + "." + CHR$((q \ 32 AND 7) + 48) + "." + LTRIM$(STR$(q AND 31)): IF xe THEN EXIT FOR
       NEXT j
       IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Abort time adjustments (parameters of calibration):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 1
       FOR j = 0 TO 4
        q = dcalfwd(i, j)
        PRINT #1, LTRIM$(STR$(q \ 256)) + "." + CHR$((q \ 32 AND 7) + 48) + "." + LTRIM$(STR$(q AND 31)): IF xe THEN EXIT FOR
       NEXT j
       IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Positioning time adjustments for Read Sector (parameters of calibration):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 1
       FOR j = 0 TO 4
        PRINT #1, LTRIM$(STR$(dcalpos(i, j)))
       NEXT j
       IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Positioning time adjustments for Read ID (parameters of calibration):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 1
       FOR j = 0 TO 4
        PRINT #1, LTRIM$(STR$(dcalposid(i, j)))
       NEXT j
       IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Maximum permissible dispersions of ID-to-ID distance:"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 2
       PRINT #1, ctdop(i): IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Abort time adjustments (preserve next IDAM from truncation):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 2
       PRINT #1, ctabtkid(i): IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Abort time adjustments (cut off CRC bytes):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 2
       PRINT #1, ctabtcrc(i): IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Offsets from sector beginning to abort position (to destroy DATA AM):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 2
       PRINT #1, ctbdmpos(i): IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Minimum permissible GPL for Compress Track:"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, ctmingpl
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Maximum permissible track length deviations:"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      FOR i = 0 TO 4
       PRINT #1, doptbl(i): IF xe THEN EXIT FOR
      NEXT i
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Switches:"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      SELECT CASE ctchid
       CASE 0: s = "NONE"
       CASE 1: s = "FIRST"
       CASE 2: s = "ALL"
      END SELECT
      PRINT #1, s
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      IF ctlssf THEN s = "MOVE" ELSE s = "IGNORE"
      PRINT #1, s
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      IF ctenao THEN s = "COMP" ELSE s = "NOCOMP"
      PRINT #1, s
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      IF ctenww THEN s = "WARN" ELSE s = "NOWARN"
      PRINT #1, s
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Use FDC chip bug to create MD-sectors:"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      IF nobug THEN s = "NO" ELSE s = "YES"
      PRINT #1, s
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Viewer & editor command lines:"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, stview
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, stedit
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, sbview
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Colors:"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      IF dboxflg = -1 THEN s = "COLOR1" ELSE s = "COLOR2"
      PRINT #1, s
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      IF oboxflg = -1 THEN s = "COLOR1" ELSE s = "COLOR2"
      PRINT #1, s
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, "*** Characters for CRC field in report file (Ok, Error, No info):"
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      PRINT #1, srepcrc
      IF xe THEN xe = 0: moff: winoff: GOTO menu
      CLOSE #1: moff: winoff: GOTO men2

ret:  GOSUB ckmi
      IF x GOTO svs1
      ON menuflg * 2 + men2flg + 1 GOTO men2, top2, menu, top1, rst, rst
ckmi: ERASE la, ll
      la(0) = 1: la(5) = 1: la(6) = 1: la(8) = 1: la(9) = 1
      IF dfdflg THEN la(7) = 1
      IF stview <> "" OR stedit <> "" THEN la(3) = 1: ll(8) = 1
      IF stedit <> "" THEN ll(1) = 1
      IF sbview <> "" THEN la(4) = 1: ll(4) = 1: ll(9) = 1
      IF diskattr <> -1 THEN ll(3) = 1: ll(7) = 1: la(1) = 1: ll(10) = 1: ll(0) = 1: ll(2) = 1
      IF idflg THEN ll(5) = 1: ll(6) = 1
      FOR i = 0 TO 10
       IF ll(i) THEN la(2) = 1: EXIT FOR
      NEXT i
      RETURN

wrif: dosprin "Wrong FDA.INI file" + scrlf: GOTO abtm
ncfd: dosprin "At least one AT-compatible floppy disk drive required" + scrlf: GOTO abtm
w386: dosprin "FDA cannot be run under supervision of Microsoft Windows/386" + scrlf: GOTO abtm

er:   CLOSE #1
      CLOSE #2
      CLOSE #3
      CLOSE #4
      IF diskattr = -1 THEN
       closedisk
      END IF
      IF foflg THEN
       regs.ax = &H3E00
       regs.bx = filnum
       interruptx &H21, regs, regs
       foflg = 0
      END IF
      IF htflg THEN qtemp = timerf(1): htflg = 0
      IF boflg THEN dmabdel: boflg = 0
      stopmot
      SELECT CASE ERR
       CASE 24, 25, 27, 57, 68, 70, 71, 72
        serr = "Drive I/O error"
       CASE 53, 64, 75, 76
        serr = "File/path not found"
       CASE 54
        serr = "Access denied"
       CASE 61, 67
        serr = "Disk full"
       CASE 62
        serr = "Unexpected end-of-file"
       CASE 7, 14
        serr = "Out of memory"
       CASE ELSE
        dosprin "Fatal Error" + STR$(ERR) + scrlf: END
        'ERROR ERR
      END SELECT
opcan:
      IF enerr THEN erwind serr
      xe = 1: RESUME NEXT
END

SUB blin
      SELECT CASE diskattr
       CASE -1: s = "No Operating Directory": f = 0
       CASE 0: s = sopdir: f = 1
       CASE 1: s = "<UNTITLED>": f = 1
       CASE 2: s = "<DOS Format>": f = 1
      END SELECT
      IF f THEN s = s + " " + STR$(ncyl) + " Cyl"
      IF idflg THEN s = s + "  DI"
      IF LEN(s) > 64 THEN s = "..." + RIGHT$(s, 61)
      v = STRING$(64, 0): LSET v = s
      fprint 24, 1, v, 3
END SUB

