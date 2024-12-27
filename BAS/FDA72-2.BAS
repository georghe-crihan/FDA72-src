'$INCLUDE: 'FDA72-G.BI'
END

FUNCTION command (BYVAL comnum, BYVAL helpnum) STATIC
      ON LOCAL ERROR GOTO er
      REDIM la(0)
      ON comnum + 1 GOTO rdid, wdat, dtws, etws, ctws, absr, absv, btws, ctrk, rdtr, vtrf, vtdf, wrtr, vrpt, vdat, vecl, oopf, pfdf, dcal, svcs
rdid: menuflg = 1: moff: winoff
      wincre 5, 3, 15, 74, "Analyse & Read Disk", dboxflg
      mkbord1 7, 6, 4, 51, 7: fprint 7, 20, " Drive Characteristics ", 19
      SELECT CASE drvatyp
       CASE 1: s1 = "5.25" + CHR$(34): s2 = "42"
       CASE 2: s1 = "5.25" + CHR$(34): s2 = "84"
       CASE 3, 4: s1 = "3.5" + CHR$(34) + " ": s2 = "82"
      END SELECT
      fprint 8, 8, "Drive A: " + s1 + " Maximum number of cylinders = " + s2, 7
      IF drvbtyp THEN
       SELECT CASE drvbtyp
        CASE 1: s1 = "5.25" + CHR$(34): s2 = "42"
        CASE 2: s1 = "5.25" + CHR$(34): s2 = "84"
        CASE 3, 4: s1 = "3.5" + CHR$(34) + " ": s2 = "82"
       END SELECT
       fprint 9, 8, "Drive B: " + s1 + " Maximum number of cylinders = " + s2, 7
      END IF
      mkbord1 7, 62, 6, 12, 7: fprint 7, 64, " Drive ", 19
      fprint 8, 73, CHR$(24), 5: mkwind 9, 73, 2, 1, 5: fprint 11, 73, CHR$(25), 5
      fprint 8, 68, "A:", 7: IF drvbtyp THEN fprint 9, 68, "B:", 7
      fprint 12, 6, "Number of cylinders: [   ]", 7
      s1 = MID$(STR$(ncyl), 2): v = "   ": LSET v = s1: fprint 12, 28, v, 8
      fprint 12, 36, "[ ] All tracks non-std", 7
      alltrkns = 0
      fprint 14, 6, "Operating directory name:", 7
      IF diskattr THEN s2 = "" ELSE s2 = sopdir
      v = STRING$(68, 0): LSET v = s2: fprint 15, 6, v, 8
      mkbut 17, 28, 10, "Ok", 1, 14, 18
      mkbut 17, 41, 10, "Cancel", 0, 16, 18
rdi1: help = 1: mon: IF drvbtyp = 0 THEN nod = 1 ELSE nod = 2
ri1:  x = msel(8, 63, 4, 10, nod, 0, la(), 0, 7, 8, 9, drive)
      IF x = 6 THEN GOSUB ri: ON f + 1 GOTO ri1, ri1, ri2, ri3, ri4, ri6, ri7, help
      ON x + 1 GOTO ri5, ri2, ri5, ri7, help
ri2:  stred = s1: x = editstr(12, 28, 3, 8): s1 = stred
      IF x = 5 THEN GOSUB ri: ON f + 1 GOTO ri2, ri1, ri2, ri3, ri4, ri6, ri7, help
      ON x + 1 GOTO ri5, ri3, ri1, ri7, help
ri3:  x = checkbox(12, 36, 21, alltrkns, 7, 9)
      IF x = 5 THEN GOSUB ri: ON f + 1 GOTO ri3, ri1, ri2, ri3, ri4, ri6, ri7, help
      ON x + 1 GOTO ri5, ri4, ri2, ri7, help
ri4:  stred = s2: x = editstr(15, 6, 68, 8): s2 = stred
      IF x = 5 THEN GOSUB ri: ON f + 1 GOTO ri4, ri1, ri2, ri3, ri4, ri6, ri7, help
      ON x + 1 GOTO ri5, ri5, ri3, ri7, help
ri5:  x = buttons(17, 28, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB ri: ON f + 1 GOTO ri5, ri1, ri2, ri3, ri4, ri6, ri7, help
      ON x + 1 GOTO ri1, ri4, ri7, help, ri5, ri6, ri7
ri6:  IF s1 = "" THEN SOUND 150, 1: GOTO ri1
      v1 = LEFT$(s1, 1): v2 = RIGHT$(s1, 1)
      IF v1 < "0" OR v1 > "9" OR v2 < "0" OR v2 > "9" THEN SOUND 150, 1: GOTO ri1
      a = VAL(s1): IF a = 0 THEN SOUND 150, 1: GOTO ri1
      closedisk
      blin
      s = s2: GOSUB chkp
      ncyl = a: diskdens = 0: sopdir = s: dattrsv = x
      RANDOMIZE PEEK(&H6C): disknum = RND(1) * 255
      REDIM qidoffset(ncyl - 1, 1)
      REDIM qwrioffset(ncyl - 1, 1), dwrilen(ncyl - 1, 1)
      IF DIR$(sopdir + "DISK.FMT") <> "" THEN KILL sopdir + "DISK.FMT"
      OPEN sopdir + "DISK.FMT" FOR BINARY ACCESS READ WRITE AS #5
      s = "SMA FF06" + CHR$(disknum) + CHR$(ncyl): PUT #5, , s
      qidoff = 10
      moff
      wawind "Initializing Drive..."
      x = init(0)
      IF erflg GOTO er0
      IF x THEN winoff: GOTO rdi1
      s = "Reading Disk": winoff: winoff
      GOSUB opw: GOTO drp
ri7:  moff: winoff: x = 0: GOTO ex
opco: IF aflg THEN
       IF diskattr = -1 THEN closedisk
       winoff
       x = 0: GOTO ex
      END IF
      cyl = ncyl: head = 0: GOSUB aow
      IF obrf THEN
       GOSUB obr
       IF x THEN
        closedisk
        winoff
        x = 0: GOTO ex
       END IF
       diskattr = dattrsv: idflg = 1
       blin
       IF aoerr = 0 THEN ltcyl = 0: lthead = 0
      END IF
      wincre 9, 28, 7, 24, "", dboxflg
      fprint 11, 31, "Operation complete", 7
      mkbut 13, 35, 10, "Ok", 1, 12, 18
      SOUND 600, 2: SOUND 850, 2: SOUND 900, 2
      mon
opc1: f = 0: WHILE f = 0: mkey f, a, b, bs: WEND
      IF f = 1 THEN
       IF a = 13 OR a = 27 GOTO opc2
      ELSE
       IF a = 13 AND b > 34 AND b < 46 GOTO opc2
      END IF
      GOTO opc1
opc2: moff: winoff: winoff: x = 0: GOTO ex
ri:   IF mpr > 6 AND mpr < 13 AND mpc > 61 AND mpc < 74 THEN
       IF mpr > 7 AND mpr < nod + 8 AND mpc > 62 AND mpc < 73 THEN moff: chattr drive + 8, 63, 10, 7: mon: drive = mpr - 8
       f = 1: RETURN
      ELSEIF mpr = 12 AND mpc > 27 AND mpc < 31 THEN f = 2: RETURN
      ELSEIF mpr = 12 AND mpc > 35 AND mpc < 57 THEN
       IF alltrkns THEN alltrkns = 0: s = " " ELSE alltrkns = 1: s = "x"
       moff
       fprint 12, 37, s, 7
       mon
       f = 3: RETURN
      ELSEIF mpr = 15 AND mpc > 5 AND mpc < 74 THEN f = 4: RETURN
      ELSEIF mpr = 17 AND mpc > 27 AND mpc < 39 THEN f = 5: RETURN
      ELSEIF mpr = 17 AND mpc > 40 AND mpc < 52 THEN f = 6: RETURN
      ELSEIF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 7: RETURN
      ELSE f = 0: RETURN
      END IF
opw:  wincre 5, 15, 15, 50, s, oboxflg
      mkbord1 7, 18, 4, 44, 7
      fprint 8, 24, "Drive:", 7: fprint 8, 31, CHR$(65 + drive) + ":", 9
      fprint 9, 20, "Cylinders:", 7: fprint 9, 30, STR$(ncyl), 9
      IF drvtyp = 3 OR drvtyp = 4 THEN s = "3.5" ELSE s = "5.25"
      fprint 8, 43, "Disk size:", 7: fprint 8, 54, s + CHR$(34), 9
      fprint 9, 35, "Recording density:", 7
      mkbord1 12, 18, 4, 44, 7
      fprint 13, 20, "Cylinder:     Head:", 7
      fprint 14, 20, STRING$(25, 176) + "    % completed", 7
      fprint 17, 31, "Press Esc to abort", 7: chattr 17, 37, 3, 9
      xp = -1
      RETURN
aow:  fprint 13, 29, STR$(cyl), 9: fprint 13, 39, STR$(head), 9
      x = (cyl * 2 + head) / (ncyl * 2) * 100
      IF x <> xp THEN
       s = STRING$(4, 32): RSET s = STR$(x)
       fprint 14, 45, s, 9
       FOR i = xp \ 4 + 1 TO x \ 4
        fprint 14, 19 + i, CHR$(219), 7
       NEXT i
       xp = x
      END IF
      SELECT CASE owd
       CASE -2: s = "N/A   "
       CASE -1: s = "      "
       CASE 0: s = "Low   "
       CASE 1: s = "Double"
       CASE 2: s = "Medium"
       CASE 3: s = "High  "
       CASE 4: s = "Quad  "
      END SELECT
      fprint 9, 54, s, 9
      mkey f, a, b, bs
      IF f = 1 AND a = 27 THEN
       stopmot
       wincre 9, 22, 7, 35, "", 2
       fprint 11, 25, "Operation interrupted by user", 7
       mkbut 13, 27, 10, "Abort", 1, 14, 18
       mkbut 13, 40, 10, "Resume", 0, 16, 18
       mon
       DO: mkey f, a, b, bs: LOOP WHILE f: f = 1
aow1:  x = buttons(13, 27, 10, 2, 12, 14, 16)
       ON x + 1 GOTO aow1, aow1, aow3, aow1, aow1, aow2, aow3
aow3:  f = -1
aow2:  moff: winoff
      ELSE
       f = 0
      END IF
      RETURN
chkp: IF s = "" THEN s = stemp + "~UNTITLD\": x = 1 ELSE x = 0
      s = UCASE$(s)
      IF RIGHT$(s, 1) <> "\" AND RIGHT$(s, 1) <> ":" THEN s = s + "\"
      RETURN
wdat: moff: winoff: menuflg = 1
wdt1: s = "Format & Write Disk": GOSUB adl
      IF x THEN x = 0: GOTO ex
      wawind "Initializing Drive..."
      diskdens = 0
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        wrirst
        cdens
      NEXT head, cyl
      x = init(1)
      IF erflg GOTO er0
      IF x THEN winoff: x = 0: GOTO ex
      winoff
      s = "Writing Disk": GOSUB opw: GOTO dwp
adl:  IF drvbtyp = 0 THEN x = 0: moff: RETURN
      wincre 6, 25, 12, 30, s, dboxflg
      mkbord1 8, 28, 6, 12, 7: fprint 8, 30, " Drive ", 19
      fprint 9, 39, CHR$(24), 5: mkwind 10, 39, 2, 1, 5: fprint 12, 39, CHR$(25), 5
      fprint 9, 34, "A:", 7: fprint 10, 34, "B:", 7
      mkbut 15, 28, 10, "Ok", 1, 14, 18
      mkbut 15, 41, 10, "Cancel", 0, 16, 18
      mon
      help = 2
adl1: x = msel(9, 29, 4, 10, 2, 0, la(), 0, 7, 8, 9, drive)
      IF x = 6 THEN
       IF mpr = 15 AND mpc > 27 AND mpc < 39 GOTO adl3
       IF mpr = 15 AND mpc > 40 AND mpc < 52 GOTO adl4
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      ELSE : ON x + 1 GOTO adl2, adl2, adl2, adl4, help
      END IF
      GOTO adl1
adl2: x = buttons(15, 28, 10, 2, 12, 14, 16)
      IF x = 4 THEN
       IF mpr > 7 AND mpr < 14 AND mpc > 27 AND mpc < 39 THEN
        IF mpr > 8 AND mpr < 11 AND mpc > 28 AND mpc < 38 THEN
         chattr 9 + drive, 29, 10, 7
         drive = mpr - 9
        END IF
        GOTO adl1
       END IF
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      ELSE
       ON x + 1 GOTO adl1, adl1, adl4, help, adl2, adl3, adl4
      END IF
      GOTO adl2
adl3: moff: winoff: x = 0: RETURN
adl4: moff: winoff: x = 1: RETURN
dtws: s = "Decode Track Write Sequence": GOSUB ats: IF x THEN x = 0: GOTO ex
      wawind "Decoding Track Write Sequence..."
      OPEN sopdir + "TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2) + ".SEQ" FOR OUTPUT AS #1
      hrp 3
      PRINT #1,
      cyl = ltcyl: head = lthead: wrirst
      dtws
      PRINT #1, "END"
      CLOSE #1
      winoff
      x = 0: GOTO ex
ats:  moff
      wincre 8, 23, 9, 34, s, dboxflg
      fprint 10, 27, "Number of cylinder: [   ]", 7
      s = MID$(STR$(ltcyl), 2): sf = "   ": LSET sf = s: fprint 10, 48, sf, 8
      fprint 12, 29, "Number of head: [  ]", 7
      v = MID$(STR$(lthead), 2): sf = "  ": LSET sf = v: fprint 12, 46, sf, 8
      mkbut 14, 28, 10, "Ok", 1, 14, 18
      mkbut 14, 41, 10, "Cancel", 0, 16, 18
      help = 3: mon
ats1: stred = s: x = editstr(10, 48, 3, 8): s = stred
      IF x = 5 THEN
       IF mpr = 12 AND mpc > 45 AND mpc < 48 GOTO ats2
       IF mpr = 14 AND mpc > 27 AND mpc < 39 GOTO ats4
       IF mpr = 14 AND mpc > 40 AND mpc < 52 GOTO ats5
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      END IF
      ON x + 1 GOTO ats3, ats2, ats3, ats5, help, ats1
ats2: stred = v: x = editstr(12, 46, 2, 8): v = stred
      IF x = 5 THEN
       IF mpr = 10 AND mpc > 47 AND mpc < 51 GOTO ats1
       IF mpr = 14 AND mpc > 27 AND mpc < 39 GOTO ats4
       IF mpr = 14 AND mpc > 40 AND mpc < 52 GOTO ats5
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      END IF
      ON x + 1 GOTO ats3, ats3, ats1, ats5, help, ats2
ats3: x = buttons(14, 28, 10, 2, 12, 14, 16)
      IF x = 4 THEN
       IF mpr = 10 AND mpc > 47 AND mpc < 51 GOTO ats1
       IF mpr = 12 AND mpc > 45 AND mpc < 48 GOTO ats2
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      END IF
      ON x + 1 GOTO ats1, ats2, ats5, help, ats3, ats4, ats5
ats4: IF s = "" OR v = "" THEN SOUND 150, 1: GOTO ats1
      IF LEFT$(s, 1) < "0" OR LEFT$(s, 1) > "9" OR RIGHT$(s, 1) < "0" OR RIGHT$(s, 1) > "9" OR v < "0" OR v > "1" THEN SOUND 150, 1: GOTO ats1
      IF VAL(s) >= ncyl THEN SOUND 150, 1: GOTO ats1
      x = 0: ltcyl = VAL(s): lthead = VAL(v)
ats5: moff: winoff: RETURN
etws: s = "Edit Track Write Sequence": GOSUB ats: IF x THEN x = 0: GOTO ex
      sprog = stedit
      sfile = sopdir + "TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2)
      stype = "SEQ"
      winoff
      GOTO cprg
ctws: s = "Encode Track Write Sequence": GOSUB ats: IF x THEN x = 0: GOTO ex
      wawind "Encoding Track Write Sequence..."
      OPEN sopdir + "TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2) + ".SEQ" FOR INPUT AS #1
      x = ctws(1)
      CLOSE #1
      IF x THEN winoff: x = 0: GOTO ex
      cyl = ltcyl: head = lthead: wriupd
      winoff
      x = 0: GOTO ex
absr: menuflg = 1: moff: winoff
      s = "Make Full Track Image File"
      GOSUB addt
      IF x THEN x = 0: GOTO ex
      wawind "Initializing Drive..."
      diskdens = power(otrd)
      x = init(0)
      IF erflg GOTO er0
      IF x THEN winoff: x = 0: GOTO ex
      winoff
      wawind "Reading Track..."
      GOTO tap
addt: moff
      wincre 7, 5, 11, 70, s, dboxflg
      mkbord1 9, 8, 6, 12, 7: fprint 9, 10, " Drive ", 19
      fprint 10, 19, CHR$(24), 5: mkwind 11, 19, 2, 1, 5: fprint 13, 19, CHR$(25), 5
      fprint 10, 14, "A:", 7: IF drvbtyp THEN fprint 11, 14, "B:", 7
      mkbord1 9, 22, 7, 23, 7: fprint 9, 24, " Recording Density ", 19
      fprint 10, 24, "( ) Low", 7
      fprint 11, 24, "( ) Double", 7
      fprint 12, 24, "( ) Medium", 7
      fprint 13, 24, "( ) High", 7
      fprint 14, 24, "( ) Quad", 7
      fprint otrd + 10, 25, CHR$(7), 7
      fprint 10, 47, "Number of cylinder: [   ]", 7
      s1 = MID$(STR$(ltcyl), 2): v = "   ": LSET v = s1: fprint 10, 68, v, 8
      fprint 12, 47, "Number of head: [  ]", 7
      s2 = MID$(STR$(lthead), 2): v = "  ": LSET v = s2: fprint 12, 64, v, 8
      mkbut 14, 48, 10, "Ok", 1, 14, 18
      mkbut 14, 61, 10, "Cancel", 0, 16, 18
      help = 4: mon: IF drvbtyp = 0 THEN nod = 1 ELSE nod = 2
adt1: x = msel(10, 9, 4, 10, nod, 0, la(), 0, 7, 8, 9, drive)
      IF x = 6 THEN GOSUB adt: ON f + 1 GOTO adt1, adt1, adt2, adt3, adt4, adt6, adt7, help
      ON x + 1 GOTO adt5, adt2, adt5, adt7, help
adt2: x = eggs(10, 24, 5, 19, otrd, 7, 9)
      IF x = 5 THEN GOSUB adt: ON f + 1 GOTO adt2, adt1, adt2, adt3, adt4, adt6, adt7, help
      ON x + 1 GOTO adt5, adt3, adt1, adt7, help
adt3: stred = s1: x = editstr(10, 68, 3, 8): s1 = stred
      IF x = 5 THEN GOSUB adt: ON f + 1 GOTO adt3, adt1, adt2, adt3, adt4, adt6, adt7, help
      ON x + 1 GOTO adt5, adt4, adt2, adt7, help
adt4: stred = s2: x = editstr(12, 64, 2, 8): s2 = stred
      IF x = 5 THEN GOSUB adt: ON f + 1 GOTO adt4, adt1, adt2, adt3, adt4, adt6, adt7, help
      ON x + 1 GOTO adt5, adt5, adt3, adt7, help
adt5: x = buttons(14, 48, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB adt: ON f + 1 GOTO adt5, adt1, adt2, adt3, adt4, adt6, adt7, help
      ON x + 1 GOTO adt1, adt4, adt7, help, adt5, adt6, adt7
adt6: IF s1 = "" OR s2 = "" THEN SOUND 150, 1: GOTO adt1
      IF LEFT$(s1, 1) < "0" OR LEFT$(s1, 1) > "9" OR RIGHT$(s1, 1) < "0" OR RIGHT$(s1, 1) > "9" OR s2 < "0" OR s2 > "1" THEN SOUND 150, 1: GOTO adt1
      IF VAL(s1) > ncyl - 1 THEN SOUND 150, 1: GOTO adt1
      ltcyl = VAL(s1): lthead = VAL(s2)
      x = 0
adt7: moff: winoff: RETURN
adt:  IF mpr > 8 AND mpr < 15 AND mpc > 7 AND mpc < 20 THEN
       IF mpr > 9 AND mpr < 10 + nod AND mpc > 8 AND mpc < 19 THEN moff: chattr drive + 10, 9, 10, 7: mon: drive = mpr - 10
       f = 1: RETURN
      ELSEIF mpr > 8 AND mpr < 16 AND mpc > 21 AND mpc < 45 THEN
       IF mpr > 9 AND mpr < 15 AND mpc > 22 AND mpc < 44 THEN moff: mkwind 10, 25, 5, 1, 7: fprint mpr, 25, CHR$(7), 7: mon: otrd = mpr - 10
       f = 2: RETURN
      ELSEIF mpr = 10 AND mpc > 67 AND mpc < 71 THEN f = 3: RETURN
      ELSEIF mpr = 12 AND mpc > 63 AND mpc < 66 THEN f = 4: RETURN
      ELSEIF mpr = 14 AND mpc > 47 AND mpc < 59 THEN f = 5: RETURN
      ELSEIF mpr = 14 AND mpc > 60 AND mpc < 72 THEN f = 6: RETURN
      ELSEIF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 7: RETURN
      ELSE : f = 0: RETURN
      END IF
absv: moff: winoff: menuflg = 1
      wincre 5, 3, 14, 74, "View Full Track Image File", dboxflg
      fprint 7, 6, "Operating directory name:", 7
      IF diskattr THEN s1 = "" ELSE s1 = sopdir
      v = STRING$(68, 0): LSET v = s1: fprint 8, 6, v, 8
      mkbord1 10, 6, 7, 33, 7: fprint 10, 13, " Recording Density ", 19
      fprint 11, 8, "( ) Low", 7
      fprint 12, 8, "( ) Double", 7
      fprint 13, 8, "( ) Medium", 7
      fprint 14, 8, "( ) High", 7
      fprint 15, 8, "( ) Quad", 7
      fprint otrd + 11, 9, CHR$(7), 7
      fprint 11, 41, "Number of cylinder: [   ]", 7
      s2 = MID$(STR$(ltcyl), 2): v = "   ": LSET v = s2: fprint 11, 62, v, 8
      fprint 13, 41, "Number of head: [  ]", 7
      s3 = MID$(STR$(lthead), 2): v = "  ": LSET v = s3: fprint 13, 58, v, 8
      mkbut 15, 46, 10, "Ok", 1, 14, 18
      mkbut 15, 59, 10, "Cancel", 0, 16, 18
      help = 5: mon
av1:  stred = s1: x = editstr(8, 6, 68, 8): s1 = stred
      IF x = 5 THEN GOSUB av: ON f + 1 GOTO av1, av1, av2, av3, av4, av6, av7, help
      ON x + 1 GOTO av5, av2, av5, av7, help
av2:  x = eggs(11, 8, 5, 29, otrd, 7, 9)
      IF x = 5 THEN GOSUB av: ON f + 1 GOTO av2, av1, av2, av3, av4, av6, av7, help
      ON x + 1 GOTO av5, av3, av1, av7, help
av3:  stred = s2: x = editstr(11, 62, 3, 8): s2 = stred
      IF x = 5 THEN GOSUB av: ON f + 1 GOTO av3, av1, av2, av3, av4, av6, av7, help
      ON x + 1 GOTO av5, av4, av2, av7, help
av4:  stred = s3: x = editstr(13, 58, 2, 8): s3 = stred
      IF x = 5 THEN GOSUB av: ON f + 1 GOTO av4, av1, av2, av3, av4, av6, av7, help
      ON x + 1 GOTO av5, av5, av3, av7, help
av5:  x = buttons(15, 46, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB av: ON f + 1 GOTO av5, av1, av2, av3, av4, av6, av7, help
      ON x + 1 GOTO av1, av4, av7, help, av5, av6, av7
av6:  IF s2 = "" OR s3 = "" THEN SOUND 150, 1: GOTO av1
      IF LEFT$(s2, 1) < "0" OR LEFT$(s2, 1) > "9" OR RIGHT$(s2, 1) < "0" OR RIGHT$(s2, 1) > "9" OR s3 < "0" OR s3 > "1" THEN SOUND 150, 1: GOTO av1
      s = s1: GOSUB chkp
      ltcyl = VAL(s2): lthead = VAL(s3)
      SELECT CASE otrd
       CASE 0: v = "L"
       CASE 1: v = "D"
       CASE 2: v = "M"
       CASE 3: v = "H"
       CASE 4: v = "Q"
      END SELECT
      sprog = sbview
      sfile = s + "TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2) + v
      stype = "ABS"
      moff
      winoff
      GOTO cprg
av7:  moff: winoff: x = 0: GOTO ex
av:   IF mpr = 8 AND mpc > 5 AND mpc < 74 THEN f = 1: RETURN
      IF mpr > 9 AND mpr < 17 AND mpc > 5 AND mpc < 39 THEN
       IF mpr > 10 AND mpr < 16 AND mpc > 6 AND mpc < 38 THEN otrd = mpr - 11: moff: mkwind 11, 9, 5, 1, 7: fprint mpr, 9, CHR$(7), 7: mon
       f = 2: RETURN
      END IF
      IF mpr = 11 AND mpc > 61 AND mpc < 65 THEN f = 3: RETURN
      IF mpr = 13 AND mpc > 57 AND mpc < 60 THEN f = 4: RETURN
      IF mpr = 15 AND mpc > 45 AND mpc < 57 THEN f = 5: RETURN
      IF mpr = 15 AND mpc > 58 AND mpc < 70 THEN f = 6: RETURN
      IF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 7: RETURN
      f = 0: RETURN
btws: moff
      wincre 5, 7, 14, 65, "Build Track Write Sequence", dboxflg
      fprint 7, 10, "Number of cylinder: [   ]", 7
      s1 = LTRIM$(STR$(ltcyl)): v = "   ": LSET v = s1: fprint 7, 31, v, 8
      fprint 9, 10, "Number of head: [  ]", 7
      s2 = LTRIM$(STR$(lthead)): v = "  ": LSET v = s2: fprint 9, 27, v, 8
      mkbut 12, 10, 23, "Set build options", 0, 16, 18
      mkbord1 7, 37, 5, 32, 7: fprint 7, 44, " ID Substitution ", 19
      chid = ctchid
      fprint 8, 39, "( ) None", 7
      fprint 9, 39, "( ) First sector only", 7
      fprint 10, 39, "( ) All sectors", 7
      fprint chid + 8, 40, CHR$(7), 7
      mkbord1 13, 37, 4, 32, 7: fprint 13, 43, " GAP4A Overwriting ", 19
      lssf = ctlssf
      fprint 14, 39, "( ) Ignore", 7
      fprint 15, 39, "( ) Move last sector writing", 7
      fprint lssf + 14, 40, CHR$(7), 7
      mkbut 15, 10, 10, "Ok", 1, 14, 18
      mkbut 15, 23, 10, "Cancel", 0, 16, 18
      help = 12: mon: xf = 0: yf = 0
ob1:  stred = s1
      x = editstr(7, 31, 3, 8)
      IF stred <> s1 THEN
       s1 = stred
       IF yf THEN
        warwind "Number of cylinder was changed; build options are initialized"
       END IF
       xf = 0: yf = 0
      END IF
      IF x = 5 THEN GOSUB ob: ON f + 1 GOTO ob1, ob1, ob2, ob9, ob4, ob5, ob7, ob8, help
      ON x + 1 GOTO ob6, ob2, ob6, ob8, help
ob2:  stred = s2
      x = editstr(9, 27, 2, 8)
      IF stred <> s2 THEN
       s2 = stred
       IF yf THEN
        warwind "Number of head was changed; build options are initialized"
       END IF
       xf = 0: yf = 0
      END IF
      IF x = 5 THEN GOSUB ob: ON f + 1 GOTO ob2, ob1, ob2, ob9, ob4, ob5, ob7, ob8, help
      ON x + 1 GOTO ob6, ob3, ob1, ob8, help
ob3:  moff: chattr 15, 10, 10, 16: fprint 15, 10, " ", 16: fprint 15, 19, " ", 16: mon
      x = buttons(12, 10, 23, 1, 12, 16, 16)
      moff
      fprint 12, 10, " ", 16: fprint 12, 32, " ", 16
      chattr 15, 10, 10, 14: fprint 15, 10, CHR$(16), 14: fprint 15, 19, CHR$(17), 14
      mon
      IF x = 4 THEN GOSUB ob: ON f + 1 GOTO ob3, ob1, ob2, ob9, ob4, ob5, ob7, ob8, help
      ON x + 1 GOTO ob4, ob2, ob8, help, ob3, ob9
ob4:  x = eggs(8, 39, 3, 28, chid, 7, 9)
      IF x = 5 THEN GOSUB ob: ON f + 1 GOTO ob4, ob1, ob2, ob9, ob4, ob5, ob7, ob8, help
      ON x + 1 GOTO ob6, ob5, ob4, ob8, help
ob5:  x = eggs(14, 39, 2, 28, lssf, 7, 9)
      IF x = 5 THEN GOSUB ob: ON f + 1 GOTO ob5, ob1, ob2, ob9, ob4, ob5, ob7, ob8, help
      ON x + 1 GOTO ob6, ob6, ob4, ob8, help
ob6:  x = buttons(15, 10, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB ob: ON f + 1 GOTO ob6, ob1, ob2, ob9, ob4, ob5, ob7, ob8, help
      ON x + 1 GOTO ob1, ob5, ob8, help, ob6, ob7, ob8
ob7:  GOSUB obdt: IF y THEN SOUND 150, 1: GOTO ob1
      IF nos = 0 THEN erwind "Track is non-formatted": GOTO ob1
      moff
      xf = 0: yf = 0
      btws
      mon
      GOTO ob1
ob8:  moff: winoff: x = 0: GOTO ex
ob9:  x = 0: GOSUB obdt: IF y THEN SOUND 150, 1: GOTO ob1
      moff
      GOSUB bop: IF x = 0 THEN yf = 1
      mon
      GOTO ob1
ob:   IF mpr = 7 AND mpc > 30 AND mpc < 34 THEN f = 1: RETURN
      IF mpr = 9 AND mpc > 26 AND mpc < 29 THEN f = 2: RETURN
      IF mpr = 12 AND mpc > 9 AND mpc < 34 THEN f = 3: RETURN
      IF mpr > 6 AND mpr < 12 AND mpc > 36 AND mpc < 69 THEN
       IF mpr > 7 AND mpr < 11 AND mpc > 37 AND mpc < 68 THEN
        moff
        mkwind 8, 40, 3, 1, 7
        chid = mpr - 8: fprint mpr, 40, CHR$(7), 7
        mon
       END IF
       f = 4: RETURN
      END IF
      IF mpr > 12 AND mpr < 17 AND mpc > 36 AND mpc < 69 THEN
       IF mpr > 13 AND mpr < 16 AND mpc > 37 AND mpc < 68 THEN
        moff
        mkwind 14, 40, 2, 1, 7
        lssf = mpr - 14: fprint mpr, 40, CHR$(7), 7
        mon
       END IF
       f = 5: RETURN
      END IF
      IF mpr = 15 AND mpc > 9 AND mpc < 21 THEN f = 6: RETURN
      IF mpr = 15 AND mpc > 22 AND mpc < 34 THEN f = 7: RETURN
      IF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 8: RETURN
      f = 0: RETURN
obdt: IF xf = 0 THEN
       IF s1 = "" OR s2 = "" THEN y = 1: RETURN
       IF LEFT$(s1, 1) < "0" OR LEFT$(s1, 1) > "9" OR RIGHT$(s1, 1) < "0" OR RIGHT$(s1, 1) > "9" OR s2 < "0" OR s2 > "1" THEN y = 1: RETURN
       ltcyl = VAL(s1): lthead = VAL(s2)
       IF ltcyl >= ncyl THEN y = 1: RETURN
       cyl = ltcyl: head = lthead
       fmtrst
       xf = 1
      END IF
      y = 0: RETURN
bop:  wincre 6, 22, 13, 36, "Build Options", dboxflg
      fprint 8, 25, "Gap tolerance: [   ]", 7
      sw1 = LTRIM$(STR$(dop)): v = "   ": LSET v = sw1: fprint 8, 41, v, 8
      fprint 10, 25, "Don't damage next IDAM: [    ]", 7
      sw2 = LTRIM$(STR$(abtkid)): v = "    ": LSET v = sw2: fprint 10, 50, v, 8
      fprint 12, 25, "Cut off CRC bytes: [    ]", 7
      sw3 = LTRIM$(STR$(abtcrc)): v = "    ": LSET v = sw3: fprint 12, 45, v, 8
      fprint 14, 25, "Destroy DATA AM offset: [   ]", 7
      sw4 = LTRIM$(STR$(bdmpos)): v = "   ": LSET v = sw4: fprint 14, 50, v, 8
      mkbut 16, 28, 10, "Ok", 1, 14, 18
      mkbut 16, 41, 10, "Cancel", 0, 16, 18
      bophelp = help: help = 13: mon
bop1: stred = sw1: x = editstr(8, 41, 3, 8): sw1 = stred
      IF x = 5 THEN GOSUB bopm: ON f + 1 GOTO bop1, bop1, bop2, bop3, bop4, bop6, bop7, help
      ON x + 1 GOTO bop5, bop2, bop5, bop7, help
bop2: stred = sw2: x = editstr(10, 50, 4, 8): sw2 = stred
      IF x = 5 THEN GOSUB bopm: ON f + 1 GOTO bop2, bop1, bop2, bop3, bop4, bop6, bop7, help
      ON x + 1 GOTO bop5, bop3, bop1, bop7, help
bop3: stred = sw3: x = editstr(12, 45, 4, 8): sw3 = stred
      IF x = 5 THEN GOSUB bopm: ON f + 1 GOTO bop3, bop1, bop2, bop3, bop4, bop6, bop7, help
      ON x + 1 GOTO bop5, bop4, bop2, bop7, help
bop4: stred = sw4: x = editstr(14, 50, 3, 8): sw4 = stred
      IF x = 5 THEN GOSUB bopm: ON f + 1 GOTO bop4, bop1, bop2, bop3, bop4, bop6, bop7, help
      ON x + 1 GOTO bop5, bop5, bop3, bop7, help
bop5: x = buttons(16, 28, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB bopm: ON f + 1 GOTO bop5, bop1, bop2, bop3, bop4, bop6, bop7, help
      ON x + 1 GOTO bop1, bop4, bop7, help, bop5, bop6, bop7
bop6: IF sw1 = "" OR sw2 = "" OR sw3 = "" OR sw4 = "" THEN SOUND 150, 1: GOTO bop1
      IF LEFT$(sw2, 1) = "-" THEN sw2 = MID$(sw2, 2): x = 1 ELSE x = 0
      IF LEFT$(sw3, 1) = "-" THEN sw3 = MID$(sw3, 2): y = 1 ELSE y = 0
      IF LEN(sw2) > 2 OR LEN(sw3) > 2 THEN SOUND 150, 1: GOTO bop1
      IF LEFT$(sw1, 1) < "0" OR LEFT$(sw1, 1) > "9" OR RIGHT$(sw1, 1) < "0" OR RIGHT$(sw1, 1) > "9" OR LEFT$(sw2, 1) < "0" OR LEFT$(sw2, 1) > "9" OR RIGHT$(sw2, 1) < "0" OR RIGHT$(sw2, 1) > "9" OR LEFT$(sw3, 1) < "0" OR LEFT$(sw3, 1) > "9" OR RIGHT$ _
(sw3, 1) < "0" OR RIGHT$(sw3, 1) > "9" OR LEFT$(sw4, 1) < "0" OR LEFT$(sw4, 1) > "9" OR RIGHT$(sw4, 1) < "0" OR RIGHT$(sw4, 1) > "9" THEN SOUND 150, 1: GOTO bop1
      a = VAL(sw1): b = VAL(sw2): c = VAL(sw3): d = VAL(sw4)
      IF x THEN b = -b
      IF y THEN c = -c
      IF a <> dop OR b <> abtkid OR c <> abtcrc OR d <> bdmpos THEN
       dop = a: abtkid = b: abtcrc = c: bdmpos = d
       moff
       winoff
       help = bophelp
       x = 0: RETURN
      END IF
bop7: moff: winoff: help = bophelp: x = 1: RETURN
bopm: IF mpr = 8 AND mpc > 40 AND mpc < 44 THEN f = 1: RETURN
      IF mpr = 10 AND mpc > 49 AND mpc < 54 THEN f = 2: RETURN
      IF mpr = 12 AND mpc > 44 AND mpc < 49 THEN f = 3: RETURN
      IF mpr = 14 AND mpc > 49 AND mpc < 53 THEN f = 4: RETURN
      IF mpr = 16 AND mpc > 27 AND mpc < 39 THEN f = 5: RETURN
      IF mpr = 16 AND mpc > 40 AND mpc < 52 THEN f = 6: RETURN
      IF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 7: RETURN
      f = 0: RETURN
ctrk: moff
      wincre 5, 3, 14, 74, "Compress Track", dboxflg
      fprint 7, 6, "Number of cylinder: [   ]", 7
      s1 = LTRIM$(STR$(ltcyl)): v = "   ": LSET v = s1: fprint 7, 27, v, 8
      fprint 9, 6, "Number of head: [  ]", 7
      s2 = LTRIM$(STR$(lthead)): v = "  ": LSET v = s2: fprint 9, 23, v, 8
      mkbut 11, 6, 33, "Set build options", 0, 16, 18
      fprint 13, 6, "List sequential numbers of sectors with appropriate commands:", 7
      s3 = sqez: v = STRING$(68, 0): LSET v = s3: fprint 14, 6, v, 8
      mingpl = ctmingpl
      fprint 16, 6, "Minimum GPL: [   ]", 7
      s4 = LTRIM$(STR$(mingpl)): v = "   ": LSET v = s4: fprint 16, 20, v, 8
      mkbord1 7, 42, 5, 32, 7: fprint 7, 49, " ID Substitution ", 19
      chid = ctchid
      fprint 8, 44, "( ) None", 7
      fprint 9, 44, "( ) First sector only", 7
      fprint 10, 44, "( ) All sectors", 7
      fprint chid + 8, 45, CHR$(7), 7
      mkbut 16, 37, 10, "Ok", 1, 14, 18
      mkbut 16, 50, 10, "View", 0, 16, 18
      mkbut 16, 63, 10, "Cancel", 0, 16, 18
      help = 14: mon: xf = 0: yf = 0
sq1:  stred = s1
      x = editstr(7, 27, 3, 8)
      IF stred <> s1 THEN
       s1 = stred
       IF yf THEN
        warwind "Number of cylinder was changed; build options are initialized"
       END IF
       xf = 0: yf = 0
      END IF
      IF x = 5 THEN GOSUB sq: ON f + 1 GOTO sq1, sq1, sq2, sq11, sq4, sq5, sq6, sq8, sq9, sq10, help
      ON x + 1 GOTO sq7, sq2, sq7, sq10, help
sq2:  stred = s2
      x = editstr(9, 23, 2, 8)
      IF stred <> s2 THEN
       s2 = stred
       IF yf THEN
        warwind "Number of head was changed; build options are initialized"
       END IF
       xf = 0: yf = 0
      END IF
      IF x = 5 THEN GOSUB sq: ON f + 1 GOTO sq2, sq1, sq2, sq11, sq4, sq5, sq6, sq8, sq9, sq10, help
      ON x + 1 GOTO sq7, sq3, sq1, sq10, help
sq3:  moff: chattr 16, 37, 10, 16: fprint 16, 37, " ", 16: fprint 16, 46, " ", 16: mon
      x = buttons(11, 6, 33, 1, 12, 16, 16)
      moff
      fprint 11, 6, " ", 16: fprint 11, 38, " ", 16
      chattr 16, 37, 10, 14: fprint 16, 37, CHR$(16), 14: fprint 16, 46, CHR$(17), 14
      mon
      IF x = 4 THEN GOSUB sq: ON f + 1 GOTO sq3, sq1, sq2, sq11, sq4, sq5, sq6, sq8, sq9, sq10, help
      ON x + 1 GOTO sq4, sq2, sq10, help, sq3, sq11
sq4:  stred = s3: x = editstr(14, 6, 68, 8): s3 = stred
      IF x = 5 THEN GOSUB sq: ON f + 1 GOTO sq4, sq1, sq2, sq11, sq4, sq5, sq6, sq8, sq9, sq10, help
      ON x + 1 GOTO sq7, sq5, sq3, sq10, help
sq5:  stred = s4: x = editstr(16, 20, 3, 8): s4 = stred
      IF x = 5 THEN GOSUB sq: ON f + 1 GOTO sq5, sq1, sq2, sq11, sq4, sq5, sq6, sq8, sq9, sq10, help
      ON x + 1 GOTO sq7, sq6, sq4, sq10, help
sq6:  x = eggs(8, 44, 3, 28, chid, 7, 9)
      IF x = 5 THEN GOSUB sq: ON f + 1 GOTO sq6, sq1, sq2, sq11, sq4, sq5, sq6, sq8, sq9, sq10, help
      ON x + 1 GOTO sq7, sq7, sq5, sq10, help
sq7:  x = buttons(16, 37, 10, 3, 12, 14, 16)
      IF x = 4 THEN GOSUB sq: ON f + 1 GOTO sq7, sq1, sq2, sq11, sq4, sq5, sq6, sq8, sq9, sq10, help
      ON x + 1 GOTO sq1, sq6, sq10, help, sq7, sq8, sq9, sq10
sq8:  GOSUB obdt: IF y THEN SOUND 150, 1: GOTO sq1
      IF nos = 0 OR dtf = 0 THEN erwind "This operation is valid for non-standard tracks only": GOTO sq1
      sqst f, noe, sf, s3
      IF f THEN SOUND 150, 1: GOTO sq1
      sqez = s3
      IF s4 = "" THEN SOUND 150, 1: GOTO sq1
      IF LEFT$(s4, 1) < "0" OR LEFT$(s4, 1) > "9" OR RIGHT$(s4, 1) < "0" OR RIGHT$(s4, 1) > "9" THEN SOUND 150, 1: GOTO sq1
      mingpl = VAL(s4)
      moff
      xf = 0: yf = 0
      x = ctrk(mingpl, noe, sf)
      IF x THEN erwind "Could not compress track": mon: GOTO sq1
      OPEN stemp + "~COMPRES.RPT" FOR OUTPUT AS #1
      hrp 1
      trp
      CLOSE #1
      wawind "Building Track Write Sequence..."
      iamlen = 0: lssf = 0: a = dop: dop = 4
      x = obr
      dop = a
      IF x THEN
       erwind "Could not build track write sequence"
      ELSE
       wriupd
      END IF
      winoff
      mon
      GOTO sq1
sq9:  sprog = ""
      IF stedit <> "" THEN sprog = stedit
      IF stview <> "" THEN sprog = stview
      IF sprog = "" THEN SOUND 150, 1: GOTO sq1
      sfile = stemp + "~COMPRES": stype = "RPT"
      moff
      winoff
      winoff
      mvflg = 1: GOTO cprg
sq10: moff: winoff: x = 0: GOTO ex
sq11: x = 1: GOSUB obdt: IF y THEN SOUND 150, 1: GOTO sq1
      moff
      GOSUB bop: IF x = 0 THEN yf = 1
      mon
      GOTO sq1
sq:   IF mpr = 7 AND mpc > 26 AND mpc < 30 THEN f = 1: RETURN
      IF mpr = 9 AND mpc > 22 AND mpc < 25 THEN f = 2: RETURN
      IF mpr = 11 AND mpc > 5 AND mpc < 40 THEN f = 3: RETURN
      IF mpr = 14 AND mpc > 5 AND mpc < 74 THEN f = 4: RETURN
      IF mpr = 16 AND mpc > 19 AND mpc < 23 THEN f = 5: RETURN
      IF mpr > 6 AND mpr < 12 AND mpc > 41 AND mpc < 74 THEN
       IF mpr > 7 AND mpr < 11 AND mpc > 42 AND mpc < 73 THEN
        moff
        mkwind 8, 45, 3, 1, 7
        chid = mpr - 8: fprint mpr, 45, CHR$(7), 7
        mon
       END IF
       f = 6: RETURN
      END IF
      IF mpr = 16 AND mpc > 36 AND mpc < 48 THEN f = 7: RETURN
      IF mpr = 16 AND mpc > 49 AND mpc < 61 THEN f = 8: RETURN
      IF mpr = 16 AND mpc > 62 AND mpc < 74 THEN f = 9: RETURN
      IF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 10: RETURN
      f = 0: RETURN
rdtr: menuflg = 1: moff: winoff
      s = "Analyse & Read Track"
      GOSUB addt
      IF x THEN x = 0: GOTO ex
      alltrkns = 1
      wawind "Initializing Drive..."
      diskdens = power(otrd)
      x = init(0)
      IF erflg GOTO er0
      IF x THEN winoff: x = 0: GOTO ex
      winoff
      wawind "Reading Track..."
      GOTO trp
vtrf: s = "View Track Report File": GOSUB ats: IF x THEN x = 0: GOTO ex
      IF stview <> "" THEN sprog = stview ELSE sprog = stedit
      sfile = stemp + "~TRACKAR\TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2)
      stype = "RPT"
      winoff
      GOTO cprg
vtdf: s = "View Track Data File": GOSUB ats: IF x THEN x = 0: GOTO ex
      sprog = sbview
      sfile = stemp + "~TRACKAR\TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2)
      stype = "BIN"
      winoff
      GOTO cprg
wrtr: moff: winoff: menuflg = 1
      wincre 7, 17, 10, 45, "Execute Track Write Sequence", dboxflg
      mkbord1 9, 20, 6, 12, 7: fprint 9, 22, " Drive ", 19
      fprint 10, 31, CHR$(24), 5: mkwind 11, 31, 2, 1, 5: fprint 13, 31, CHR$(25), 5
      fprint 10, 26, "A:", 7: IF drvbtyp THEN fprint 11, 26, "B:", 7
      fprint 10, 34, "Number of cylinder: [   ]", 7
      s1 = MID$(STR$(ltcyl), 2): v = "   ": LSET v = s1: fprint 10, 55, v, 8
      fprint 12, 34, "Number of head: [  ]", 7
      s2 = MID$(STR$(lthead), 2): v = "  ": LSET v = s2: fprint 12, 51, v, 8
      mkbut 14, 35, 10, "Ok", 1, 14, 18
      mkbut 14, 48, 10, "Cancel", 0, 16, 18
wrt1: help = 6: mon: IF drvbtyp = 0 THEN nod = 1 ELSE nod = 2
wt1:  x = msel(10, 21, 4, 10, nod, 0, la(), 0, 7, 8, 9, drive)
      IF x = 6 THEN GOSUB wt: ON f + 1 GOTO wt1, wt1, wt2, wt3, wt5, wt6, help
      ON x + 1 GOTO wt4, wt2, wt4, wt6, help
wt2:  stred = s1: x = editstr(10, 55, 3, 8): s1 = stred
      IF x = 5 THEN GOSUB wt: ON f + 1 GOTO wt2, wt1, wt2, wt3, wt5, wt6, help
      ON x + 1 GOTO wt4, wt3, wt1, wt6, help
wt3:  stred = s2: x = editstr(12, 51, 2, 8): s2 = stred
      IF x = 5 THEN GOSUB wt: ON f + 1 GOTO wt3, wt1, wt2, wt3, wt5, wt6, help
      ON x + 1 GOTO wt4, wt4, wt2, wt6, help
wt4:  x = buttons(14, 35, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB wt: ON f + 1 GOTO wt4, wt1, wt2, wt3, wt5, wt6, help
      ON x + 1 GOTO wt1, wt3, wt6, help, wt4, wt5, wt6
wt5:  IF s1 = "" OR s2 = "" THEN SOUND 150, 1: GOTO wt1
      IF LEFT$(s1, 1) < "0" OR LEFT$(s1, 1) > "9" OR RIGHT$(s1, 1) < "0" OR RIGHT$(s1, 1) > "9" OR s2 < "0" OR s2 > "1" THEN SOUND 150, 1: GOTO wt1
      IF VAL(s1) > ncyl - 1 THEN SOUND 150, 1: GOTO wt1
      ltcyl = VAL(s1): lthead = VAL(s2)
      moff
      wawind "Initializing Drive..."
      diskdens = 0: cyl = ltcyl: head = lthead
      wrirst
      cdens
      x = init(1)
      IF erflg GOTO er0
      IF x THEN winoff: GOTO wrt1
      winoff
      wawind "Writing Track..."
      GOTO twp
wt6:  moff: winoff: x = 0: GOTO ex
wt:   IF mpr > 8 AND mpr < 15 AND mpc > 19 AND mpc < 32 THEN
       IF mpr > 9 AND mpr < 10 + nod AND mpc > 20 AND mpc < 31 THEN moff: chattr drive + 10, 21, 10, 7: mon: drive = mpr - 10
       f = 1: RETURN
      ELSEIF mpr = 10 AND mpc > 54 AND mpc < 58 THEN f = 2: RETURN
      ELSEIF mpr = 12 AND mpc > 50 AND mpc < 53 THEN f = 3: RETURN
      ELSEIF mpr = 14 AND mpc > 34 AND mpc < 46 THEN f = 4: RETURN
      ELSEIF mpr = 14 AND mpc > 47 AND mpc < 59 THEN f = 5: RETURN
      ELSEIF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 6: RETURN
      ELSE : f = 0: RETURN
      END IF
vrpt: s = "View Disk Report File": GOSUB arf: IF s = "" THEN x = 0: GOTO ex
      IF stview <> "" THEN sprog = stview ELSE sprog = stedit
      sfile = s + "DISK": stype = "RPT"
      winoff
cprg: s = "": x = 0: rvideos: enerr = 0: menuflg = 2
      FOR i = 1 TO LEN(sprog)
       sf = MID$(sprog, i, 1)
       SELECT CASE x
        CASE 0
         IF sf = "!" THEN x = 1 ELSE s = s + sf
        CASE 1
         SELECT CASE sf
          CASE ".": x = 2
          CASE ":": x = 0: s = s + LEFT$(CURDIR$, 2)
          CASE "\": x = 0: s = s + CURDIR$
          CASE "!": x = 0: s = s + "!"
          CASE ELSE: x = 0: s = s + "!" + sf
         END SELECT
        CASE 2
         x = 0: IF sf = "!" THEN s = s + sfile + "." + stype ELSE s = s + sfile + sf
       END SELECT
      NEXT i
      IF x = 1 THEN s = s + "!"
      IF x = 2 THEN s = s + sfile
      s = UCASE$(s)
      dosprin s + scrlf
      SHELL s
      dosprin scrlf + "Press any key to continue" + scrlf
      a = bioskey
      x = 0: GOTO ex
arf:  IF diskattr THEN stred = "" ELSE stred = sopdir
      moff
      wincre 8, 3, 8, 74, s, dboxflg
      fprint 10, 6, "Operating directory name:", 7
      mkwind 11, 6, 1, 68, 8
      mkbut 13, 28, 10, "Ok", 1, 14, 18
      mkbut 13, 41, 10, "Cancel", 0, 16, 18
      mon
      help = 7
arf1: x = editstr(11, 6, 68, 8)
      IF x = 5 THEN
       IF mpr = 13 AND mpc > 27 AND mpc < 39 GOTO arf3
       IF mpr = 13 AND mpc > 40 AND mpc < 52 GOTO arf4
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      ELSE : ON x + 1 GOTO arf2, arf2, arf2, arf4, help
      END IF
      GOTO arf1
arf2: x = buttons(13, 28, 10, 2, 12, 14, 16)
      IF x = 4 THEN
       IF mpr = 11 AND mpc > 5 AND mpc < 74 GOTO arf1
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      ELSE : ON x + 1 GOTO arf1, arf1, arf4, help, arf2, arf3, arf4
      END IF
      GOTO arf2
arf3: s = stred: GOSUB chkp: moff: winoff: RETURN
arf4: s = "": moff: winoff: RETURN
vdat: s = "View Disk Data File": GOSUB arf: IF s = "" THEN x = 0: GOTO ex
      sprog = sbview
      sfile = s + "DISK": stype = "BIN"
      winoff
      GOTO cprg
vecl: s = STRING$(68, 0): moff
      wincre 5, 3, 14, 74, "Viewer & Editor Command Lines", dboxflg
      fprint 7, 6, "Command line of external text file viewer for report files:", 7
      stv = stview: LSET s = stv: fprint 8, 6, s, 8
      fprint 10, 6, "Command line of external text file editor for write sequence files:", 7
      ste = stedit: LSET s = ste: fprint 11, 6, s, 8
      fprint 13, 6, "Command line of external binary file viewer for track image files:", 7
      sbv = sbview: LSET s = sbv: fprint 14, 6, s, 8
      mkbut 16, 28, 10, "Ok", 1, 14, 18
      mkbut 16, 41, 10, "Cancel", 0, 16, 18
      mon
      help = 8
cl1:  stred = stv: x = editstr(8, 6, 68, 8): stv = stred
      IF x = 5 THEN GOSUB cl: ON f + 1 GOTO cl1, cl1, cl2, cl3, cl5, cl6, help
      ON x + 1 GOTO cl4, cl2, cl4, cl6, help
cl2:  stred = ste: x = editstr(11, 6, 68, 8): ste = stred
      IF x = 5 THEN GOSUB cl: ON f + 1 GOTO cl2, cl1, cl2, cl3, cl5, cl6, help
      ON x + 1 GOTO cl4, cl3, cl1, cl6, help
cl3:  stred = sbv: x = editstr(14, 6, 68, 8): sbv = stred
      IF x = 5 THEN GOSUB cl: ON f + 1 GOTO cl3, cl1, cl2, cl3, cl5, cl6, help
      ON x + 1 GOTO cl4, cl4, cl2, cl6, help
cl4:  x = buttons(16, 28, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB cl: ON f + 1 GOTO cl4, cl1, cl2, cl3, cl5, cl6, help
      ON x + 1 GOTO cl1, cl3, cl6, help, cl4, cl5, cl6
cl5:  stview = UCASE$(stv)
      stedit = UCASE$(ste)
      sbview = UCASE$(sbv)
cl6:  moff: winoff: x = 0: GOTO ex
cl:   IF mpr = 8 AND mpc > 5 AND mpc < 74 THEN f = 1: RETURN
      IF mpr = 11 AND mpc > 5 AND mpc < 74 THEN f = 2: RETURN
      IF mpr = 14 AND mpc > 5 AND mpc < 74 THEN f = 3: RETURN
      IF mpr = 16 AND mpc > 27 AND mpc < 39 THEN f = 4: RETURN
      IF mpr = 16 AND mpc > 40 AND mpc < 52 THEN f = 5: RETURN
      IF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 6: RETURN
      f = 0: RETURN
oopf: s = "Open Operating Directory": GOSUB arf: IF s = "" THEN x = 0: GOTO ex
      sf = s: dattrsv = x: closedisk: blin
      wawind "Opening Operating Directory..."
      enerr = 0
      OPEN sf + "DISK.WRI" FOR BINARY ACCESS READ WRITE AS #6
      enerr = 1
      IF xe THEN
       xe = 0
oof5:  erwind "DISK.WRI file is missing, cannot open": winoff: x = 0: GOTO ex
      ELSE
       IF LOF(6) = 0 THEN CLOSE #6: KILL sf + "DISK.WRI": GOTO oof5
       v = STRING$(10, 0): GET #6, , v
       IF LEFT$(v, 6) <> "SMA WS" THEN erwind "DISK.WRI file is invalid, cannot open": CLOSE #6: winoff: x = 0: GOTO ex
       IF MID$(v, 7, 2) < "01" THEN erwind "DISK.WRI file format is too old to be supported, cannot open": CLOSE #6: winoff: x = 0: GOTO ex
       IF MID$(v, 7, 2) < "02" THEN
        wincre 8, 11, 9, 58, "Open Operating Directory", dboxflg
        fprint 10, 14, "DISK.WRI file has an old but still supported format.", 7
        fprint 11, 14, "To be used with this version of FDA, this file must", 7
        fprint 12, 14, "be converted to the new format.", 7
        mkbut 14, 27, 11, "Convert", 1, 14, 18
        mkbut 14, 41, 11, "Cancel", 0, 16, 18
        mon
ood1:   x = buttons(14, 27, 11, 2, 12, 14, 16)
        ON x + 1 GOTO ood1, ood1, ood3, ood1, ood1, ood2, ood3
ood2:   moff: winoff: GOTO ood4
ood3:   moff: winoff: CLOSE #6: winoff: x = 0: GOTO ex
       END IF
ood4:  IF MID$(v, 7, 2) <= "01" THEN cvtws01
       IF MID$(v, 7, 2) > "02" THEN erwind "DISK.WRI file format is unknown, cannot open": CLOSE #6: winoff: x = 0: GOTO ex
       disknum = ASC(MID$(v, 9, 1)): ncyl = ASC(MID$(v, 10, 1))
      END IF
      n = 0
      enerr = 0
      OPEN sf + "DISK.FMT" FOR BINARY ACCESS READ WRITE AS #5
      enerr = 1
      IF xe THEN
       xe = 0
      ELSE
       IF LOF(5) = 0 THEN CLOSE #5: KILL sf + "DISK.FMT": GOTO oof2
       v = STRING$(10, 0): GET #5, , v
       IF LEFT$(v, 6) <> "SMA FF" THEN x = prowind("DISK.FMT file is invalid and will be ignored"): CLOSE #5: IF x THEN CLOSE #6: winoff: x = 0: GOTO ex ELSE GOTO oof2
       IF MID$(v, 7, 2) < "04" THEN x = prowind("DISK.FMT file format is too old to be supported, will be ignored"): CLOSE #5: IF x THEN CLOSE #6: winoff: x = 0: GOTO ex ELSE GOTO oof2
       IF MID$(v, 7, 2) < "06" THEN
        wincre 8, 11, 9, 58, "Open Operating Directory", dboxflg
        fprint 10, 14, "DISK.FMT file has an old but still supported format.", 7
        fprint 11, 14, "To be used with this version of FDA, this file must", 7
        fprint 12, 14, "be converted to the new format.", 7
        mkbut 14, 14, 15, "Convert", 1, 14, 18
        mkbut 14, 32, 15, "Ignore File", 0, 16, 18
        mkbut 14, 50, 15, "Cancel", 0, 16, 18
        mon
ood5:   x = buttons(14, 14, 15, 3, 12, 14, 16)
        ON x + 1 GOTO ood5, ood5, ood8, ood5, ood5, ood6, ood7, ood8
ood6:   moff: winoff: GOTO ood9
ood7:   moff: winoff: CLOSE #5: GOTO oof2
ood8:   moff: winoff: CLOSE #5: CLOSE #6: winoff: x = 0: GOTO ex
       END IF
ood9:  IF MID$(v, 7, 2) <= "04" THEN cvtff04 sf
       IF MID$(v, 7, 2) <= "05" THEN cvtff05
       IF MID$(v, 7, 2) > "06" THEN x = prowind("DISK.FMT file format is unknown, will be ignored"): CLOSE #5: IF x THEN CLOSE #6: winoff: x = 0: GOTO ex ELSE GOTO oof2
       IF disknum <> ASC(MID$(v, 9, 1)) OR ncyl <> ASC(MID$(v, 10, 1)) THEN x = prowind("The DISK.FMT file doesn't match the DISK.WRI file and will be ignored"): CLOSE #5: IF x THEN CLOSE #6: winoff: x = 0: GOTO ex ELSE GOTO oof2
       n = 1
      END IF
oof2: sopdir = sf
      REDIM qwrioffset(ncyl - 1, 1), dwrilen(ncyl - 1, 1)
      scanwrifile
      IF n THEN
       REDIM qidoffset(ncyl - 1, 1)
       scanfmtfile
      END IF
      diskattr = dattrsv
      IF n THEN idflg = 1
      blin
      ltcyl = 0: lthead = 0
      winoff
      x = 0: GOTO ex
pfdf: moff: winoff: menuflg = 1
df16: wawind "Loading Overlay File..."
      OPEN spath + "FDA.DFD" FOR BINARY AS #1
      SEEK #1, 513
      s = " ": GET #1, , s: nof = ASC(s)
      REDIM sform(nof)
      ldfdfil sform(), nof
      winoff
df15: aflg = 0: l = 0: fsys = 0
      wincre 2, 3, 20, 74, "Prepare for DOS Format", dboxflg
      mkbord1 4, 6, 14, 42, 7: fprint 4, 23, " Format ", 19
      fprint 5, 47, CHR$(24), 5: mkwind 6, 47, 10, 1, 5: fprint 16, 47, CHR$(25), 5
      s3a = STRING$(3, 32): s3b = STRING$(3, 32): s5 = STRING$(5, 32)
      FOR i = 0 TO nof - 1
       LSET s5 = STR$(ASC(sform(i)) * ASC(MID$(sform(i), 2, 1)))
       LSET s3a = STR$(ASC(sform(i)))
       LSET s3b = STR$(ASC(MID$(sform(i), 2, 1)))
       fprint 5 + i, 7, s5 + "K," + s3a + " tracks," + s3b + " sectors per track", 7
      NEXT i
      fprint 5 + nof, 8, "User-defined format", 7
      mkbord1 4, 50, 7, 24, 7: fprint 4, 52, " Recording Density ", 19
      fprint 5, 52, "( ) Low         1.2M", 7
      fprint 6, 52, "( ) Double       All", 7
      fprint 7, 52, "( ) Medium  Ex. 1.2M", 7
      fprint 8, 52, "( ) High        1.2M", 7
      fprint 9, 52, "( ) Quad       1.44M", 7
      fprint dfrd + 5, 53, CHR$(7), 7
      mkbord1 12, 50, 6, 24, 7: fprint 12, 53, " DOS System Files ", 19
      fprint 13, 52, "(" + CHR$(7) + ") None", 7
      fprint 14, 52, "( ) From drive A:", 7
      fprint 15, 52, "( ) From drive B:", 7
      fprint 16, 52, "( ) From drive C:", 7
      mkbut 19, 21, 10, "Set", 1, 14, 18
      mkbut 19, 34, 10, "Delete", 0, 16, 18
      mkbut 19, 47, 10, "Return", 0, 16, 18
      mon
      help = 9
df1:  x = msel(5, 7, 12, 40, nof + 1, 0, la(), 1, 7, 8, 9, l)
      IF x = 6 THEN GOSUB df: ON f + 1 GOTO df1, df1, df2, df4, df6, df7, df8, help
      ON x + 1 GOTO df5, df2, df5, df8, help, df6
df2:  x = eggs(5, 52, 5, 21, dfrd, 7, 9)
      IF x = 5 THEN GOSUB df: ON f + 1 GOTO df2, df1, df2, df4, df6, df7, df8, help
      ON x + 1 GOTO df5, df4, df1, df8, help
df4:  x = eggs(13, 52, 4, 21, fsys, 7, 9)
      IF x = 5 THEN GOSUB df: ON f + 1 GOTO df4, df1, df2, df4, df6, df7, df8, help
      ON x + 1 GOTO df5, df5, df2, df8, help
df5:  x = buttons(19, 21, 10, 3, 12, 14, 16)
      IF x = 4 THEN GOSUB df: ON f + 1 GOTO df5, df1, df2, df4, df6, df7, df8, help
      ON x + 1 GOTO df1, df4, df8, help, df5, df6, df7, df8
df6:  IF l <> nof GOTO df9
      moff
      winoff
      wincre 8, 14, 9, 52, "User-defined Format", dboxflg
      fprint 10, 27, "Number of cylinders: [   ]", 7
      s = MID$(STR$(ncyl), 2): t = "   ": LSET t = s
      fprint 10, 49, t, 8
      fprint 12, 23, "Number of sectors per track: [   ]", 7
      v = MID$(STR$(dfnos), 2): t = "   ": LSET t = v
      fprint 12, 53, t, 8
      mkbut 14, 17, 13, "Add", 1, 14, 18
      mkbut 14, 33, 13, "Don't Add", 0, 16, 18
      mkbut 14, 49, 13, "Cancel", 0, 16, 18
      mon
      help = 10
df10: stred = s: x = editstr(10, 49, 3, 8): s = stred
      IF x = 5 THEN
       IF mpr = 12 AND mpc > 52 AND mpc < 56 GOTO df11
       IF mpr = 14 AND mpc > 16 AND mpc < 31 GOTO df13
       IF mpr = 14 AND mpc > 32 AND mpc < 47 GOTO df14
       IF mpr = 14 AND mpc > 48 AND mpc < 63 GOTO df17
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      ELSE : ON x + 1 GOTO df12, df11, df12, df17, help
      END IF
      GOTO df10
df11: stred = v: x = editstr(12, 53, 3, 8): v = stred
      IF x = 5 THEN
       IF mpr = 10 AND mpc > 48 AND mpc < 52 GOTO df10
       IF mpr = 14 AND mpc > 16 AND mpc < 31 GOTO df13
       IF mpr = 14 AND mpc > 32 AND mpc < 47 GOTO df14
       IF mpr = 14 AND mpc > 48 AND mpc < 63 GOTO df17
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      ELSE : ON x + 1 GOTO df12, df12, df10, df17, help
      END IF
      GOTO df11
df12: x = buttons(14, 17, 13, 3, 12, 14, 16)
      IF x = 4 THEN
       IF mpr = 10 AND mpc > 48 AND mpc < 52 GOTO df10
       IF mpr = 12 AND mpc > 52 AND mpc < 56 GOTO df11
       IF mpr = 0 AND mpc > 69 AND mpc < 77 GOTO help
      ELSE : ON x + 1 GOTO df10, df11, df17, help, df12, df13, df14, df17
      END IF
      GOTO df12
df13: aflg = 1: IF nof = 11 THEN aflg = 0: SOUND 150, 1: GOTO df10
df14: IF s = "" OR v = "" THEN aflg = 0: SOUND 150, 1: GOTO df10
      s1 = LEFT$(s, 1): s2 = RIGHT$(s, 1): s3 = LEFT$(v, 1): s4 = RIGHT$(v, 1)
      IF s1 < "0" OR s1 > "9" OR s2 < "0" OR s2 > "9" OR s3 < "0" OR s3 > "9" OR s4 < "0" OR s4 > "9" THEN aflg = 0: SOUND 150, 1: GOTO df10
      noc = VAL(s): nos = VAL(v)
      IF noc = 0 OR nos = 0 THEN aflg = 0: SOUND 150, 1: GOTO df10
      dfpcalc noc, nos, nof, aflg, sform()
df9:  noc = ASC(sform(l))
      nos = ASC(MID$(sform(l), 2, 1))
      csz = ASC(MID$(sform(l), 3, 1))
      fsz = ASC(MID$(sform(l), 4, 1))
      noe = ASC(MID$(sform(l), 5, 1))
      mdb = ASC(RIGHT$(sform(l), 1))
      anos = noc * nos * 2
      moff
      winoff
      f = 1
      IF dfrd = 1 AND (nos = 8 OR nos = 9) THEN f = 0: agpl = 80
      IF dfrd = 3 AND nos = 15 THEN f = 0: agpl = 84
      IF dfrd = 4 AND nos = 18 THEN f = 0: agpl = 108
      IF f THEN
       drt = drttbl(dfrd)
       gpl = (drt - 66) / nos
       agpl = gpl - gpltab(2)
       IF agpl < 1 THEN erwind "Format impossible for this recording density": GOTO df15
      END IF
      wawind "Preparing for DOS Format..."
      IF agpl > 255 THEN agpl = 255
      ncyl = noc: dfnos = nos: RANDOMIZE PEEK(&H6C): disknum = RND(1) * 255
      sopdir = stemp + "~DOSFORM\": closedisk
      blin
      ltcyl = 0: lthead = 0
      REDIM qwrioffset(ncyl - 1, 1), dwrilen(ncyl - 1, 1)
      IF DIR$(sopdir + "DISK.WRI") <> "" THEN KILL sopdir + "DISK.WRI"
      OPEN sopdir + "DISK.WRI" FOR BINARY ACCESS READ WRITE AS #6
      s = "SMA WS02" + CHR$(disknum) + CHR$(ncyl): PUT #6, , s: qoff = 10

      dosform dfflen, anos, csz, fsz, noe, mdb, fsys

      x = 0: q = 0
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        dftws dfflen, x, q, qoff
      NEXT head, cyl
      IF aflg THEN
       nof = nof + 1: s = CHR$(nof): PUT #1, 513, s
       SEEK #1, 514 + (nof - 1) * 6
       PUT #1, , sform(nof - 1)
      END IF
      ERASE sform
      CLOSE #1
      CLOSE #2
      moff
      winoff
      diskattr = 2: blin
      x = 0: GOTO ex
df7:  IF l = nof THEN SOUND 150, 1: GOTO df1
      s = STRING$(513 + nof * 6, 0): GET #1, 1, s
      CLOSE #1
      nof = nof - 1
      s = LEFT$(s, 512) + CHR$(nof) + MID$(s, 514, l * 6) + RIGHT$(s, (nof - l) * 6)
      KILL spath + "FDA.DFD"
      OPEN spath + "FDA.DFD" FOR BINARY ACCESS WRITE AS #1
      PUT #1, , s
df17: CLOSE #1: moff: winoff: GOTO df16
df8:  CLOSE #1: moff: winoff: x = 0: GOTO ex
df:   IF mpr > 3 AND mpr < 18 AND mpc > 5 AND mpc < 48 THEN
       moff
       chattr 5 + l, 7, 40, 7: mon
       IF mpr > 4 AND mpr < 6 + nof AND mpc > 6 AND mpc < 47 THEN l = mpr - 5
       f = 1: RETURN
      ELSEIF mpr > 3 AND mpr < 11 AND mpc > 49 AND mpc < 74 THEN
       IF mpr > 4 AND mpr < 10 AND mpc > 50 AND mpc < 73 THEN dfrd = mpr - 5
       moff
       mkwind 5, 53, 5, 1, 7: fprint 5 + dfrd, 53, CHR$(7), 7: mon
       f = 2: RETURN
      ELSEIF mpr > 11 AND mpr < 18 AND mpc > 49 AND mpc < 74 THEN
       IF mpr > 12 AND mpr < 17 AND mpc > 50 AND mpc < 73 THEN fsys = mpr - 13
       moff
       mkwind 13, 53, 4, 1, 7: fprint 13 + fsys, 53, CHR$(7), 7: mon
       f = 3: RETURN
      ELSEIF mpr = 19 AND mpc > 20 AND mpc < 32 THEN f = 4: RETURN
      ELSEIF mpr = 19 AND mpc > 33 AND mpc < 45 THEN f = 5: RETURN
      ELSEIF mpr = 19 AND mpc > 46 AND mpc < 58 THEN f = 6: RETURN
      ELSEIF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 7: RETURN
      ELSE f = 0: RETURN
      END IF
dcal: moff: winoff: menuflg = 1
      wincre 2, 18, 20, 43, "Calibrate", dboxflg
      mkbord1 4, 21, 6, 12, 7: fprint 4, 23, " Drive ", 19
      fprint 5, 32, CHR$(24), 5: mkwind 6, 32, 2, 1, 5: fprint 8, 32, CHR$(25), 5
      fprint 5, 27, "A:", 7: IF drvbtyp THEN fprint 6, 27, "B:", 7
      mkbord1 4, 35, 7, 23, 7: fprint 4, 37, " Recording Density ", 19
      fprint 5, 37, "( ) Low", 7
      fprint 6, 37, "( ) Double", 7
      fprint 7, 37, "( ) Medium", 7
      fprint 8, 37, "( ) High", 7
      fprint 9, 37, "( ) Quad", 7
      fprint dcrd + 5, 38, CHR$(7), 7
      mkbut 12, 28, 10, "Ok", 1, 14, 18
      mkbut 12, 41, 10, "Cancel", 0, 16, 18
      mkbord1 15, 21, 6, 37, 7: fprint 15, 35, " Results ", 19
      fprint 16, 23, "Disk revolution time:", 7
      fprint 17, 23, "Track length for FDC:", 7
      fprint 18, 23, "Abort precompensation:", 7
      fprint 19, 23, "Pos. precomp./FDC bug:", 7
      dcwf = 1
dcl0: help = 11: mon: IF drvbtyp = 0 THEN nod = 1 ELSE nod = 2
dc1:  x = msel(5, 22, 4, 10, nod, 0, la(), 0, 7, 8, 9, drive)
      IF x = 6 THEN GOSUB dc: ON f + 1 GOTO dc1, dc1, dc2, dc4, dc5, help
      ON x + 1 GOTO dc3, dc2, dc3, dc5, help
dc2:  x = eggs(5, 37, 5, 13, dcrd, 7, 9)
      IF x = 5 THEN GOSUB dc: ON f + 1 GOTO dc2, dc1, dc2, dc4, dc5, help
      ON x + 1 GOTO dc3, dc3, dc1, dc5, help
dc3:  x = buttons(12, 28, 10, 2, 12, 14, 16)
      IF x = 4 THEN GOSUB dc: ON f + 1 GOTO dc3, dc1, dc2, dc4, dc5, help
      ON x + 1 GOTO dc1, dc2, dc5, help, dc3, dc4, dc5
dc4:  moff
      IF dcwf THEN
       x = prowind("All data on the diskette will be destroyed!")
       IF x GOTO dcl0
       dcwf = 0
      END IF
      closedisk
      blin
      wawind "Initializing Drive..."
      ncyl = 1: diskdens = power(dcrd)
      x = init(1)
      IF erflg GOTO er0
      IF x THEN winoff: GOTO dcl0
      y = ixfr(dcrd, 0)
      winoff
      wawind "Calibrating. Wait for 25 s..."
      GOTO dcp
dcl1: v = "           ": LSET v = s1: fprint 16, 45, v, 7
      v = "           ": LSET v = s2: fprint 17, 45, v, 7
      v = "          ": LSET v = s3: fprint 18, 46, v, 7
      v = "          ": LSET v = s4: fprint 19, 46, v, 7
      GOTO dcl0
dc5:  moff: winoff: x = 0: GOTO ex
dc:   IF mpr > 3 AND mpr < 10 AND mpc > 20 AND mpc < 33 THEN
       IF mpr > 4 AND mpr < 5 + nod AND mpc > 21 AND mpc < 32 THEN moff: chattr drive + 5, 22, 10, 7: mon: drive = mpr - 5
       f = 1
      ELSEIF mpr > 3 AND mpr < 11 AND mpc > 34 AND mpc < 58 THEN
       IF mpr > 4 AND mpr < 10 AND mpc > 35 AND mpc < 57 THEN moff: mkwind 5, 38, 5, 1, 7: fprint mpr, 38, CHR$(7), 7: mon: dcrd = mpr - 5
       f = 2
      ELSEIF mpr = 12 AND mpc > 27 AND mpc < 39 THEN f = 3
      ELSEIF mpr = 12 AND mpc > 40 AND mpc < 52 THEN f = 4
      ELSEIF mpr = 0 AND mpc > 69 AND mpc < 77 THEN f = 5
      ELSE : f = 0
      END IF
      RETURN
svcs: wawind "Saving Current Settings..."
      x = 1: GOTO ex

      '-------------- Операционный блок ------------------

drp:  POKE &H40, 0
      qoff = 0
      q = dmabcre: IF q = 0 THEN erwind "Out of memory": aflg = 1: GOTO opco
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)
      LSET sid = sopdir + "DISK.BIN" + CHR$(0)
      regs.ax = &H3C00
      regs.cx = 0
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF regs.flags AND 1 THEN
       dmabdel
       boflg = 0
       SELECT CASE regs.ax
        CASE 3: ERROR 53
        CASE 4: ERROR 67
        CASE 5: ERROR 54
       END SELECT
      END IF
      foflg = 1
      filnum = regs.ax

      aflg = 0: owd = -1
    FOR cyl = 0 TO ncyl - 1
     FOR head = 0 TO 1
      POKE &H40, 0
      hdd = drive
      IF head THEN hdd = hdd + 4

      GOSUB aow
      IF f = 1 THEN
pex1:  aflg = 1: EXIT FOR
      END IF
      IF f = -1 THEN x = init(0): IF erflg GOTO er0
      hdpos cyl: IF erflg GOTO er0
      FOR i = dens(0) + 5 TO dens(0) + 1 STEP -1
       POKE &H40, 0
       dot = dens(i)
       x = ixfr(dot, 1): IF x GOTO tnd
       ERASE spar
       x = drp(boff, 0): IF erflg GOTO er0
       IF nos OR dtf THEN
        IF fwd = 0 THEN
         stopmot
         calreq
         GOTO pex1
        END IF
        EXIT FOR
       END IF
tnd:  NEXT i
      POKE &H40, 0
      IF nos THEN
       IF x THEN
        x = drp(boff, 1): IF erflg GOTO er0
       END IF
       FOR i = 1 TO nos
        IF spar(i).offset <> -1 THEN spar(i).offset = spar(i).offset + qoff
       NEXT i
      END IF
      qidoffset(cyl, head) = qidoff
      s = CHR$(dtf) + CHR$(dot) + CHR$(nos) + CHR$(alen) + CHR$(agpl) + CHR$(dop) + LEFT$(MKI$(abtkid), 1) + LEFT$(MKI$(abtcrc), 1) + CHR$(bdmpos)
      PUT #5, , s
      qidoff = qidoff + 9
      FOR i = 1 TO nos
       s = CHR$(spar(i).c) + CHR$(spar(i).h) + CHR$(spar(i).r) + CHR$(spar(i).n) + MKI$(spar(i).gpl) + MKI$(spar(i).post) + CHR$(spar(i).csn) + CHR$(spar(i).crc) + CHR$(spar(i).dt) + MKL$(spar(i).offset) + MKI$(spar(i).datlen) + MKI$(spar(i).dif) +  _
MKI$(spar(i).sfb) + MKI$(spar(i).agr)
       PUT #5, , s
       qidoff = qidoff + 23
      NEXT i
      IF nos = 0 GOTO nexttrack
      dens(0) = dot: owd = dot: qoff = qoff + boff

      IF boff THEN
       regs.ax = &H4000
       regs.bx = filnum
       regs.cx = boff
       regs.dx = dmaboff
       regs.ds = dmabseg
       interruptx &H21, regs, regs
       IF regs.ax <> boff THEN ERROR 61
      END IF

nexttrack:

     NEXT head
      IF aflg THEN EXIT FOR
    NEXT cyl
      regs.ax = &H3E00
      regs.bx = filnum
      interruptx &H21, regs, regs
      foflg = 0
      stopmot
      dmabdel
      boflg = 0
      IF aflg GOTO opco
      obrf = 1: GOTO opco

      '--------------------------- обработка ------------------------------

obr:  wincre 10, 18, 5, 44, "Building Write Sequence & Saving Info", oboxflg
      fprint 12, 21, "Cylinder:     Head: x      % completed", 7
      xp = -1
      OPEN sopdir + "DISK.RPT" FOR OUTPUT AS #1
      hrp 0
      IF DIR$(sopdir + "DISK.WRI") <> "" THEN KILL sopdir + "DISK.WRI"
      OPEN sopdir + "DISK.WRI" FOR BINARY ACCESS READ WRITE AS #6
      s = "SMA WS02" + CHR$(disknum) + CHR$(ncyl): PUT #6, , s: qoff = 10
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        fprint 12, 30, STR$(cyl), 7: fprint 12, 40, STR$(head), 7
        x = (cyl * 2 + head) / (ncyl * 2) * 100
        IF x <> xp THEN
         s = STRING$(4, 32): RSET s = STR$(x)
         fprint 12, 44, s, 7
         xp = x
        END IF
        fmtrst
        trp
        IF nos THEN
         chid = ctchid: lssf = ctlssf
         xf = 0
         IF ctenao THEN
          IF spar(nos).dt <> 2 THEN
           IF spar(nos).n < 8 THEN
            IF gpltab(spar(nos).n) > spar(nos).gpl + dop THEN xf = 3
           END IF
          END IF
         END IF
         a = dop
         IF xf = 3 THEN
          x = ctrk(ctmingpl, 0, "")
          IF x THEN xf = 0: fmtrst ELSE dop = 4
         END IF
obr1:    IF xf THEN iamlen = 0: lssf = 0 ELSE iamlen = 66
         aoerr = obr
         IF aoerr THEN IF xf THEN xf = 2 ELSE xf = 1: GOTO obr1
         dop = a
        ELSE
         xf = 0
         seq = MKL$(-1) + CHR$(0)
        END IF
        qwrioffset(cyl, head) = qoff: dwrilen(cyl, head) = LEN(seq): qoff = qoff + LEN(seq)
        PUT #6, , seq
        IF xf <> 0 AND ctenww <> 0 THEN
         ltcyl = cyl: lthead = head
         BEEP
         SELECT CASE xf
          CASE 1: s1 = "IAM is overwritten on "
          CASE 2: s1 = "Could not build write sequence for "
          CASE 3: s1 = "Compression is done on "
         END SELECT
         s2 = "Cylinder" + STR$(cyl) + " Head" + STR$(head)
         s = s1 + s2
         post = (74 - LEN(s)) \ 2
         wincre 9, post, 7, LEN(s) + 6, "Warning", 2
         fprint 11, post + 3, s, 7
         mkbut 13, 26, 12, "Continue", 1, 14, 18
         mkbut 13, 41, 12, "Cancel", 0, 16, 18
         mon
aoe1:    x = buttons(13, 26, 12, 2, 12, 14, 16)
         ON x + 1 GOTO aoe1, aoe1, aoe3, aoe1, aoe1, aoe2, aoe3
aoe2:    moff: winoff
        END IF
      NEXT head, cyl
      CLOSE #1
      clkident
      cerident
      gtdident
      winoff
      x = 0: RETURN
aoe3: moff: winoff: winoff: CLOSE #1: closedisk: x = 1: RETURN

      '----------- запись копии ----------------------------------------

dwp:  POKE &H40, 0
      aflg = 0
      q = dmabcre: IF q = 0 THEN erwind "Out of memory": aflg = 1: GOTO opco
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)
      LSET sid = sopdir + "DISK.BIN" + CHR$(0)
      regs.ax = &H3D00
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF regs.flags AND 1 THEN
       dmabdel
       boflg = 0
       SELECT CASE regs.ax
        CASE 2, 3: ERROR 53
        CASE 4: ERROR 67
        CASE 5: ERROR 54
       END SELECT
      END IF
      foflg = 1
      filnum = regs.ax

      owd = -2
   FOR cyl = 0 TO ncyl - 1
    FOR head = 0 TO 1
      POKE &H40, 0
      GOSUB aow
      IF f = 1 THEN
pex2:  aflg = 1: EXIT FOR
      END IF
      IF f = -1 THEN x = init(1): IF erflg GOTO er0
      wrirst
      IF ASC(MID$(seq, 5, 1)) = 0 GOTO ntrk1
      hdd = drive
      IF head THEN hdd = hdd + 4
      hdpos cyl: IF erflg GOTO er0

      ql = CVL(LEFT$(seq, 4))
      IF ql <> -1 THEN
       regs.ax = &H4200
       regs.bx = filnum
       regs.cx = e243(ql)
       regs.dx = e241(ql)
       interruptx &H21, regs, regs
       regs.ax = &H3F00
       regs.bx = filnum
       regs.cx = 31744
       regs.dx = dmaboff + 1024
       regs.ds = dmabseg
       interruptx &H21, regs, regs
      END IF

      dwp ql - 1024: IF erflg GOTO er0

ntrk1:
    NEXT head
      IF aflg THEN EXIT FOR
   NEXT cyl
      IF aflg = 0 THEN
       POKE &H40, 0
       clkregen
       IF erflg GOTO er0
       cerregen
       IF erflg GOTO er0
       gtdregen
       IF erflg GOTO er0
      END IF
      regs.ax = &H3E00
      regs.bx = filnum
      interruptx &H21, regs, regs
      foflg = 0
      stopmot
      dmabdel
      boflg = 0
      obrf = 0: GOTO opco

      '--------------------- Чтение дорожки -----------------------

trp:  qoff = 0
      q = dmabcre: IF q = 0 THEN erwind "Out of memory": GOTO eor2
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)
      LSET sid = stemp + "~TRACKAR\TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2) + ".BIN" + CHR$(0)
      regs.ax = &H3C00
      regs.cx = 0
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF regs.flags AND 1 THEN
       dmabdel
       boflg = 0
       SELECT CASE regs.ax
        CASE 3: ERROR 53
        CASE 4: ERROR 67
        CASE 5: ERROR 54
       END SELECT
      END IF
      foflg = 1
      filnum = regs.ax

      cyl = ltcyl: head = lthead
      hdd = drive
      IF head THEN hdd = hdd + 4
      POKE &H40, 0

      hdpos cyl: IF erflg GOTO er0
      x = ixfr(otrd, 1)
      ERASE spar
      dot = otrd
      x = drp(boff, 0): IF erflg GOTO er0
      IF x THEN
       x = drp(boff, 1): IF erflg GOTO er0
       f = 1
      ELSE
       f = 0
      END IF

      IF boff THEN
       regs.ax = &H4000
       regs.bx = filnum
       regs.cx = boff
       regs.dx = dmaboff
       regs.ds = dmabseg
       interruptx &H21, regs, regs
       IF regs.ax <> boff THEN ERROR 61
      END IF

      regs.ax = &H3E00
      regs.bx = filnum
      interruptx &H21, regs, regs
      foflg = 0
      stopmot
      IF f THEN warwind "Sector data truncated"
      OPEN stemp + "~TRACKAR\TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2) + ".RPT" FOR OUTPUT AS #1
      hrp 1
      trp
      CLOSE #1

eotr: dmabdel: boflg = 0
eor2: winoff: x = 0: GOTO ex

      '------------------ Чтение всей дорожки ---------------------

tap:  q = dmabcre: IF q = 0 THEN erwind "Out of memory": GOTO eoa2
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)

      cyl = ltcyl: head = lthead
      hdpos cyl: IF erflg GOTO er0
      hdd = drive: IF head THEN hdd = hdd + 4
      x = ixfr(otrd, 1)
      dmal = lensec(maxlen)
      cmd(1) = &H42
      cmd(2) = hdd
      cmd(6) = maxlen
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 0, dmal
      dbtc 9: IF erflg GOTO er0
      waint
      IF erflg GOTO er0
      n = csfc: IF erflg GOTO er0

      stopmot
      SELECT CASE otrd
       CASE 0: s = "L"
       CASE 1: s = "D"
       CASE 2: s = "M"
       CASE 3: s = "H"
       CASE 4: s = "Q"
      END SELECT
      LSET sid = sopdir + "TRK" + MID$(STR$(ltcyl), 2) + "-" + MID$(STR$(lthead), 2) + s + ".ABS" + CHR$(0)
      regs.ax = &H3C00
      regs.cx = 0
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF regs.flags AND 1 THEN
       SELECT CASE regs.ax
        CASE 3: ERROR 53
        CASE 4: ERROR 67
        CASE 5: ERROR 54
       END SELECT
      END IF
      foflg = 1
      filnum = regs.ax
      regs.ax = &H4000
      regs.bx = filnum
      regs.cx = dmal
      regs.dx = dmaboff
      regs.ds = dmabseg
      interruptx &H21, regs, regs
      regs.ax = &H3E00
      regs.bx = filnum
      interruptx &H21, regs, regs
      foflg = 0
      xf = 0
      s1 = "FDC Status:"
      IF n THEN
       s2 = HEX$(cstat(0)) + "H"
       SELECT CASE cstat(0) AND &HC0
        CASE 0: s3 = "NT": xf = 1
        CASE &H40: s3 = "AT"
        CASE &H80: s3 = "IC"
        CASE &HC0: s3 = ""
       END SELECT
       IF cstat(0) AND &H20 THEN s3 = s3 + " SE"
       IF cstat(0) AND &H10 THEN s3 = s3 + " EC"
       IF cstat(0) AND 8 THEN s3 = s3 + " NR"
       IF n > 1 THEN
        s2 = s2 + " " + HEX$(cstat(1)) + "H"
        IF cstat(1) AND &H80 THEN s3 = s3 + " EN"
        IF cstat(1) AND &H20 THEN s3 = s3 + " DE"
        IF cstat(1) AND &H10 THEN s3 = s3 + " OR"
        IF cstat(1) AND 4 THEN s3 = s3 + " ND"
        IF cstat(1) AND 2 THEN s3 = s3 + " NW"
        IF cstat(1) AND 1 THEN s3 = s3 + " MA"
        IF n > 2 THEN
         s2 = s2 + " " + HEX$(cstat(2)) + "H"
         IF cstat(2) AND &H40 THEN s3 = s3 + " CM"
         IF cstat(2) AND &H20 THEN s3 = s3 + " DD"
         IF cstat(2) AND &H10 THEN s3 = s3 + " WC"
         IF cstat(2) AND 8 THEN s3 = s3 + " SH"
         IF cstat(2) AND 4 THEN s3 = s3 + " SN"
         IF cstat(2) AND 2 THEN s3 = s3 + " BC"
         IF cstat(2) AND 1 THEN s3 = s3 + " MD"
         FOR i = 3 TO n - 1
          s2 = s2 + " " + HEX$(cstat(i)) + "H"
         NEXT i
        END IF
       END IF
      ELSE
       s2 = "None"
       s3 = ""
      END IF
      IF xf THEN s4 = "Normal termination" ELSE s4 = "Abnormal termination"
      x = 0
      IF LEN(s1) > x THEN x = LEN(s1)
      IF LEN(s2) > x THEN x = LEN(s2)
      IF LEN(s3) > x THEN x = LEN(s3)
      IF LEN(s4) > x THEN x = LEN(s4)
      post = (74 - x) \ 2
      IF xf THEN a = dboxflg ELSE a = 2
      wincre 7, post, 11, x + 6, "", a
      fprint 9, post + 3, s1, 7
      fprint 10, post + 3, s2, 7
      fprint 11, post + 3, s3, 7
      a = (80 - LEN(s4)) \ 2
      fprint 13, a, s4, 7
      mkbut 15, 35, 10, "Ok", 1, 12, 18
      mon
sts1: f = 0: WHILE f = 0: mkey f, a, b, bs: WEND
      IF f = 1 THEN
       IF a = 13 OR a = 27 GOTO sts2
      ELSE
       IF a = 15 AND b > 34 AND b < 46 GOTO sts2
      END IF
      GOTO sts1
sts2: moff: winoff
eoar: dmabdel: boflg = 0
eoa2: winoff: x = 0: GOTO ex

      '--------------------- запись дорожки -----------------------

twp:  aflg = 0
      q = dmabcre: IF q = 0 THEN erwind "Out of memory": GOTO eow2
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)
      LSET sid = sopdir + "DISK.BIN" + CHR$(0)
      regs.ax = &H3D00
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      IF regs.flags AND 1 THEN
       dmabdel
       boflg = 0
       SELECT CASE regs.ax
        CASE 2, 3: ERROR 53
        CASE 4: ERROR 67
        CASE 5: ERROR 54
       END SELECT
      END IF
      foflg = 1
      filnum = regs.ax

      cyl = ltcyl: head = lthead
      wrirst
      IF ASC(MID$(seq, 5, 1)) = 0 GOTO eotw
      hdd = drive
      IF head THEN hdd = hdd + 4
      hdpos cyl: IF erflg GOTO er0

      ql = CVL(LEFT$(seq, 4))
      IF ql <> -1 THEN
       regs.ax = &H4200
       regs.bx = filnum
       regs.cx = e243(ql)
       regs.dx = e241(ql)
       interruptx &H21, regs, regs
       regs.ax = &H3F00
       regs.bx = filnum
       regs.cx = 31744
       regs.dx = dmaboff + 1024
       regs.ds = dmabseg
       interruptx &H21, regs, regs
      END IF

      dwp ql - 1024: IF erflg GOTO er0

      regs.ax = &H3E00
      regs.bx = filnum
      interruptx &H21, regs, regs
      foflg = 0
      stopmot
eotw: dmabdel: boflg = 0
eow2: winoff: winoff: x = 0: GOTO ex

      '------------------------ калибровка ------------------------

dcp:  q = dmabcre: IF q = 0 THEN erwind "Out of memory": GOTO eoc3
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)
      cyl = 0: head = 0: hdd = drive
      hdpos 0: IF erflg GOTO er0
      dcpmain s1, s2, s3, s4
eoc2: dmabdel: boflg = 0
eoc3: winoff
      stopmot
      GOTO dcl1

help: hlp helpnum: ON help + 1 GOTO ri1, ri1, adl1, ats1, adt1, av1, wt1, arf1, cl1, df1, df10, dc1, ob1, bop1, sq1
ex:   command = x: EXIT FUNCTION

er0:  SELECT CASE erflg
       CASE 1: xderr = 200: bioserr = 128
       CASE 2: xderr = 201: bioserr = 32
       CASE 3: xderr = 202: bioserr = 64
       CASE 4: xderr = 203
       CASE 5: xderr = 204
      END SELECT
      erflg = 0: ERROR xderr

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
       CASE 200
        serr = "Disk timeout"
       CASE 201
        serr = "FDC failure"
       CASE 202
        serr = "Seek failure"
       CASE 203
        serr = "No disk in drive"
       CASE 204
        serr = "Write-Protected Disk"
       CASE ELSE
        ERROR ERR
      END SELECT
opcan:
      IF enerr THEN
       erwind serr
       moff
       IF menuflg THEN woff ELSE woff1
       x = 0: RESUME ex
      ELSE
       xe = 1: RESUME NEXT
      END IF
END FUNCTION

