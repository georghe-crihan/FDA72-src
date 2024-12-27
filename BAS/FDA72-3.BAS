'$INCLUDE: 'FDA72-G.BI'
DIM win(1 TO 5) AS winpar
END

SUB abt
      swinfdcproc 22, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 GOTO er0
      x = dsd0
      xdx = x * 16 + 527
      swinfdcproc 18, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 GOTO er0
      EXIT SUB

er0:  IF xfl AND 128 THEN
       erflg = 3
      ELSE
       IF xfl AND 64 THEN erflg = 1 ELSE erflg = 2
      END IF
END SUB

FUNCTION buttons (BYVAL br, BYVAL bc, BYVAL nc, BYVAL nb, BYVAL aa, BYVAL ad, BYVAL ao)
      bb = 0: moff
but1: FOR i = 0 TO nb - 1
       chattr br, bc + i * (nc + 3), nc, ao
       fprint br, bc + i * (nc + 3), " ", ao
       fprint br, bc + i * (nc + 3) + nc - 1, " ", ao
      NEXT i
      chattr br, bc + bb * (nc + 3), nc, aa
      fprint br, bc + bb * (nc + 3), CHR$(16), aa
      fprint br, bc + bb * (nc + 3) + nc - 1, CHR$(17), aa
      mon
      f = 0: WHILE f = 0: mkey f, a, b, bs: WEND: moff
      chattr br, bc + bb * (nc + 3), nc, ao
      fprint br, bc + bb * (nc + 3), " ", ao
      fprint br, bc + bb * (nc + 3) + nc - 1, " ", ao
      chattr br, bc, nc, ad
      fprint br, bc, CHR$(16), ad
      fprint br, bc + nc - 1, CHR$(17), ad
      IF f = 1 THEN
       SELECT CASE a
        CASE 0
         SELECT CASE b
          CASE &HF: bb = bb - 1: IF bb = -1 THEN buttons = 1: GOTO ex0
          CASE &H3B: buttons = 3: GOTO ex0
          CASE &H4B: IF bb THEN bb = bb - 1
          CASE &H4D: IF bb <> nb - 1 THEN bb = bb + 1
         END SELECT
        CASE 9: bb = bb + 1: IF bb = nb THEN buttons = 0: GOTO ex0
        CASE &HD: buttons = 5 + bb: GOTO ex0
        CASE &H1B: buttons = 2: GOTO ex0
       END SELECT
      ELSE
       IF a <> br THEN buttons = 4: mpr = a: mpc = b: GOTO ex0
       FOR i = 0 TO nb - 1
        IF b > bc + i * (nc + 3) - 1 AND b < bc + i * (nc + 3) + nc + 1 THEN buttons = 5 + i: f = 0: EXIT FOR
       NEXT i
       IF f = 0 GOTO ex0
      END IF
      GOTO but1
ex0:  mon
END FUNCTION

SUB calreq
      s = "Calibration required.  Drive: " + CHR$(drive + 65) + ":  Density: "
      SELECT CASE dot
       CASE 0: s = s + "Low"
       CASE 1: s = s + "Double"
       CASE 2: s = s + "Medium"
       CASE 3: s = s + "High"
       CASE 4: s = s + "Quad"
      END SELECT
      erwind s
END SUB

SUB cdens
      posit = 6
      FOR i = 1 TO ASC(MID$(seq, 5, 1))
       x = ASC(MID$(seq, posit, 1)): posit = posit + 1
       SELECT CASE x
        CASE 0
         x = ASC(MID$(seq, posit, 1))
         posit = posit + x + 1
        CASE 1
         x = ASC(MID$(seq, posit, 1)): posit = posit + 1
         diskdens = diskdens OR power(x)
        CASE 3
         x = ASC(MID$(seq, posit, 1))
         y = ASC(MID$(seq, posit + 4, 1))
         posit = posit + 5 + y * 3 + x * 4
        CASE 4
         posit = posit + 14
         x = ASC(MID$(seq, posit, 1)): posit = posit + x * 3 + 1
         x = ASC(MID$(seq, posit, 1)): posit = posit + x * 3 + 1
       END SELECT
      NEXT i
END SUB

FUNCTION checkbox (BYVAL br, BYVAL bc, BYVAL nc, opt, BYVAL an, BYVAL ab)
      moff
      cursor br, bc + 1
      chattr br, bc, 3, ab
cbx1: mon
      DO: mkey f, a, b, bs: LOOP UNTIL f
      moff
      IF f = 1 THEN
       SELECT CASE a
        CASE 0
         SELECT CASE b
          CASE &HF: checkbox = 2: GOTO ex3
          CASE &H3B: checkbox = 4: GOTO ex3
         END SELECT
        CASE 9: checkbox = 1: GOTO ex3
        CASE &HD: checkbox = 0: GOTO ex3
        CASE &H1B: checkbox = 3: GOTO ex3
        CASE &H20
cbx2:    IF opt THEN opt = 0: s = " " ELSE opt = 1: s = "x"
         fprint br, bc + 1, s, ab
         GOTO cbx1
       END SELECT
      ELSE
       IF a <> br OR b < bc OR b > bc + nc - 1 THEN checkbox = 5: mpr = a: mpc = b: GOTO ex3
       GOTO cbx2
      END IF
      GOTO cbx1
ex3:  chattr br, bc, 3, an
      curoff
      mon
END FUNCTION

SUB closedisk
      diskattr = -1: idflg = 0
      ERASE qidoffset, qwrioffset, dwrilen
      CLOSE #5
      CLOSE #6
END SUB

FUNCTION csfc
      cntr = 0
      DO
       swinfdcproc 2, xax, xbx, xcx, xdx, xfl
       IF xfl AND 1 THEN
        IF xfl AND 64 THEN erflg = 1
        EXIT DO
       END IF
       cstat(cntr) = e121(xax)
       cntr = cntr + 1
      LOOP
      csfc = cntr
END FUNCTION

SUB cvtff04 (sf)
      CLOSE #5
      NAME sf + "DISK.FMT" AS sf + "~DISKOLD.FMT"
      OPEN sf + "~DISKOLD.FMT" FOR BINARY ACCESS READ AS #1
      OPEN sf + "DISK.FMT" FOR BINARY ACCESS READ WRITE AS #5
      v = STRING$(10, 0): GET #1, , v
      MID$(v, 7) = "05": PUT #5, , v
      ncyl = ASC(MID$(v, 10, 1))
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        v = STRING$(9, 0): GET #1, , v
        nos = ASC(MID$(v, 3, 1))
        PUT #5, , v
        FOR i = 1 TO nos
         v = STRING$(21, 0): GET #1, , v
         v = v + MKI$(0)
         PUT #5, , v
      NEXT i, head, cyl
      CLOSE #1: KILL sf + "~DISKOLD.FMT"
END SUB

SUB cvtff05
      SEEK #5, 7: v = "06": PUT #5, , v
      SEEK #5, 10: v = " ": GET #5, , v: ncyl = ASC(v)
      q = 10
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        SEEK #5, q + 3: v = " ": GET #5, , v: nos = ASC(v)
        q = q + 9
        FOR i = 1 TO nos
         SEEK #5, q + 7: v = "  ": GET #5, , v: post = CVI(v)
         post = post + 146
         SEEK #5, q + 7: v = MKI$(post): PUT #5, , v
         q = q + 23
      NEXT i, head, cyl
END SUB

SUB cvtws01
      SEEK #6, 7: v = "02": PUT #6, , v
      SEEK #6, 10: v = " ": GET #6, , v: ncyl = ASC(v)
      q = 10
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        SEEK #6, q + 5: v = " ": GET #6, , v: noop = ASC(v)
        q = q + 5
        FOR i = 1 TO noop
         SEEK #6, q + 1: v = " ": GET #6, , v
         SELECT CASE ASC(v)
          CASE 0
           SEEK #6, q + 2: v = " ": GET #6, , v
           q = q + ASC(v) + 2
          CASE 1
           q = q + 2
          CASE 2
           q = q + 1
          CASE 3
           SEEK #6, q + 2: v = " ": GET #6, , v: nsec = ASC(v)
           SEEK #6, q + 6: v = " ": GET #6, , v: f = ASC(v)
           q = q + 6
           IF f THEN
            SEEK #6, q + 2: v = "  ": GET #6, , v: post = CVI(v)
            post = post + 146
            SEEK #6, q + 2: v = MKI$(post): PUT #6, , v
            q = q + 3
           END IF
           q = q + nsec * 4
          CASE 4
           oldrev = 0
           SEEK #6, q + 16: v = " ": GET #6, , v: f = ASC(v)
           q = q + 16
           IF f THEN
            SEEK #6, q + 2: v = "  ": GET #6, , v: post = CVI(v)
            post = post + 168
            SEEK #6, q + 2: v = MKI$(post): PUT #6, , v
            IF post = 146 THEN oldrev = 1: qoldpos = q
            q = q + 3
           END IF
           SEEK #6, q + 1: v = " ": GET #6, , v: f = ASC(v)
           q = q + 1
           IF f THEN
            SEEK #6, q + 2: v = "  ": GET #6, , v: post = CVI(v)
            post = post + 146
            SEEK #6, q + 2: v = MKI$(post): PUT #6, , v
            IF oldrev THEN
             SEEK #6, q + 1: v = " ": GET #6, , v: x = ASC(v)
             x = x + 1
             SEEK #6, q + 1: v = CHR$(x): PUT #6, , v
             SEEK #6, qoldpos + 1: v = " ": GET #6, , v: x = ASC(v)
             x = x + 1
             SEEK #6, qoldpos + 1: v = CHR$(x): PUT #6, , v
            END IF
            q = q + 3
           END IF
         END SELECT
      NEXT i, head, cyl
END SUB

FUNCTION cxfr (BYVAL dens)
      SELECT CASE (drvtyp - 1) * 8 + dens
       CASE 1: dr = 2
       CASE 2: dr = 1
       CASE 8: dr = 2
       CASE 9: dr = 1
       CASE 11: dr = 0
       CASE 17: dr = 2
       CASE 18: dr = 1
       CASE 25: dr = 2
       CASE 26: dr = 1
       CASE 28: dr = 0
       CASE ELSE: dr = -1
      END SELECT
      cxfr = dr
END FUNCTION

SUB dbtc (BYVAL n)
      POKE &H3E, PEEK(&H3E) AND &H7F
      FOR i = 1 TO n
       xax = e241(i2l(cmd(i)) * 256&)
       swinfdcproc 0, xax, xbx, xcx, xdx, xfl
       IF xfl AND 1 THEN
        IF xfl AND 64 THEN erflg = 1 ELSE erflg = 2
        EXIT FOR
       END IF
      NEXT i
END SUB

SUB dcpabs (q, f)
      qa = 0: f = 0
      FOR i = 0 TO 7
       POKE &H40, 0
       s = CHR$(0) + CHR$(0) + CHR$(1) + CHR$(3)
       dmabwr 0, s, 4
       cmd(1) = &H4D
       cmd(2) = hdd
       cmd(3) = 3
       cmd(4) = 1
       cmd(5) = 1
       cmd(6) = &H50
       dmach2 &H4A, 0, 4
       dbtc 6: IF erflg GOTO er6
       waint
       IF erflg GOTO er6
       n = csfc: IF erflg GOTO er6
       dmal = lensec(maxlen)
       cmd(1) = &H42
       cmd(2) = hdd
       cmd(6) = maxlen
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, dmal
       dbtc 9: IF erflg GOTO er6
       waint
       IF erflg GOTO er6
       n = csfc: IF erflg GOTO er6
       q = qtrln(0, dcrd): IF q = -1 THEN f = 1: EXIT FOR
       qa = qa + q
      NEXT i
      q = qa / 8

er6:
END SUB

SUB dcpbug (a)
      seq = MKL$(-1) + CHR$(3) + CHR$(1) + CHR$(dcrd)
      seq = seq + CHR$(3) + CHR$(1) + CHR$(1) + CHR$(1) + "ö" + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0)
      seq = seq + CHR$(4) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0) + MKL$(-1) + MKI$(128) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
      dwp -1
      IF erflg GOTO er9
      cmd(1) = &H46
      cmd(2) = hdd
      cmd(3) = 0
      cmd(4) = 0
      cmd(5) = 0
      cmd(6) = 0
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H42, 0, 16
      dbtc 9: IF erflg GOTO er9
      waint
      IF erflg GOTO er9
      n = csfc: IF erflg GOTO er9
      IF cstat(2) AND 1 THEN a = 1 ELSE a = 0: nobug = 1

er9:
END SUB

SUB dcpfwd (a, ff)
      qa = 0: xf = 0: ff = 0
      REDIM af(7)
      FOR i = 0 TO 7
       POKE &H40, 0
       s = CHR$(0) + CHR$(0) + CHR$(1) + CHR$(3)
       dmabwr 0, s, 4
       cmd(1) = &H4D
       cmd(2) = hdd
       cmd(3) = 3
       cmd(4) = 1
       cmd(5) = 1
       cmd(6) = &H50
       dmach2 &H4A, 0, 4
       dbtc 6: IF erflg GOTO er7
       waint
       IF erflg GOTO er7
       n = csfc: IF erflg GOTO er7
       seq = MKL$(-1) + CHR$(3) + CHR$(1) + CHR$(dcrd) + CHR$(2) + CHR$(3) + CHR$(1) + CHR$(3) + CHR$(1) + CHR$(&HAF) + CHR$(1) + CHR$(1) + MKI$(206) + CHR$(0) + CHR$(0) + CHR$(1) + CHR$(3)
       dwp -1
       IF erflg GOTO er7
       cmd(1) = &H42
       cmd(2) = hdd
       cmd(6) = 3
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, 1024
       dbtc 9: IF erflg GOTO er7
       waint
       IF erflg GOTO er7
       n = csfc: IF erflg GOTO er7
       s = STRING$(1024, 0): dmabrd 0, s, 1024
       fintran f, a, b, s
       IF f THEN
        IF xf = 3 THEN ff = 1: EXIT FOR
        xf = xf + 1: i = i - 1
       ELSE
        qa = qa + a
        af(i) = a
       END IF
      NEXT i
      a = qa / 8
      FOR i = 0 TO 7
       IF ABS(a - af(i)) > 2304 THEN ff = 1: EXIT FOR
      NEXT i
      ERASE af

er7:
END SUB

SUB dcpmain (s1, s2, s3, s4)
      dcptim q, f, xf: IF erflg GOTO er10
      IF xf THEN erwind "FDC is not compatible with i8272A; FDA cannot operate": EXIT SUB
      IF f THEN
dcp1:  erwind "Calibrate failure"
       EXIT SUB
      ELSE
       qcaltim(drive, dcrd) = q
       s1 = LTRIM$(STR$(q \ 16)) + "." + LTRIM$(STR$(q AND 15))
      END IF

      dcpabs q, f: IF erflg GOTO er10
      IF f GOTO dcp1
      qcalabs(drive, dcrd) = q
      s2 = LTRIM$(STR$(q \ 256)) + "." + CHR$((q \ 32 AND 7) + 48) + "." + LTRIM$(STR$(q AND 31))

      dcalfwd(drive, dcrd) = 0: dcalpos(drive, dcrd) = 0

      dcpfwd a, ff: IF erflg GOTO er10
      IF ff GOTO dcp1
      dcalfwd(drive, dcrd) = a
      s3 = LTRIM$(STR$(a \ 256)) + "." + CHR$((a \ 32 AND 7) + 48) + "." + LTRIM$(STR$(a AND 31))

      dcppos a, ff: IF erflg GOTO er10
      IF ff GOTO dcp1
      dcalpos(drive, dcrd) = a
      s4 = LTRIM$(STR$(a))

      dcpposid a, ff: IF erflg GOTO er10
      IF ff GOTO dcp1
      dcalposid(drive, dcrd) = a
      s4 = s4 + "/" + LTRIM$(STR$(a))

      dcpbug a: IF erflg GOTO er10
      s4 = s4 + "/"
      IF a THEN s4 = s4 + "Yes" ELSE s4 = s4 + "No"

er10:
END SUB

SUB dcppos (a, ff)
      q = timerf(0): htflg = 1
      ff = 1: a = 0
      FOR i = 0 TO 39
       s1pos
       IF erflg GOTO er8
       qgpl = CDBL(qtlen - (a - 146) * 256&) * qdrt / (qtlen * 16)
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = 0
       cmd(4) = 0
       cmd(5) = 1
       cmd(6) = 3
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H42, 0, 16
       WHILE timerf(2) < qgpl: WEND
       dbtc 9: IF erflg GOTO er8
       waint
       IF erflg GOTO er8
       qt = timerf(2)
       n = csfc: IF erflg GOTO er8
       a = a + 10
       IF qt * 16& < qdrt * 3 \ 2 THEN ff = 0: EXIT FOR
      NEXT i
      q = timerf(1): htflg = 0

er8:
END SUB

SUB dcpposid (a, ff)
      q = timerf(0): htflg = 1
      ff = 1: a = 0
      FOR i = 0 TO 18
       s1pos
       IF erflg GOTO er11
       cmd(1) = &H4A
       cmd(2) = hdd
       qgpl = CDBL(qtlen - (a - 146) * 256&) * qdrt / (qtlen * 16)
       WHILE timerf(2) < qgpl: WEND
       erflg = fdardid(VARPTR(cmd(1)), qt, VARPTR(cstat(0)))
       IF erflg GOTO er11
       a = a + 5
       IF qt * 16& < qdrt * 3 \ 2 THEN ff = 0: EXIT FOR
      NEXT i
      q = timerf(1): htflg = 0

er11:
END SUB

SUB dcptim (q1, f, xf)
      s = CHR$(0) + CHR$(0) + CHR$(1) + CHR$(3)
      dmabwr 0, s, 4
      cmd(1) = &H4D
      cmd(2) = hdd
      cmd(3) = 3
      cmd(4) = 1
      cmd(5) = 1
      cmd(6) = &H50
      dmach2 &H4A, 0, 4
      dbtc 6: IF erflg GOTO er5
      waint
      IF erflg GOTO er5
      n = csfc: IF erflg GOTO er5
      cmd(1) = &H42
      cmd(2) = hdd
      cmd(6) = 1
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      q1 = 0: f = 0: xf = 0
      q = timerf(0): htflg = 1
      FOR i = -1 TO 8
       POKE &H40, 0
       dmach2 &H42, 0, 16
       dbtc 9: IF erflg GOTO er5
       waint
       IF erflg GOTO er5
       qt = timerf(2)
       IF i = -1 THEN q = timerf(ctimer)
       n = csfc: IF erflg GOTO er5
       IF (cstat(0) AND &HC0) = &H80 THEN xf = 1: EXIT FOR
       IF cstat(1) AND 1 THEN f = 1: EXIT FOR
       IF i > 0 THEN
        qc = qt - qa
        IF qc < 0 THEN qc = qc + 65536
        q1 = q1 + qc
       END IF
       qa = qt
      NEXT i
      q = timerf(1): htflg = 0
      q1 = q1 * 2

er5:
END SUB

SUB dfpcalc (BYVAL noc, BYVAL nos, BYVAL nof, aflg, sform())
      FOR i = 0 TO nof - 1
       IF ASC(sform(i)) = noc AND ASC(MID$(sform(i), 2, 1)) = nos THEN aflg = 0: EXIT FOR
      NEXT i
      MID$(sform(nof), 1) = CHR$(noc) + CHR$(nos)
      f = 0
      IF noc = 40 AND nos = 8 THEN a = 2: b = 1: c = 112: d = &HFF: f = 1
      IF noc = 40 AND nos = 9 THEN a = 2: b = 2: c = 112: d = &HFD: f = 1
      IF noc = 80 AND nos = 9 THEN a = 2: b = 3: c = 112: d = &HF9: f = 1
      IF noc = 80 AND nos = 15 THEN a = 1: b = 7: c = 224: d = &HF9: f = 1
      IF f = 0 THEN
       a = 1: c = 224: d = &HF0
       n = noc * nos * 2 - 15
       b = 1
       DO
        x = n - b * 2
        y = x \ 2: IF x AND 1 THEN y = y + 1
        x = y * 3
        IF x <= b * 512 THEN EXIT DO
        b = b + 1
       LOOP
      END IF
      MID$(sform(nof), 3) = CHR$(a) + CHR$(b) + CHR$(c) + CHR$(d)
END SUB

SUB dftws (BYVAL dfflen, x, q, qoff)
      seq = CHR$(1) + CHR$(dfrd) + CHR$(3) + CHR$(nos) + CHR$(2) + CHR$(agpl) + "ö" + CHR$(0)
      FOR i = 1 TO nos
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(i) + CHR$(2)
      NEXT i
      noop = 2
      IF x < dfflen THEN
       seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(1) + CHR$(2) + CHR$(0) + MKL$(q) + MKI$(nos * 512) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
       noop = 3
      ELSE
       q = -1
      END IF
      seq = MKL$(q) + CHR$(noop) + seq
      qwrioffset(cyl, head) = qoff: dwrilen(cyl, head) = LEN(seq): qoff = qoff + LEN(seq)
      PUT #6, , seq
      x = x + nos
      q = q + nos * 512
END SUB

SUB dosform (dfflen, BYVAL anos, BYVAL csz, BYVAL fsz, BYVAL noe, BYVAL mdb, fsys)
      sf1 = "": sf2 = ""
      s = STRING$(512, 0)
      sf = STRING$(512, 0)
      GET #1, 1, sf
   IF fsys THEN
      x = absread(fsys - 1, 0, 1, sf): IF x THEN ERROR 57
      x = CVI(MID$(sf, 23, 2)) * ASC(MID$(sf, 17, 1)) + CVI(MID$(sf, 15, 2))
      y = CVI(MID$(sf, 18, 2)): n = y \ &H10: IF y AND &HF THEN n = n + 1
      f = 0
    FOR i = x TO x + n - 1
      x = absread(fsys - 1, i, 1, s): IF x THEN ERROR 57
     FOR j = 0 TO 15 * 32 STEP 32
      IF ASC(MID$(s, j + 1, 1)) = 0 THEN f = 2: EXIT FOR
      IF ASC(MID$(s, j + 12, 1)) AND 4 THEN
       IF f THEN
        sf2 = MID$(s, j + 1, 11)
        q2d = CVL(MID$(s, j + 23, 4)): q2l = CVL(MID$(s, j + 29, 4))
       ELSE
        sf1 = MID$(s, j + 1, 11)
        q1d = CVL(MID$(s, j + 23, 4)): q1l = CVL(MID$(s, j + 29, 4))
       END IF
       f = f + 1: IF f = 2 THEN EXIT FOR
      END IF
     NEXT j
      IF f = 2 THEN EXIT FOR
    NEXT i
      IF sf2 = "" THEN warwind "DOS system files not found": fsys = 0
   END IF
      OPEN sopdir + "DISK.BIN" FOR BINARY ACCESS WRITE AS #2
      n = noe \ &H10: IF noe AND &HF THEN n = n + 1
      x = fsz
      IF fsys THEN
       i = 512 * csz
       f1 = q1l \ i: IF q1l MOD i THEN f1 = f1 + 1
       f2 = q2l \ i: IF q2l MOD i THEN f2 = f2 + 1
      ELSE f1 = 0: f2 = 0
      END IF
      f = 0: q = 0
      dfflen = x * 2 + n + (f1 + f2) * csz + 1
      MID$(sf, 14) = CHR$(csz)
      MID$(sf, 15) = MKI$(1)
      MID$(sf, 17) = CHR$(2)
      MID$(sf, 18) = MKI$(noe)
      MID$(sf, 20) = MKI$(anos)
      MID$(sf, 22) = CHR$(mdb)
      MID$(sf, 23) = MKI$(x)
      MID$(sf, 25) = MKI$(nos)
      MID$(sf, 27) = MKI$(2)
      MID$(sf, 29) = MKL$(0)
      MID$(sf, 33) = MKL$(0)
      MID$(sf, 37) = MKI$(0)
      MID$(sf, 510) = CHR$(0)
      PUT #2, , sf
      sb = ""
      FOR i = 1 TO x
       s = STRING$(512, 0)
       IF i = 1 THEN
        MID$(s, 1) = CHR$(mdb) + MKI$(-1)
        IF fsys THEN
         FOR j = 2 TO f1 + 1
          k = j + 1: IF j = f1 + 1 THEN k = &HFF8
          p = (j \ 2) * 3
          x1 = ASC(MID$(s, p + 1, 1))
          x2 = ASC(MID$(s, p + 2, 1))
          x3 = ASC(MID$(s, p + 3, 1))
          IF j AND 1 THEN
           x2 = (x2 AND &HF) + (k AND &HF) * &H10
           x3 = k \ &H10
          ELSE
           x1 = k AND &HFF
           x2 = (x2 AND &HF0) + k \ &H100
          END IF
          MID$(s, p + 1) = CHR$(x1) + CHR$(x2) + CHR$(x3)
         NEXT j
         y = f2 + j - 1
         FOR j = j TO y
          k = j + 1: IF j = y THEN k = &HFF8
          p = (j \ 2) * 3
          x1 = ASC(MID$(s, p + 1, 1))
          x2 = ASC(MID$(s, p + 2, 1))
          x3 = ASC(MID$(s, p + 3, 1))
          IF j AND 1 THEN
           x2 = (x2 AND &HF) + (k AND &HF) * &H10
           x3 = k \ &H10
          ELSE
           x1 = k AND &HFF
           x2 = (x2 AND &HF0) + k \ &H100
          END IF
          MID$(s, p + 1) = CHR$(x1) + CHR$(x2) + CHR$(x3)
         NEXT j
        END IF
       END IF
       sb = sb + s
      NEXT i
      PUT #2, , sb
      PUT #2, , sb
      sb = ""
      FOR i = 1 TO n
       s = STRING$(512, 0)
       IF i = 1 AND fsys THEN MID$(s, 1) = sf1 + CHR$(&H27) + STRING$(10, 0) + MKL$(q1d) + MKI$(2) + MKL$(q1l) + sf2 + CHR$(&H27) + STRING$(10, 0) + MKL$(q2d) + MKI$(f1 + 2) + MKL$(q2l)
       sb = sb + s
      NEXT i
      PUT #2, , sb
      IF fsys THEN
       sf = sf1: q = q1l
       GOSUB wsf
       CLOSE #3
       sf = sf2: q = q2l
       GOSUB wsf
      END IF
      EXIT SUB
wsf:  x = csz * 512
      y = q \ x: IF q MOD x THEN y = y + 1
      q = CLNG(x) * CLNG(y)
      OPEN CHR$(64 + fsys) + ":\" + RTRIM$(LEFT$(sf, 8)) + "." + RTRIM$(RIGHT$(sf, 3)) FOR BINARY ACCESS READ AS #3
      sb = STRING$(16384, 0)
      DO
       IF q < 16384 THEN sb = STRING$(q, 0): q = 0 ELSE q = q - 16384
       GET #3, , sb
       PUT #2, , sb
       IF q = 0 THEN EXIT DO
      LOOP
      RETURN
END SUB

FUNCTION drp (boff, BYVAL mode)
      REDIM qtim(1 TO mns)
      q = timerf(0): htflg = 1
      s1pos
      IF erflg GOTO er1
      cmd(1) = &H4A: cmd(2) = hdd: xf = 0: x = 0
      qadd = (qdrt / 16) - dop * 16& * qdrt / qtlen
      qgpl = CDBL(qtlen - poscid * 256&) * qdrt / (qtlen * 16)
      WHILE timerf(2) < qgpl: WEND
      FOR i = 1 TO mns + 1
       erflg = fdardid(VARPTR(cmd(1)), qt, VARPTR(cstat(0)))
       IF erflg THEN EXIT FOR
       IF cstat(1) AND 1 THEN xf = 1: EXIT FOR
       IF i = 1 THEN qend = qt + qadd
       IF qt > qend THEN EXIT FOR
       IF i > mns THEN x = 1: EXIT FOR
       qtim(i) = qt
       spar(i).c = cstat(3)
       spar(i).h = cstat(4)
       spar(i).r = cstat(5)
       spar(i).n = cstat(6)
      NEXT i

      q = timerf(1): htflg = 0

      IF erflg GOTO er1
      IF xf THEN nos = 0: dtf = 0: EXIT FUNCTION
      IF x THEN nos = 0: dtf = 2: EXIT FUNCTION
      nos = i - 1

      FOR i = 1 TO nos
       spar(i).post = CDBL(qtim(i) - (qdrt / 16)) * qtlen / (qdrt * 16&) - 22
      NEXT i
      FOR i = 1 TO nos - 1
       spar(i).gpl = spar(i + 1).post - spar(i).post
      NEXT i
      spar(nos).gpl = drt - spar(nos).post + spar(1).post
      FOR i = 1 TO nos
       IF spar(i).gpl < 22 THEN xf = 1: EXIT FOR
      NEXT i
      IF xf THEN nos = 0: dtf = 1: EXIT FUNCTION
      IF nos > 1 THEN gpl1 = (spar(nos).post - spar(1).post) / (nos - 1)

      alen = spar(1).n
      IF alltrkns THEN dtf = 2: GOTO ptrk ELSE dtf = 0
      FOR i = 1 TO nos
       IF spar(i).c <> cyl THEN dtf = 1: EXIT FOR
       IF spar(i).h <> head THEN dtf = 1: EXIT FOR
       IF spar(i).r <> i THEN dtf = 1: EXIT FOR
       IF spar(i).n <> alen THEN dtf = 1: EXIT FOR
       IF i <> nos AND ABS(spar(i).gpl - gpl1) > dop THEN dtf = 1: EXIT FOR
      NEXT i
      IF dtf GOTO ptrk
      IF alen >= maxlen THEN dtf = 1: GOTO ptrk
      IF i2l(lensec(alen)) * nos > drt THEN dtf = 1: GOTO ptrk
      IF nos > 1 THEN
       agpl = gpl1 - gpltab(alen): x = 0
       IF agpl < 1 THEN x = 1 - agpl: agpl = 1
       IF agpl > 255 THEN x = agpl - 255: agpl = 255
       IF x > dop THEN dtf = 1: GOTO ptrk
      ELSE
       agpl = 1
      END IF
      
      '--------- çâ¥­¨¥ §®­ë ¤ ­­ëå á¥ªâ®à®¢ ¤®à®¦ª¨ cyl, head ------------

      cmd(1) = &H42
      cmd(2) = hdd
      cmd(6) = alen + 1
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 0, lensec(alen + 1)
      dbtc 9: IF erflg GOTO er1
      waint
      IF erflg GOTO er1
      n = csfc: IF erflg GOTO er1
      IF cstat(0) AND &HC0 THEN dtf = 1: GOTO ptrk
      ln = lensec(alen)
      s = STRING$(ln, 0): dmabrd ln, s, ln
      minln = spar(1).gpl - gpltab(alen) - dop
      maxln = spar(1).gpl - gpltab(alen) + dop
      a = fdaagap(SSEG(s), SADD(s), ln, minln, maxln)
      IF e122(a) < 1 THEN dtf = 1: GOTO ptrk
      IF e122(a) = 3 THEN
       x = e121(a)
       IF ABS(x - agpl) > dop THEN dtf = 1: GOTO ptrk
       agpl = x
      END IF
      spar(1).agr = a + alen * 4096
      cmd(1) = &H46
      cmd(2) = hdd
      cmd(3) = cyl
      cmd(4) = head
      cmd(5) = 1
      cmd(6) = alen
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      boff = nos * lensec(alen)
      dmach2 &H46, 0, boff
      dbtc 9: IF erflg GOTO er1
      waint
      IF erflg GOTO er1
      n = csfc: IF erflg GOTO er1
      IF cstat(1) AND &H25 OR cstat(2) AND &H61 THEN dtf = 1: GOTO ptrk
      boff = 0
      FOR i = 1 TO nos
       spar(i).csn = power(alen)
       spar(i).crc = power(alen)
       s = STRING$(lensec(alen), 0): dmabrd boff, s, lensec(alen)
       FOR j = 0 TO alen - 1
        a = crc16(s, lensec(j) + 2, 0)
        spar(i).csn = spar(i).csn OR power(j)
        IF a = 0 THEN spar(i).crc = spar(i).crc OR power(j)
       NEXT j
       spar(i).offset = boff
       spar(i).datlen = lensec(alen)
       boff = boff + lensec(alen)
      NEXT i
      drp = 0: EXIT FUNCTION

ptrk: q = timerf(0): htflg = 1

      FOR lmax = 0 TO maxlen
       IF gpltab(lmax) >= spar(1).gpl THEN EXIT FOR
      NEXT lmax
      IF spar(1).n < 8 AND spar(1).n > lmax AND mode = 0 THEN lmax = spar(1).n
      cmd(1) = &H42
      cmd(2) = hdd
      cmd(6) = lmax
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 0, lensec(lmax)
      dbtc 9: IF erflg GOTO er1
      waint
      IF erflg GOTO er1
      n = csfc: IF erflg GOTO er1
      IF cstat(2) AND &H40 THEN spar(1).dt = 1 ELSE spar(1).dt = 0
      IF cstat(1) AND 1 THEN spar(1).dt = 2
      spar(1).csn = spar(1).csn OR power(lmax)
      IF (cstat(2) AND &H20) = 0 THEN spar(1).crc = spar(1).crc OR power(lmax)
      IF spar(1).dt = 2 THEN boff = 0: spar(1).offset = -1 ELSE boff = lensec(lmax): spar(1).datlen = boff

      xf = 0
      FOR i = 1 TO nos
       FOR j = i + 1 TO nos
        IF spar(i).c = spar(j).c AND spar(i).h = spar(j).h AND spar(i).r = spar(j).r AND spar(i).n = spar(j).n THEN xf = 1: EXIT FOR
       NEXT j
       IF xf THEN EXIT FOR
      NEXT i

      f = 0
      FOR i = 2 TO nos
       IF xf THEN
        s1pos
        IF erflg GOTO er1
        q = (spar(i).post - posc) * 256&
        IF q < 0 THEN q = q + qtlen
        qgpl = CDBL(q) * qdrt / (qtlen * 16)
       END IF
       GOSUB rdsec: IF f THEN EXIT FOR
      NEXT i

      q = timerf(1): htflg = 0
      IF f THEN drp = 1: EXIT FUNCTION
      FOR i = 1 TO nos
       IF spar(i).dt = 2 GOTO nodi
       x = spar(i).datlen
       s = STRING$(x, 0): dmabrd spar(i).offset, s, x
       spar(i).sfb = CVI(LEFT$(s, 2))
       spar(i).dif = findif(s, x)
       FOR j = 0 TO maxlen
        IF lensec(j) = x THEN EXIT FOR
        a = crc16(s, lensec(j) + 2, spar(i).dt)
        spar(i).csn = spar(i).csn OR power(j)
        IF a = 0 THEN spar(i).crc = spar(i).crc OR power(j)
       NEXT j
       a = 0
       FOR j = maxlen TO 0 STEP -1
        IF lensec(j) > x GOTO nogpi
        IF lensec(j) < spar(i).dif THEN EXIT FOR
        ln = x - lensec(j)
        s = STRING$(ln, 0): dmabrd spar(i).offset + lensec(j), s, ln
        minln = spar(i).gpl - gpltab(j) - dop
        maxln = spar(i).gpl - gpltab(j) + dop
        a = fdaagap(SSEG(s), SADD(s), ln, minln, maxln)
        IF a THEN a = a + j * 4096: EXIT FOR
nogpi: NEXT j
       spar(i).agr = a
nodi: NEXT i
      drp = 0: EXIT FUNCTION

rdsec:
      ls = spar(i).n: IF ls > 7 THEN ls = 7
      IF mode THEN
       FOR lmax = 0 TO maxlen
        IF gpltab(lmax) >= spar(i).gpl THEN EXIT FOR
       NEXT lmax
       IF ls > lmax THEN ls = lmax
      END IF
      IF lensec(ls) > 31744 - boff THEN f = 1: RETURN
      cmd(1) = &H46
      cmd(2) = hdd
      cmd(3) = spar(i).c
      cmd(4) = spar(i).h
      cmd(5) = spar(i).r
      cmd(6) = spar(i).n
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, boff, lensec(ls)
      IF xf THEN WHILE timerf(2) < qgpl: WEND
      dbtc 9: IF erflg GOTO er1
      waint
      IF erflg GOTO er1
      n = csfc: IF erflg GOTO er1
      IF cstat(1) AND 5 THEN spar(i).dt = 2: spar(i).offset = -1: RETURN
      IF spar(i).n < maxlen THEN
       spar(i).csn = spar(i).csn OR power(spar(i).n)
       IF (cstat(2) AND &H20) = 0 THEN spar(i).crc = spar(i).crc OR power(spar(i).n)
      END IF
      IF cstat(2) AND &H40 THEN spar(i).dt = 1
      spar(i).offset = boff
      boff = boff + lensec(ls): spar(i).datlen = lensec(ls)
      f = 0: RETURN
er1:
END FUNCTION

FUNCTION drttbl (BYVAL dens)
      SELECT CASE dens
       CASE 0: dr = 2: a = 6
       CASE 1: dr = 2: a = 5
       CASE 2: dr = 1: a = 5
       CASE 3: dr = 0: a = 6
       CASE 4: dr = 0: a = 5
      END SELECT
      nomcalc dr, a, qntl, qndrt
      drttbl = qntl / 256
END FUNCTION

SUB dwp (BYVAL ql)
      q = timerf(0): htflg = 1
      posit = 6
      FOR i = 1 TO ASC(MID$(seq, 5, 1))
       opc = ASC(MID$(seq, posit, 1))
       posit = posit + 1
       SELECT CASE opc
        CASE 0
         posit = posit + ASC(MID$(seq, posit, 1)) + 1
        CASE 1
         dens = ASC(MID$(seq, posit, 1))
         posit = posit + 1
         x = ixfr(dens, 2)
        CASE 2
         s1pos
         IF erflg GOTO er2
        CASE 3
         nosf = ASC(MID$(seq, posit, 1))
         lsf = ASC(MID$(seq, posit + 1, 1))
         gplf = ASC(MID$(seq, posit + 2, 1))
         fb = ASC(MID$(seq, posit + 3, 1))
         aflg = ASC(MID$(seq, posit + 4, 1))
         posit = posit + 5
         IF aflg THEN
          xa = ASC(MID$(seq, posit, 1))
          ca = CVI(MID$(seq, posit + 1, 2))
          posit = posit + 3
         END IF
         si = ""
         FOR j = 1 TO nosf
          si = si + MID$(seq, posit, 4)
          posit = posit + 4
         NEXT j
         IF aflg THEN
          qabt = ca * 256& + xa * qtlen - fwd
          IF qabt < 0 THEN qabt = qabt + qtlen
          qabt = CDBL(qabt) * qdrt / (qtlen * 16)
         END IF
         cmd(1) = &H4D
         cmd(2) = hdd
         cmd(3) = lsf
         cmd(4) = nosf
         cmd(5) = gplf
         cmd(6) = fb
         dmabwr 0, si, LEN(si)
         dmach2 &H4A, 0, nosf * 4
         dbtc 6: IF erflg GOTO er2
         IF aflg = 0 THEN
          waint
          IF erflg GOTO er2
          n = csfc: IF erflg GOTO er2
         ELSE
          WHILE timerf(2) < qabt: WEND
          abt
          IF erflg GOTO er2
         END IF
        CASE 4
         idc = ASC(MID$(seq, posit, 1))
         idh = ASC(MID$(seq, posit + 1, 1))
         idr = ASC(MID$(seq, posit + 2, 1))
         idn = ASC(MID$(seq, posit + 3, 1))
         dt = ASC(MID$(seq, posit + 4, 1))
         qoff = CVL(MID$(seq, posit + 5, 4))
         dmal = CVI(MID$(seq, posit + 9, 2))
         dtl = ASC(MID$(seq, posit + 11, 1))
         cmd7 = ASC(MID$(seq, posit + 12, 1))
         cmd8 = ASC(MID$(seq, posit + 13, 1))
         pflg = ASC(MID$(seq, posit + 14, 1))
         posit = posit + 15
         IF pflg THEN
          xp = ASC(MID$(seq, posit, 1))
          cp = CVI(MID$(seq, posit + 1, 2))
          posit = posit + 3
         END IF
         aflg = ASC(MID$(seq, posit, 1))
         posit = posit + 1
         IF aflg THEN
          xa = ASC(MID$(seq, posit, 1))
          ca = CVI(MID$(seq, posit + 1, 2))
          posit = posit + 3
         END IF
         q = &H7FFFFFFF
         IF pflg THEN
          qpos = (cp - posc) * 256& + xp * qtlen
          IF qpos < q THEN q = qpos
         END IF
         IF aflg THEN
          qabt = ca * 256& + xa * qtlen - fwd
          IF qabt < q THEN q = qabt
         END IF
         IF q < 0 THEN q = qtlen ELSE q = 0
         IF pflg THEN qpos = CDBL(qpos + q) * qdrt / (qtlen * 16)
         IF aflg THEN qabt = CDBL(qabt + q) * qdrt / (qtlen * 16)
         IF dt THEN cmd(1) = &H49 ELSE cmd(1) = &H45
         cmd(2) = hdd
         cmd(3) = idc
         cmd(4) = idh
         cmd(5) = idr
         cmd(6) = idn
         cmd(7) = cmd7
         cmd(8) = cmd8
         cmd(9) = dtl
         IF qoff = -1 THEN x = 0 ELSE x = qoff - ql
         dmach2 &H4A, x, dmal
         IF pflg THEN WHILE timerf(2) < qpos: WEND
         dbtc 9: IF erflg GOTO er2
         IF aflg = 0 THEN
          waint
          IF erflg GOTO er2
          n = csfc: IF erflg GOTO er2
         ELSE
          WHILE timerf(2) < qabt: WEND
          abt
          IF erflg GOTO er2
         END IF
       END SELECT
      NEXT i
      q = timerf(1): htflg = 0

er2:
END SUB

FUNCTION editstr (BYVAL brow, BYVAL bcol, BYVAL l, BYVAL attr)
      x = LEN(stred): es = x
      DO
       moff
       mkwind brow, bcol, 1, l, attr
       fprint brow, bcol, stred, attr
       mon
       cursor brow, bcol + x
       DO: mkey f, a, b, bs: LOOP UNTIL f
       IF f = 1 THEN
        SELECT CASE a
         CASE 0
          SELECT CASE b
           CASE &HF: editstr = 2: EXIT DO
           CASE &H3B: editstr = 4: EXIT DO
           CASE &H47: x = 0
           CASE &H4B: IF x THEN x = x - 1
           CASE &H4D: IF x < es THEN x = x + 1
           CASE &H4F: x = es
           CASE &H53
            IF x < es THEN
             stred = LEFT$(stred, x) + RIGHT$(stred, es - x - 1)
             es = es - 1
            END IF
          END SELECT
         CASE 8
          IF x THEN
           x = x - 1
           stred = LEFT$(stred, x) + RIGHT$(stred, es - x - 1)
           es = es - 1
          END IF
         CASE 9: editstr = 1: EXIT DO
         CASE &HD: editstr = 0: EXIT DO
         CASE &H1B: editstr = 3: EXIT DO
         CASE 32 TO 127
          IF es + 1 < l THEN
           stred = LEFT$(stred, x) + UCASE$(CHR$(a)) + RIGHT$(stred, es - x)
           x = x + 1: es = es + 1
          END IF
        END SELECT
       ELSE
        IF a <> brow OR b < bcol OR b > bcol + l THEN
         editstr = 5: mpr = a: mpc = b: EXIT DO
        END IF
        x = b - bcol: IF x > es THEN x = es
       END IF
      LOOP
      curoff
END FUNCTION

FUNCTION eggs (BYVAL br, BYVAL bc, BYVAL nr, BYVAL nc, opt, BYVAL an, BYVAL ab)
      l = opt: moff
egs1: cursor br + l, bc + 1
      chattr br + l, bc, 3, ab
      mon
      DO: mkey f, a, b, bs: LOOP UNTIL f
      moff
      chattr br + l, bc, 3, an
      IF f = 1 THEN
       SELECT CASE a
        CASE 0
         SELECT CASE b
          CASE &HF: eggs = 2: GOTO ex1
          CASE &H3B: eggs = 4: GOTO ex1
          CASE &H48
           IF l THEN l = l - 1
           GOTO egs1
          CASE &H50
           IF l <> nr - 1 THEN l = l + 1
           GOTO egs1
         END SELECT
        CASE 9: eggs = 1: GOTO ex1
        CASE &HD: eggs = 0: GOTO ex1
        CASE &H1B: eggs = 3: GOTO ex1
        CASE &H20
egs2:    mkwind br, bc + 1, nr, 1, an
         opt = l: fprint br + l, bc + 1, CHR$(7), an
         GOTO egs1
       END SELECT
      ELSE
       IF a < br OR a > br + nr - 1 OR b < bc OR b > bc + nc - 1 THEN eggs = 5: mpr = a: mpc = b: GOTO ex1
       l = a - br: GOTO egs2
      END IF
      GOTO egs1
ex1:  curoff: mon
END FUNCTION

SUB erwind (s)
      msav
      moff
      post = (80 - LEN(s)) \ 2
      wincre 9, post - 3, 7, LEN(s) + 6, "Error", 2
      fprint 11, post, s, 7
      mkbut 13, 35, 10, "Ok", 1, 12, 18
      mon
erw1: f = 0: WHILE f = 0: mkey f, a, b, bs: WEND
      IF f = 1 THEN
       IF a = 13 OR a = 27 GOTO erw2
      ELSE
       IF a = 13 AND b > 34 AND b < 46 GOTO erw2
      END IF
      GOTO erw1
erw2: moff: winoff: mrst
END SUB

SUB fintran (f, a, b, s)
      f = 0
      x = findif(s, 1024)
      IF x > 90 THEN f = 1: EXIT SUB
      a = ASC(MID$(s, x, 1)): b = ASC(MID$(s, x + 1, 1))
      c = ASC(MID$(s, x + 2, 1)): d = ASC(MID$(s, x + 3, 1))
      x = x * 8: y = x
      IF c <> d THEN f = 2: EXIT SUB
      IF b = c THEN
       m = a XOR b: IF m = 0 THEN f = 3: EXIT SUB
       n = m
       FOR i = 0 TO 7
        IF n AND 128 THEN EXIT FOR
        n = n * 2
       NEXT i
       y = y + i
       FOR i = 0 TO 7
        IF m AND 1 THEN EXIT FOR
        m = m \ 2
       NEXT i
       x = x - i
      ELSE
       n = a XOR b: IF n = 0 THEN f = 4: EXIT SUB
       FOR i = 0 TO 7
        IF n AND 128 THEN EXIT FOR
        n = n * 2
       NEXT i
       y = y + i
       m = b XOR c: IF m = 0 THEN f = 5: EXIT SUB
       FOR i = 0 TO 7
        IF m AND 1 THEN EXIT FOR
        m = m \ 2
       NEXT i
       x = x - i + 8
       IF x > y THEN SWAP x, y
      END IF
      x = x * 16: y = y * 16
      a = x + y: b = y * 2 - a
END SUB

SUB fmtrst
      SEEK #5, qidoffset(cyl, head) + 1
      v = STRING$(9, 0): GET #5, , v
      dtf = ASC(v)
      dot = ASC(MID$(v, 2, 1))
      nos = ASC(MID$(v, 3, 1))
      alen = ASC(MID$(v, 4, 1))
      agpl = ASC(MID$(v, 5, 1))
      dop = ASC(MID$(v, 6, 1))
      abtkid = ASC(MID$(v, 7, 1)): IF abtkid > 127 THEN abtkid = abtkid - 256
      abtcrc = ASC(MID$(v, 8, 1)): IF abtcrc > 127 THEN abtcrc = abtcrc - 256
      bdmpos = ASC(MID$(v, 9, 1))
      FOR i = 1 TO nos
       v = STRING$(23, 0): GET #5, , v
       spar(i).c = ASC(v)
       spar(i).h = ASC(MID$(v, 2, 1))
       spar(i).r = ASC(MID$(v, 3, 1))
       spar(i).n = ASC(MID$(v, 4, 1))
       spar(i).gpl = CVI(MID$(v, 5, 2))
       spar(i).post = CVI(MID$(v, 7, 2))
       spar(i).csn = ASC(MID$(v, 9, 1))
       spar(i).crc = ASC(MID$(v, 10, 1))
       spar(i).dt = ASC(MID$(v, 11, 1))
       spar(i).offset = CVL(MID$(v, 12, 4))
       spar(i).datlen = CVI(MID$(v, 16, 2))
       spar(i).dif = CVI(MID$(v, 18, 2))
       spar(i).sfb = CVI(MID$(v, 20, 2))
       spar(i).agr = CVI(MID$(v, 22, 2))
      NEXT i
END SUB

FUNCTION getbioserr
      swinfdcproc 30, xax, xbx, xcx, xdx, xfl
      getbioserr = e121(xdx)
END FUNCTION

SUB hdpos (BYVAL cyl)
      xdx = drive
      xcx = cyl * nods * 256
      swinfdcproc 24, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 THEN
       IF xfl AND 128 THEN
        erflg = 3
       ELSE
        IF xfl AND 64 THEN erflg = 1 ELSE erflg = 2
       END IF
      END IF
END SUB

SUB hlp (BYVAL helpnum)
      IF hlpflg = 0 THEN EXIT SUB
      clx = helpnum
      OPEN spath + "FDA.HLP" FOR INPUT AS #4
      moff
      wincre 2, 3, 20, 74, "Help", 3
      mkbord1 5, 6, 13, 68, 7
      fprint 6, 73, CHR$(24), 5: mkwind 7, 73, 9, 1, 5: fprint 16, 73, CHR$(25), 5
      mkbut 19, 21, 10, "Next", 1, 14, 18
      mkbut 19, 34, 10, "Prev", 0, 16, 18
      mkbut 19, 47, 10, "Cancel", 0, 16, 18
h6:   SEEK #4, 1: f = 0
      DO
       LINE INPUT #4, v
       IF LEFT$(v, 3) <> "***" GOTO h5
       IF MID$(v, 5) = "E" THEN f = 1: EXIT DO
       IF VAL(MID$(v, 5)) = clx THEN EXIT DO
h5:   LOOP
      IF f GOTO h4
      mkwind 4, 6, 1, 68, 9: mkwind 6, 8, 11, 64, 7
      LINE INPUT #4, v: fprint 4, 6, v, 9
      FOR i = 6 TO 16
       LINE INPUT #4, v: IF LEFT$(v, 3) = "***" THEN EXIT FOR
       fprint i, 8, v, 7
      NEXT i
      mon
h1:   x = buttons(19, 21, 10, 3, 12, 14, 16)
      ON x + 1 GOTO h1, h1, h4, h4, h1, h2, h3, h4
h2:   IF clx = 20 THEN SOUND 150, 1: GOTO h1 ELSE clx = clx + 1: moff: GOTO h6
h3:   IF clx = 0 THEN SOUND 150, 1: GOTO h1 ELSE clx = clx - 1: moff: GOTO h6
h4:   CLOSE #4: moff: winoff: mon
END SUB

SUB hrp (BYVAL ftyp)
      PRINT #1, ";" + STRING$(22, 32) + "*** Floppy Disk Analyser v7.2 ***"
      SELECT CASE ftyp
       CASE 0: PRINT #1, ";" + STRING$(29, 32) + "*** Disk Report ***"
       CASE 1: PRINT #1, ";" + STRING$(29, 32) + "*** Track Report ***"
       CASE 3: PRINT #1, ";" + STRING$(25, 32) + "*** Track Write Sequence ***"
      END SELECT
      PRINT #1, "; Number of Cylinders:" + STR$(ncyl)
END SUB

FUNCTION init (BYVAL mode)
SHARED initfirst
      IF drive THEN drvtyp = drvbtyp ELSE drvtyp = drvatyp
      SELECT CASE drvtyp
       CASE 1: a = 42: b = 6
       CASE 2: a = 84: b = 11
       CASE 3: a = 82: b = 6
       CASE 4: a = 82: b = 22
      END SELECT
      nods = a \ ncyl
      IF nods < 1 GOTO opc
      IF diskdens AND NOT b GOTO opc

      fdahook
      hvflg = 1
      bioserr = getbioserr
      IF bioserr THEN initfirst = 0
      IF initfirst = 0 THEN
       swinfdcproc 22, xax, xbx, xcx, xdx, xfl
       IF xfl AND 1 GOTO er3
       initfirst = 255
      END IF
      xdx = 511                                 ' WD37C65 chip bug - do not remove!
      swinfdcproc 10, xax, xbx, xcx, xdx, xfl   ' WD37C65 chip bug - do not remove!
      bioserr = 0
      POKE &H40, 0
      xdx = drive
      swinfdcproc 6, xax, xbx, xcx, xdx, xfl
      IF drvtyp <> 1 THEN
       swinfdcproc 8, xax, xbx, xcx, xdx, xfl
       IF xfl AND 1 GOTO er3
       dskchlin = e122(xax)
      ELSE
       dskchlin = 5
      END IF
      IF dskchlin = 128 THEN erflg = 4: EXIT FUNCTION
      dsd = dsd0
      xdx = dsd * 16 + 527
      swinfdcproc 18, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 GOTO er3
      IF mode THEN
       cmd(1) = 4
       cmd(2) = drive
       dbtc 2: IF erflg THEN EXIT FUNCTION
       n = csfc: IF erflg THEN EXIT FUNCTION
       IF cstat(0) AND &H40 THEN erflg = 5: EXIT FUNCTION
       xdx = drive
       swinfdcproc 26, xax, xbx, xcx, xdx, xfl
      END IF
      POKE &H40, 0
      init = 0: EXIT FUNCTION

er3:  IF xfl AND 128 THEN
       erflg = 3
      ELSE
       IF xfl AND 64 THEN erflg = 1 ELSE erflg = 2
      END IF
      EXIT FUNCTION
opc:  erwind "Number of cylinders and/or recording density not supported by drive": init = 1
END FUNCTION

FUNCTION ixfr (BYVAL dens, BYVAL m)
SHARED initfirst
      dr = cxfr(dens): IF dr = -1 GOTO opc2
      IF drvtyp = 2 THEN a = 6 ELSE a = 5
      nomcalc dr, a, qntl, qndrt
      SELECT CASE m
       CASE 0
        qtlen = qntl
        qdrt = qndrt
        fwd = 0
        posc = 0
        poscid = 0
       CASE 1
        qtlen = qntl
        qdrt = qcaltim(drive, dens)
        fwd = dcalfwd(drive, dens)
        posc = dcalpos(drive, dens)
        poscid = dcalposid(drive, dens)
        dop = ctdop(dr): abtkid = ctabtkid(dr): abtcrc = ctabtcrc(dr): bdmpos = ctbdmpos(dr)
       CASE 2
        qtlen = qcalabs(drive, dens)
        qdrt = qcaltim(drive, dens)
        fwd = dcalfwd(drive, dens)
        posc = dcalpos(drive, dens)
        poscid = dcalposid(drive, dens)
      END SELECT
      drt = qtlen / 256
      IF drt < gpltab(6) THEN maxlen = 6 ELSE maxlen = 7
      xdx = dr * 256 + 255
      swinfdcproc 10, xax, xbx, xcx, xdx, xfl
      ixfr = 0: EXIT FUNCTION
opc2: ixfr = 1
END FUNCTION

SUB ldfdfil (sform(), nof)
      FOR i = 0 TO nof - 1
       sform(i) = STRING$(6, 0)
       GET #1, , sform(i)
      NEXT i
      sform(nof) = STRING$(6, 0)
END SUB

SUB mkbut (BYVAL br, BYVAL bc, BYVAL nc, s, BYVAL bt, BYVAL ab, BYVAL at)
      sb = " ": se = " ": IF bt THEN sb = CHR$(16): se = CHR$(17)
      pb = (nc - LEN(s)) \ 2 - 1
      fprint br, bc, sb + STRING$(pb, 32) + s + STRING$(nc - pb - LEN(s) - 2, 32) + se, ab
      fprint br + 1, bc + 1, STRING$(nc, 223), at: fprint br, bc + nc, CHR$(220), at
END SUB

FUNCTION msel (BYVAL br, BYVAL bc, BYVAL nr, BYVAL nc, BYVAL nm, BYVAL edf, la(), BYVAL dbcl, BYVAL an, BYVAL ai, BYVAL ab, cl)
      af = 0: moff
scr1: chattr br + cl, bc, nc, ai: mon
scr2: IF af = 0 THEN
       DO: mkey f, a, b, bs: LOOP UNTIL f
      ELSE
       af = 0: mkey f, a, b, bs
       IF bs AND f = 0 THEN
        f = 2: d = 4: IF p THEN d = 2 ELSE p = 1: f = 0: af = 1
        POKE &H40, 255: WHILE PEEK(&H40) > 255 - d: WEND
       ELSE p = 0
       END IF
       IF f = 0 THEN GOTO scr2
      END IF
      moff
      IF df AND f = 2 AND 255 - PEEK(&H40) < 10 THEN msel = 5: GOTO ex2
      df = 0
      chattr br + cl, bc, nc, an
      IF f = 1 THEN
       SELECT CASE a
        CASE 0
         SELECT CASE b
          CASE &HF: msel = 2: GOTO ex2
          CASE &H3B: msel = 4: GOTO ex2
          CASE &H47: IF edf = 0 THEN cl = 0: GOTO scr1
           FOR i = 0 TO nm - 1
            IF la(i) THEN EXIT FOR
           NEXT i
           cl = i: GOTO scr1
          CASE &H48
scr3:      IF edf = 0 THEN
            IF cl THEN cl = cl - 1
            GOTO scr1
           END IF
           f = 0
           FOR i = cl TO 0 STEP -1
            IF la(i) THEN cl = i: f = f + 1: IF f = 2 THEN EXIT FOR
           NEXT i
           GOTO scr1
          CASE &H4F: IF edf = 0 THEN cl = nm - 1: GOTO scr1
           FOR i = nm - 1 TO 0 STEP -1
            IF la(i) THEN EXIT FOR
           NEXT i
           cl = i: GOTO scr1
          CASE &H50
scr4:      IF edf = 0 THEN
            IF cl <> nm - 1 THEN cl = cl + 1
            GOTO scr1
           END IF
           f = 0
           FOR i = cl TO nm - 1
            IF la(i) THEN cl = i: f = f + 1: IF f = 2 THEN EXIT FOR
           NEXT i
           GOTO scr1
         END SELECT
        CASE 9: msel = 1: GOTO ex2
        CASE &HD: msel = 0: GOTO ex2
        CASE &H1B: msel = 3: GOTO ex2
       END SELECT
      ELSE
       IF a = br AND b = bc + nc THEN af = 1: GOTO scr3
       IF a = br + nr - 1 AND b = bc + nc THEN af = 1: GOTO scr4
       IF a < br - 1 OR a > br + nm OR b < bc + 1 OR b > bc + nc THEN msel = 6: mpr = a: mpc = b: GOTO ex2
       IF a = br - 1 OR a = br + nm OR b = bc - 1 OR b = bc + nc GOTO scr1
       IF edf = 0 THEN cl = a - br: GOTO scr5
       IF la(a - br) THEN cl = a - br
scr5:  IF dbcl THEN df = 1: POKE &H40, 255
      END IF
      GOTO scr1
ex2:  chattr br + cl, bc, nc, ab: mon
END FUNCTION

SUB nomcalc (BYVAL dr, BYVAL a, qntl, qndrt)
      SELECT CASE dr
       CASE 0: q = 500000
       CASE 1: q = 300000
       CASE 2: q = 250000
      END SELECT
      qntl = q * 32 / a
      qndrt = 1193180 * 16 / a
END SUB

FUNCTION power (BYVAL num)
      SELECT CASE num
       CASE 0: power = 1
       CASE 1: power = 2
       CASE 2: power = 4
       CASE 3: power = 8
       CASE 4: power = 16
       CASE 5: power = 32
       CASE 6: power = 64
       CASE 7: power = 128
       CASE ELSE: power = 0
      END SELECT
END FUNCTION

FUNCTION prowind (s)
      msav
      moff
      post = (80 - LEN(s)) \ 2
      wincre 9, post - 3, 7, LEN(s) + 6, "Warning", 2
      fprint 11, post, s, 7
      mkbut 13, 28, 10, "Ok", 1, 14, 18
      mkbut 13, 41, 10, "Cancel", 0, 16, 18
      mon
prw1: x = buttons(13, 28, 10, 2, 12, 14, 16)
      ON x + 1 GOTO prw1, prw1, prw3, prw1, prw1, prw2, prw3
prw2: x = 0
prw3: moff: winoff: mrst: prowind = x
END FUNCTION

FUNCTION qtrln (BYVAL boff, BYVAL dens)
      x = drttbl(dens) - 300
      a = fdaamf(dmabseg, e241(i2l(dmaboff) + boff + x), 512, 3)
      IF a = -1 THEN qtrln = -1: EXIT FUNCTION
      y = x + 516
      qtrln = (y * 8& - a) * 32&
END FUNCTION

SUB s1pos
      idc = 136
      idh = 148
      idr = 74
      idn = 1
      DO
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = idc
       cmd(4) = idh
       cmd(5) = idr
       cmd(6) = idn
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H42, 0, 16
       erflg = fdaspos(VARPTR(cmd(1)), VARPTR(cstat(0)))
       IF erflg GOTO er4
       IF cstat(1) AND 5 THEN EXIT DO
       idr = idr + 1
       IF idr = 256 THEN idr = 0: idh = idh + 1
       IF idh = 256 THEN idh = 0: idc = idc + 1
       IF idc = 256 THEN idc = 0
      LOOP

er4:
END SUB

SUB scanfmtfile
      q = 10
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        qidoffset(cyl, head) = q
        SEEK #5, q + 3: v = " ": GET #5, , v: nos = ASC(v)
        q = q + nos * 23 + 9
      NEXT head, cyl
END SUB

SUB scanwrifile
      q = 10
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        qwrioffset(cyl, head) = q
        SEEK #6, q + 5: v = " ": GET #6, , v: noop = ASC(v)
        q = q + 5
        FOR i = 1 TO noop
         SEEK #6, q + 1: v = " ": GET #6, , v
         SELECT CASE ASC(v)
          CASE 0
           SEEK #6, q + 2: v = " ": GET #6, , v
           q = q + ASC(v) + 2
          CASE 1
           q = q + 2
          CASE 2
           q = q + 1
          CASE 3
           SEEK #6, q + 2: v = " ": GET #6, , v: nsec = ASC(v)
           SEEK #6, q + 6: v = " ": GET #6, , v: f = ASC(v)
           q = q + 6
           IF f THEN q = q + 3
           q = q + nsec * 4
          CASE 4
           SEEK #6, q + 16: v = " ": GET #6, , v: f = ASC(v)
           q = q + 16
           IF f THEN q = q + 3
           SEEK #6, q + 1: v = " ": GET #6, , v: f = ASC(v)
           q = q + 1
           IF f THEN q = q + 3
         END SELECT
        NEXT i
        dwrilen(cyl, head) = q - qwrioffset(cyl, head)
      NEXT head, cyl
END SUB

SUB sqst (f, noe, sf, s3)
      posit = 1: sf = "": noe = 0: f = 0
      IF s3 = "" THEN EXIT SUB
      DO
       xf = 0
       IF MID$(s3, posit, 1) = "R" THEN xf = 1
       IF MID$(s3, posit, 1) = "F" THEN xf = 2
       IF xf THEN posit = posit + 1
       sf = sf + CHR$(xf)
       a = 1: b = nos: GOSUB lnum: IF f THEN EXIT DO
       sf = sf + CHR$(x)
       IF y = 2 THEN
        IF xf THEN f = 1: EXIT DO
        posit = posit + 1
        a = 1: b = 16384: GOSUB lnum: IF f THEN EXIT DO
        sf = sf + MKI$(x)
       ELSE
        sf = sf + MKI$(0)
       END IF
       noe = noe + 1
       SELECT CASE y
        CASE 0: EXIT DO
        CASE 1: posit = posit + 1
        CASE 2: f = 1: EXIT DO
       END SELECT
      LOOP
      EXIT SUB
lnum: x = 0: p = 0
      DO
       s = MID$(s3, posit, 1)
       IF s = "" THEN y = 0: EXIT DO
       SELECT CASE ASC(s)
        CASE 44: y = 1: EXIT DO
        CASE 47: y = 2: EXIT DO
        CASE 48 TO 57
         IF p = 5 THEN f = 1: EXIT DO
         posit = posit + 1: p = p + 1
         q = x * 10 + ASC(s) - 48
         IF q > b THEN f = 1: EXIT DO ELSE x = q
        CASE ELSE: f = 1: EXIT DO
       END SELECT
      LOOP
      IF p = 0 THEN f = 1
      IF x < a OR x > b THEN f = 1
      RETURN
END SUB

SUB stopmot
      xdx = bioserr
      swinfdcproc 28, xax, xbx, xcx, xdx, xfl
      IF bioserr <> 0 AND bioserr <> 255 THEN
       abt
       IF erflg THEN erflg = 0 ELSE bioserr = 0: stopmot
      END IF
      bioserr = 255
      IF hvflg THEN fdaunh: hvflg = 0
END SUB

SUB temppath
      FOR i = 1 TO 4
       SELECT CASE i
        CASE 1: s = "TEMP"
        CASE 2: s = "TMP"
        CASE 3: s = "TEMPFILES"
        CASE 4: s = "TMPFILES"
       END SELECT
       sf = ENVIRON$(s)
       IF sf <> "" THEN stemp = sf: EXIT FOR
      NEXT i
      IF stemp <> "" THEN
       IF RIGHT$(stemp, 1) <> "\" THEN stemp = stemp + "\"
      ELSE
       ql = -1: xf = 0
       FOR i = 1 TO 26
        regs.ax = &H4409
        regs.bx = i
        interruptx &H21, regs, regs
        IF regs.flags AND 1 THEN
         IF regs.ax = 1 GOTO tmploc ELSE GOTO tmpin
        END IF
        IF regs.ax AND &H1000 GOTO tmpin
tmploc: regs.ax = &H4408
        regs.bx = i
        interruptx &H21, regs, regs
        IF regs.flags AND 1 GOTO tmpin
        IF regs.ax = 0 GOTO tmpin
        regs.ax = &H3600
        regs.dx = i
        interruptx &H21, regs, regs
        IF regs.flags AND 1 GOTO tmpin
        q = i2l(regs.ax) * i2l(regs.bx) * i2l(regs.cx)
        IF q > ql THEN ql = q: xf = i
tmpin: NEXT i
       stemp = CHR$(xf + 64) + ":\"
      END IF
END SUB

SUB trp
      sok = LEFT$(srepcrc, 1)
      ser = MID$(srepcrc, 2, 1)
      sni = RIGHT$(srepcrc, 1)
      s5 = STRING$(5, 32)
      s6 = STRING$(6, 32)
      s7 = STRING$(7, 32)
      s9 = STRING$(9, 32)
      s14 = STRING$(14, 32)
      PRINT #1,
      PRINT #1, "Cylinder:" + STR$(cyl)
      PRINT #1, "Head:" + STR$(head)
      IF nos = 0 AND dtf = 0 THEN
       PRINT #1, "Non-Formatted"
       EXIT SUB
      END IF
      SELECT CASE dot
       CASE 0: s = "Low"
       CASE 1: s = "Double"
       CASE 2: s = "Medium"
       CASE 3: s = "High"
       CASE 4: s = "Quad"
      END SELECT
      PRINT #1, "Track Recording Density: " + s
      IF nos = 0 AND dtf = 1 THEN
       PRINT #1, "Incorrect Measurement"
       EXIT SUB
      END IF
      IF nos = 0 AND dtf = 2 THEN
       PRINT #1, "More than 64 sectors, cannot analyse"
       EXIT SUB
      END IF
      SELECT CASE dtf
       CASE 0: PRINT #1, "Track Type: Standard"
       CASE 1: PRINT #1, "Track Type: Non-Standard"
      END SELECT
      PRINT #1, "Number of Sectors:" + STR$(nos)
      IF dtf = 0 THEN
       PRINT #1, "Sector Length:" + STR$(alen)
       PRINT #1, "Average GPL:" + STR$(agpl)
      END IF
      PRINT #1, " C ÍÍ H ÍÍ R ÍÍ N ÍÍ S-S ÍÍ I-S ÍÍ CRC ÍÍÍ DT ÍÍ GAP3 State ÍÍ Data Offset"
     FOR k = 1 TO nos
      s = ""
      LSET s5 = STR$(spar(k).c): s = s + s5
      LSET s5 = STR$(spar(k).h): s = s + s5
      LSET s5 = STR$(spar(k).r): s = s + s5
      LSET s5 = STR$(spar(k).n): s = s + s5
      LSET s7 = STR$(spar(k).gpl): s = s + s7
      LSET s7 = STR$(spar(k).post): s = s + s7
      x = spar(k).csn: y = spar(k).crc: v = ""
      FOR ls = 0 TO 6
       IF gpltab(ls) - spar(k).gpl > dop THEN EXIT FOR
       a = (x AND 1) * 2 + (y AND 1)
       x = x \ 2: y = y \ 2
       SELECT CASE a
        CASE 0: v = v + sni
        CASE 2: v = v + ser
        CASE 3: v = v + sok
       END SELECT
      NEXT ls
      FOR l = ls TO 6
       v = v + " "
      NEXT l
      IF spar(k).dt = 2 THEN v = "N/A    "
      s = s + " " + v
      SELECT CASE spar(k).dt
       CASE 0: LSET s6 = " Nor"
       CASE 1: LSET s6 = " Del"
       CASE 2: LSET s6 = " None"
      END SELECT
      s = s + s6
      IF spar(k).agr THEN
       LSET s14 = STR$(spar(k).agr \ 4096)
       SELECT CASE spar(k).agr \ 256 AND 3
        CASE 1: v = "OVR": x = 1
        CASE 2: v = "FWR": x = 0
        CASE 3: v = "NWR": x = 1
       END SELECT
       MID$(s14, 4) = v
       IF x THEN MID$(s14, 8) = LTRIM$(STR$(spar(k).agr AND 255))
      ELSE
       LSET s14 = ""
      END IF
      s = s + s14
      IF spar(k).offset <> -1 THEN
       LSET s9 = " " + HEX$(spar(k).offset) + "H"
      ELSE
       LSET s9 = " None"
      END IF
      s = s + s9
      PRINT #1, s
     NEXT k
END SUB

SUB waint
      swinfdcproc 12, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 THEN erflg = 1
END SUB

SUB warwind (s)
      msav
      moff
      post = (80 - LEN(s)) \ 2
      wincre 9, post - 3, 7, LEN(s) + 6, "Warning", 2
      fprint 11, post, s, 7
      mkbut 13, 35, 10, "Ok", 1, 12, 18
      mon
waw1: f = 0: WHILE f = 0: mkey f, a, b, bs: WEND
      IF f = 1 THEN
       IF a = 13 OR a = 27 GOTO waw2
      ELSE
       IF a = 13 AND b > 34 AND b < 46 GOTO waw2
      END IF
      GOTO waw1
waw2: moff: winoff: mrst
END SUB

SUB wawind (s)
      msav
      moff
      post = (80 - LEN(s) - 6) \ 2
      wincre 10, post, 5, LEN(s) + 6, "", oboxflg
      fprint 12, post + 3, s, 7
      mrst
END SUB

SUB wincre (BYVAL br, BYVAL bc, BYVAL nr, BYVAL nc, stit, BYVAL dbtyp)
SHARED win() AS winpar, now
      IF now THEN
       mkbord1 win(now).br, win(now).bc, win(now).nr, win(now).nc, 7
       tit = " " + RTRIM$(win(now).tit) + " "
       IF tit <> "  " THEN fprint win(now).br, win(now).tpos, tit, 20
      END IF
      IF dbtyp < 0 THEN
       FOR i = now TO 1 STEP -1
        IF win(i).wintyp = 0 THEN dbtyp = 1: EXIT FOR
        IF win(i).wintyp = 1 THEN dbtyp = 0: EXIT FOR
       NEXT i
       IF dbtyp = -1 THEN dbtyp = 0
       IF dbtyp = -2 THEN dbtyp = 1
      END IF
      now = now + 1
      setattr 7, 20, dbtyp * 14
      post = bc + (nc - LEN(stit)) \ 2 - 1
      svscr br, bc, nr + 1, nc + 2: mkwind br, bc, nr, nc, 7
      FOR i = br + 1 TO br + nr - 1
       chattr i, bc + nc, 2, 0
      NEXT i
      chattr br + nr, bc + 2, nc, 0
      mkbord2 br, bc, nr, nc, 7: IF stit <> "" THEN fprint br, post, " " + stit + " ", 19
      win(now).br = br: win(now).bc = bc: win(now).nr = nr: win(now).nc = nc
      win(now).tpos = post: LSET win(now).tit = stit: win(now).wintyp = dbtyp
END SUB

SUB winoff
SHARED win() AS winpar, now
      rstscr
      now = now - 1
      IF now THEN
       setattr 7, 20, win(now).wintyp * 14
       mkbord2 win(now).br, win(now).bc, win(now).nr, win(now).nc, 7
       tit = " " + RTRIM$(win(now).tit) + " "
       IF tit <> "  " THEN fprint win(now).br, win(now).tpos, tit, 19
      END IF
END SUB

SUB woff
SHARED now
      WHILE now: winoff: WEND
END SUB

SUB woff1
SHARED now
      WHILE now > 1: winoff: WEND
END SUB

SUB wrirst
      seq = STRING$(dwrilen(cyl, head), 0)
      GET #6, qwrioffset(cyl, head) + 1, seq
END SUB

SUB wriupd
      CLOSE #6
      NAME sopdir + "DISK.WRI" AS sopdir + "~DISKOLD.WRI"
      OPEN sopdir + "~DISKOLD.WRI" FOR BINARY ACCESS READ AS #1
      OPEN sopdir + "DISK.WRI" FOR BINARY ACCESS READ WRITE AS #6
      v = STRING$(10, 0): GET #1, , v: PUT #6, , v
      FOR i = 0 TO ncyl - 1
       FOR j = 0 TO 1
        s = STRING$(dwrilen(i, j), 0): GET #1, , s
        IF i = cyl AND j = head THEN
         s = seq
         dwrilen(cyl, head) = LEN(seq)
        END IF
        PUT #6, , s
      NEXT j, i
      CLOSE #1: KILL sopdir + "~DISKOLD.WRI"
      q = 10
      FOR i = 0 TO ncyl - 1
       FOR j = 0 TO 1
        qwrioffset(i, j) = q
        q = q + dwrilen(i, j)
      NEXT j, i
END SUB

