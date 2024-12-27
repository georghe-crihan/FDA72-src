'$INCLUDE: 'FDA72-G.BI'
END

SUB btws
      iamlen = 66
      wawind "Building Track Write Sequence..."
      x = obr
      IF x THEN
       IF iamlen THEN
        iamlen = 0: lssf = 0
        x = obr
       END IF
       IF x THEN erwind "Could not build track write sequence" ELSE warwind "IAM is overwritten": GOTO btws1
      ELSE
btws1: wriupd
      END IF
      winoff
END SUB

SUB cerident
      IF ncyl < 40 OR ncyl > 45 AND ncyl < 80 OR ncyl > 90 THEN EXIT SUB
      cyl = 6: head = 0
      fmtrst
      IF nos = 0 OR dtf = 0 THEN EXIT SUB
      SELECT CASE dot
       CASE 0: EXIT SUB
       CASE 1: nsec = 9
       CASE 2: EXIT SUB
       CASE 3: nsec = 15
       CASE 4: nsec = 18
      END SELECT
      dens = dot
      IF nos <> nsec + 1 THEN EXIT SUB
      xf = 0
      FOR i = 1 TO nsec
       IF spar(i).c <> cyl THEN xf = 1: EXIT FOR
       IF spar(i).h <> head THEN xf = 1: EXIT FOR
       IF spar(i).r <> i THEN xf = 1: EXIT FOR
       IF spar(i).n <> 2 THEN xf = 1: EXIT FOR
       IF (spar(i).crc AND 4) = 0 THEN xf = 1: EXIT FOR
       IF spar(i).dt THEN xf = 1: EXIT FOR
      NEXT i
      IF xf THEN EXIT SUB
      IF spar(nos).c <> cyl THEN EXIT SUB
      IF spar(nos).h <> head THEN EXIT SUB
      IF spar(nos).r <> 20 THEN EXIT SUB
      IF spar(nos).n <> 1 THEN EXIT SUB
      IF (spar(nos).crc AND 2) = 0 THEN EXIT SUB
      IF spar(nos).dt THEN EXIT SUB
      xf = 0
      FOR i = 0 TO 6
       cyl = i: head = 0
       fmtrst
       IF nos = 0 OR dtf = 0 THEN xf = 1: EXIT FOR
       SELECT CASE cyl
        CASE 0: x = 1
        CASE 1: x = 4
        CASE 2: x = 4
        CASE 3: x = 0
        CASE 4: x = 0
        CASE 5: x = 1
        CASE 6: x = 1
       END SELECT
       IF dot <> dens THEN xf = 1: EXIT FOR
       IF nos <> nsec + x THEN xf = 1: EXIT FOR
       IF cyl = 0 THEN keyl = spar(nos).r
       IF cyl = 1 THEN keyh = spar(11).r
       IF cyl = 6 THEN qoff = spar(nos).offset
      NEXT i
      IF xf THEN EXIT SUB
      qkey = keyh * 256& + keyl
      SELECT CASE dens
       CASE 1
        gpl0 = 75
        gpl1a = 66: gpl1b = 75: gpl1c = 83: gpl1d = 90
        gpl2a = 5: gpl2b = 80: gpl2c = 43
        gpl3 = 110
        gpl4 = 31
        gpl5 = 110
        gpl6 = 17
       CASE 3
        gpl0 = 92
        gpl1a = 65: gpl1b = 80: gpl1c = 90: gpl1d = 100
        gpl2a = 10: gpl2b = 90: gpl2c = 70
        gpl3 = 114
        gpl4 = 35
        gpl5 = 114
        gpl6 = 27
       CASE 4
        gpl0 = 99
        gpl1a = 79: gpl1b = 89: gpl1c = 98: gpl1d = 106
        gpl2a = 10: gpl2b = 90: gpl2c = 70
        gpl3 = 115
        gpl4 = 36
        gpl5 = 115
        gpl6 = 29
      END SELECT
      FOR i = 0 TO 6
       cyl = i: head = 0
       fmtrst
       seq = CHR$(1) + CHR$(dens): noop = 1
       IF cyl <> 2 AND cyl <> 6 THEN
        seq = seq + CHR$(3) + CHR$(1) + CHR$(1) + CHR$(1) + "ö" + CHR$(0)
        seq = seq + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0)
        noop = noop + 1
       END IF
       SELECT CASE cyl
        CASE 1: nf = 4
        CASE 2: nf = 3
        CASE ELSE: nf = 1
       END SELECT
       FOR j = 1 TO nf
        SELECT CASE cyl
         CASE 0: af = 1: ns = nsec + 1: ln = 2: gpl = gpl0: bw = 1: ew = nsec: fw = 0
         CASE 1
          SELECT CASE j
           CASE 1: af = 1: ns = nsec + 1: ln = 2: gpl = gpl1d: bw = 1: ew = nsec: fw = 0
           CASE 2: af = 1: ns = 9: ln = 2: gpl = gpl1c: bw = 1: ew = 8: fw = 0
           CASE 3: af = 1: ns = 8: ln = 2: gpl = gpl1b: bw = 1: ew = 7: fw = 0
           CASE 4: af = 1: ns = 7: ln = 2: gpl = gpl1a: bw = 1: ew = 6: fw = 0
          END SELECT
         CASE 2
          SELECT CASE j
           CASE 1: af = 0: ns = nsec + 1: ln = 2: gpl = gpl2c: bw = 1: ew = nsec: fw = 0
           CASE 2: af = 1: ns = 3: ln = 0: gpl = gpl2b: bw = 1: ew = 0
           CASE 3: af = 1: ns = 2: ln = 0: gpl = gpl2a: bw = 1: ew = 0
          END SELECT
         CASE 3: af = 1: ns = nsec + 1: ln = 2: gpl = gpl3: bw = 2: ew = nsec: fw = 0
         CASE 4: af = 1: ns = nsec * 3 + 3: ln = 0: gpl = gpl4: bw = 2: ew = nsec - 1: fw = 1
         CASE 5: af = 1: ns = nsec + 1: ln = 2: gpl = gpl5: bw = 1: ew = nsec: fw = 0
         CASE 6: af = 0: ns = nsec * 3 + 2: ln = 0: gpl = gpl6: bw = 1: ew = nsec: fw = 1
        END SELECT
        IF af THEN seq = seq + CHR$(2): noop = noop + 1
        seq = seq + CHR$(3) + CHR$(ns) + CHR$(ln) + CHR$(gpl) + "ö"
        IF af THEN
         seq = seq + CHR$(1) + CHR$(1) + MKI$((gpltab(ln) + gpl) * (ns - 1) + 179)
        ELSE
         seq = seq + CHR$(0)
        END IF
        IF cyl = 2 OR cyl = 3 OR cyl = 4 THEN
         IF j = 1 OR j = 2 THEN seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
         IF cyl = 4 THEN
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
         END IF
         IF j = 2 THEN
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(spar(3).r) + CHR$(3)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(spar(4).r) + CHR$(1)
         END IF
         IF j = 3 THEN
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(spar(1).r) + CHR$(4)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(spar(2).r) + CHR$(0)
         END IF
        END IF
        FOR k = bw TO ew
         seq = seq + CHR$(cyl) + CHR$(head) + CHR$(k) + CHR$(2)
         IF fw THEN
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
         END IF
        NEXT k
        SELECT CASE cyl
         CASE 0
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(spar(nsec + 1).r) + CHR$(4)
         CASE 1
          SELECT CASE j
           CASE 1: n = nsec + 4: ln = 4
           CASE 2: n = 11: ln = 0
           CASE 3: n = 9: ln = 1
           CASE 4: n = 7: ln = 3
          END SELECT
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(spar(n).r) + CHR$(ln)
         CASE 3
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(1) + CHR$(2)
         CASE 4
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(nsec) + CHR$(2)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(255) + CHR$(1)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(1) + CHR$(2)
         CASE 5
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(spar(1).r) + CHR$(2)
         CASE 6
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(20) + CHR$(1)
          seq = seq + CHR$(cyl) + CHR$(head) + CHR$(21) + CHR$(4)
        END SELECT
        noop = noop + 1
       NEXT j
       IF cyl = 4 THEN
        seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(255) + CHR$(1) + CHR$(0) + MKL$(-1) + MKI$(256) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
        noop = noop + 1
       END IF
       FOR j = 1 TO nsec
        FOR k = 1 TO nos
         IF spar(k).c = cyl AND spar(k).h = head AND spar(k).r = j AND spar(k).n = 2 THEN q = spar(k).offset: EXIT FOR
        NEXT k
        seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(j) + CHR$(2) + CHR$(0) + MKL$(q) + MKI$(512) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
        noop = noop + 1
       NEXT j
       SELECT CASE cyl
        CASE 0: bw = 3: ew = 3
        CASE 1: bw = 4: ew = 4
        CASE 2: bw = 1: ew = 2
        CASE ELSE: bw = 1: ew = 0
       END SELECT
       FOR j = bw TO ew
        seq = seq + CHR$(2): noop = noop + 1
        SELECT CASE j
         CASE 1: a = 1: b = 1: c = 146: d = 1: e = gpltab(0) + gpl2a + 146
         CASE 2: a = 3: b = 0: c = gpltab(0) + gpl2b + 146: d = 0: e = (gpltab(0) + gpl2b) * 2 + 146
         CASE 3: a = nsec + 1: b = 0: c = (gpltab(2) + gpl0) * nsec + 146: d = 1: e = 146 + abtkid
         CASE 4: a = nsec + 4: b = 0: c = (gpltab(2) + gpl1d) * nsec + 146: d = 1: e = 146 + abtkid
        END SELECT
        seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(spar(a).r) + CHR$(spar(a).n)
        seq = seq + CHR$(0) + MKL$(spar(a).offset) + MKI$(lensec(spar(a).n))
        seq = seq + CHR$(255) + CHR$(255) + CHR$(27)
        seq = seq + CHR$(1) + CHR$(b) + MKI$(c) + CHR$(1) + CHR$(d) + MKI$(e)
        noop = noop + 1
       NEXT j
       IF cyl = 0 THEN
        s = " !CERBERUS" + STR$(dens) + " " + HEX$(qkey) + "H " + HEX$(qoff) + "H"
        seq = seq + CHR$(0) + CHR$(LEN(s)) + s
        noop = noop + 1
       END IF
       IF cyl = 5 THEN n = 2 ELSE n = 1
       seq = MKL$(spar(n).offset) + CHR$(noop) + seq
       wriupd
      NEXT i
      SELECT CASE dens
       CASE 1: gpl = 10
       CASE 3: gpl = 22
       CASE 4: gpl = 25
      END SELECT
      FOR i = 0 TO 3
       cyl = i \ 2 + 7: head = i AND 1
       fmtrst
       seq = CHR$(1) + CHR$(dens): noop = 1
       seq = seq + CHR$(3) + CHR$(nsec * 3 + 3) + CHR$(0) + CHR$(gpl) + "ö" + CHR$(0)
       FOR j = 1 TO nsec
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
       NEXT j
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(20) + CHR$(2)
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(21) + CHR$(4)
       noop = noop + 1
       seq = seq + CHR$(2): noop = noop + 1
       seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(20) + CHR$(2) + CHR$(0) + MKL$(spar(nsec + 1).offset) + MKI$(512) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(1) + CHR$(0) + MKI$((gpltab(0) + gpl) * nsec * 3 + 146) + CHR$(1) + CHR$(0) + MKI$ _
((gpltab(0) + gpl) * (nsec * 3 + 2) + abtkid + 146)
       noop = noop + 1
       seq = seq + CHR$(2): noop = noop + 1
       seq = seq + CHR$(3) + CHR$(nsec * 3 + 1) + CHR$(0) + CHR$(gpl) + "ö" + CHR$(1) + CHR$(1) + MKI$((gpltab(0) + gpl) * nsec * 3 + 179)
       FOR j = 1 TO nsec
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(j) + CHR$(2)
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
       NEXT j
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(20) + CHR$(1)
       noop = noop + 1
       FOR j = 1 TO nsec
        seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(j) + CHR$(2) + CHR$(0) + MKL$(spar(j).offset) + MKI$(512) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
        noop = noop + 1
       NEXT j
       seq = seq + CHR$(2): noop = noop + 1
       seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(21) + CHR$(4) + CHR$(0) + MKL$(spar(nsec + 2).offset) + MKI$(2048) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(1) + CHR$(0) + MKI$((gpltab(0) + gpl) * (nsec * 3 + 2) + 146) + CHR$(1) + CHR$(1) _
 + MKI$(abtkid + 146)
       noop = noop + 1
       seq = MKL$(spar(1).offset) + CHR$(noop) + seq
       wriupd
      NEXT i
END SUB

SUB cerregen
      POKE &H40, 0
      REDIM qpars(1 TO 3)
      x = fwslabel("CERBERUS", 3, qpars())
      IF x THEN EXIT SUB
      IF qpars(1) <> 1 AND qpars(1) <> 3 AND qpars(1) <> 4 THEN EXIT SUB
      dens = qpars(1)
      qkey = qpars(2)
      qoff = qpars(3)
      ERASE qpars
      POKE &H40, 0
      SELECT CASE dens
       CASE 1: nsec = 9: gpl = 17
       CASE 3: nsec = 15: gpl = 27
       CASE 4: nsec = 18: gpl = 29
      END SELECT
      regs.ax = &H4200
      regs.bx = filnum
      regs.cx = e243(qoff)
      regs.dx = e241(qoff)
      interruptx &H21, regs, regs
      regs.ax = &H3F00
      regs.bx = filnum
      regs.cx = 256
      regs.dx = dmaboff
      regs.ds = dmabseg
      interruptx &H21, regs, regs
      sf = STRING$(256, 0)
      dmabrd 0, sf, 256
      FOR i = 1 TO 3
       SELECT CASE i
        CASE 1: ln = 256: kmul = &H82BD: kadd = &H2BE7: qsum = &H1421
        CASE 2: ln = 50: kmul = &H1693: kadd = &HA625: qsum = i2l(CVI(MID$(sf, 51, 2)))
        CASE 3: ln = 50: kmul = &HCD49: kadd = &H586D: qsum = qkey
       END SELECT
       posit = 1
       FOR j = 1 TO ln
        q1 = qsum AND 255: q2 = qsum \ 256: q3 = i2l(kmul) AND 255: q4 = i2l(kmul) \ 256
        ql = q1 * q3 + (q2 * q3 + q1 * q4) * 256
        qh = q2 * q4
        qh = qh + e143(ql)
        ql = ql AND 65535
        ql = (ql + i2l(kadd)) AND 65535
        qsum = ql
        a = e141(ql + qh)
        x = ASC(MID$(sf, posit, 1))
        x = x XOR a
        MID$(sf, posit) = CHR$(x)
        posit = posit + 1
       NEXT j
      NEXT i
      FOR i = 0 TO 19
       POKE &H40, 0
       cyl = i \ 2 + 9: head = i AND 1: hdd = drive + head * 4
       hdpos cyl: IF erflg GOTO er1
       x = ixfr(dens, 2)
       POKE &H40, 0
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 20
       cmd(6) = 1
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H42, 0, 16
       dbtc 9: IF erflg GOTO er1
       waint
       IF erflg GOTO er1
       n = csfc: IF erflg GOTO er1
       IF cstat(1) AND 5 THEN EXIT FOR
       POKE &H40, 0
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 1
       cmd(6) = 2
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, nsec * 512
       dbtc 9: IF erflg GOTO er1
       waint
       IF erflg GOTO er1
       n = csfc: IF erflg GOTO er1
       POKE &H40, 0
       s = "": nf = 0
       FOR j = 1 TO nsec
        s = s + CHR$(cyl) + CHR$(head) + CHR$(j) + CHR$(2)
        s = s + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
        s = s + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
        nf = nf + 3
       NEXT j
       s = s + CHR$(cyl) + CHR$(head) + CHR$(20) + CHR$(1)
       s = s + CHR$(cyl) + CHR$(head) + CHR$(21) + CHR$(4)
       nf = nf + 2
       dmabwr 16384, s, nf * 4
       cmd(1) = &H4D
       cmd(2) = hdd
       cmd(3) = 0
       cmd(4) = nf
       cmd(5) = gpl
       cmd(6) = ASC("ö")
       dmach2 &H4A, 16384, nf * 4
       dbtc 6: IF erflg GOTO er1
       waint
       IF erflg GOTO er1
       n = csfc: IF erflg GOTO er1
       POKE &H40, 0
       cmd(1) = &H45
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 1
       cmd(6) = 2
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H4A, 0, nsec * 512
       dbtc 9: IF erflg GOTO er1
       waint
       IF erflg GOTO er1
       n = csfc: IF erflg GOTO er1
       POKE &H40, 0
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 21
       cmd(6) = 4
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, 2048
       dbtc 9: IF erflg GOTO er1
       waint
       IF erflg GOTO er1
       n = csfc: IF erflg GOTO er1
       s = STRING$(1918, 0): dmabrd 130, s, 1918
       x = findif(s, 1918)
       MID$(sf, i * 2 + 9) = MKI$(x)
      NEXT i
      POKE &H40, 0
      q = 0: posit = 1
      FOR i = 1 TO 48
       x = ASC(MID$(sf, posit, 1))
       posit = posit + 1
       q = q * 113 + x AND 65535
      NEXT i
      q = q + 197
      MID$(sf, 49) = MKI$(e241(q))
      FOR i = 1 TO 3
       SELECT CASE i
        CASE 1: ln = 50: kmul = &HCD49: kadd = &H586D: qsum = qkey
        CASE 2: ln = 50: kmul = &H1693: kadd = &HA625: qsum = i2l(CVI(MID$(sf, 51, 2)))
        CASE 3: ln = 256: kmul = &H82BD: kadd = &H2BE7: qsum = &H1421
       END SELECT
       posit = 1
       FOR j = 1 TO ln
        q1 = qsum AND 255: q2 = qsum \ 256: q3 = i2l(kmul) AND 255: q4 = i2l(kmul) \ 256
        ql = q1 * q3 + (q2 * q3 + q1 * q4) * 256
        qh = q2 * q4
        qh = qh + e143(ql)
        ql = ql AND 65535
        ql = (ql + i2l(kadd)) AND 65535
        qsum = ql
        a = e141(ql + qh)
        x = ASC(MID$(sf, posit, 1))
        x = x XOR a
        MID$(sf, posit) = CHR$(x)
        posit = posit + 1
       NEXT j
      NEXT i
      POKE &H40, 0
      cyl = 6: head = 0: hdd = drive
      hdpos cyl: IF erflg GOTO er1
      x = ixfr(dens, 2)
      dmabwr 0, sf, 256
      cmd(1) = &H45
      cmd(2) = hdd
      cmd(3) = cyl
      cmd(4) = head
      cmd(5) = 20
      cmd(6) = 1
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H4A, 0, 256
      dbtc 9: IF erflg GOTO er1
      waint
      IF erflg GOTO er1
      n = csfc: IF erflg GOTO er1
      
er1:
END SUB

SUB clkident
      IF ncyl < 40 OR ncyl > 45 AND ncyl < 80 OR ncyl > 90 THEN EXIT SUB
      cyl = 0: head = 0
      fmtrst
      IF nos = 0 OR dtf = 0 THEN EXIT SUB
      SELECT CASE dot
       CASE 0: EXIT SUB
       CASE 1: nsec = 9
       CASE 2: EXIT SUB
       CASE 3: nsec = 15
       CASE 4: nsec = 18
      END SELECT
      dens = dot
      IF nos < nsec + 2 THEN EXIT SUB
      xf = 0
      FOR i = 1 TO nsec
       IF spar(i).c <> cyl THEN xf = 1: EXIT FOR
       IF spar(i).h <> head THEN xf = 1: EXIT FOR
       IF spar(i).r <> i THEN xf = 1: EXIT FOR
       IF spar(i).n <> 2 THEN xf = 1: EXIT FOR
       IF (spar(i).crc AND 4) = 0 THEN xf = 1: EXIT FOR
       IF spar(i).dt THEN xf = 1: EXIT FOR
      NEXT i
      IF xf THEN EXIT SUB
      FOR i = i TO nos
       IF spar(i).n THEN EXIT FOR
      NEXT i
      IF i <> nos - 1 THEN EXIT SUB
      IF spar(i).c <> cyl THEN EXIT SUB
      IF spar(i).h <> head THEN EXIT SUB
      IF spar(i).n <> 1 THEN EXIT SUB
      IF (spar(i).crc AND 2) = 0 THEN EXIT SUB
      IF spar(i).dt THEN EXIT SUB
      idrinf = spar(i).r
      qoff = spar(i).offset
      OPEN sopdir + "DISK.BIN" FOR BINARY ACCESS READ AS #1
      SEEK #1, qoff + 1
      s = STRING$(256, 0)
      GET #1, , s
      CLOSE #1
      i = i + 1
      IF spar(i).c <> cyl THEN EXIT SUB
      IF spar(i).h <> head THEN EXIT SUB
      IF spar(i).n <> 2 THEN EXIT SUB
      IF spar(i).crc AND 4 THEN EXIT SUB
      IF spar(i).dt THEN EXIT SUB
      idrend = spar(i).r
      sf = "": posit = 1: q = 0
      FOR i = 1 TO 254
       x = ASC(MID$(s, posit, 1))
       posit = posit + 1
       q = (q + idrinf * 256& + x) AND 65535
       x = e121(256 - x)
       sf = sf + CHR$(x)
      NEXT i
      IF CVI(MID$(s, posit, 2)) <> e241(q) THEN EXIT SUB
      st = MID$(sf, 10, 1)
      IF st <> " " AND st <> "X" AND st <> "D" AND st <> "S" THEN EXIT SUB
      IF st <> "X" AND dens <> 1 THEN perf = 1 ELSE perf = 0
      IF st <> "X" AND dens = 1 THEN oldgpl = 1 ELSE oldgpl = 0
      mrktrk = ASC(MID$(sf, 179, 1))
      cntr = 0
      cyl = 1: head = 0
      fmtrst
      IF dot <> dens OR nos < nsec THEN EXIT SUB
      IF dtf AND nos = nsec GOTO nins
      IF nos < nsec + 1 OR dtf = 0 THEN EXIT SUB
      xf = 0
      FOR i = 1 TO nsec
       IF spar(i).c <> cyl THEN xf = 1: EXIT FOR
       IF spar(i).h <> head THEN xf = 1: EXIT FOR
       IF spar(i).r <> i THEN xf = 1: EXIT FOR
       IF spar(i).n <> 2 THEN xf = 1: EXIT FOR
       IF (spar(i).crc AND 4) = 0 THEN xf = 1: EXIT FOR
       IF spar(i).dt THEN xf = 1: EXIT FOR
      NEXT i
      IF xf THEN EXIT SUB
      FOR i = i TO nos
       IF spar(i).n THEN EXIT FOR
      NEXT i
      IF i > nos THEN EXIT SUB
      IF i < nos - 1 THEN EXIT SUB
      IF i = nos - 1 THEN cntr = 9
      IF spar(i).c <> cyl THEN EXIT SUB
      IF spar(i).h <> head THEN EXIT SUB
      IF spar(i).n <> 1 THEN EXIT SUB
      IF cntr <> 0 AND spar(i).crc AND 2 THEN EXIT SUB
      IF cntr = 0 AND (spar(i).crc AND 2) = 0 THEN EXIT SUB
      IF spar(i).dt THEN EXIT SUB
      idrclr = spar(i).r
      IF cntr THEN
       i = i + 1
       IF spar(i).c <> cyl THEN EXIT SUB
       IF spar(i).h <> head THEN EXIT SUB
       IF spar(i).n <> 2 THEN EXIT SUB
       IF spar(i).crc AND 4 THEN EXIT SUB
       IF spar(i).dt THEN EXIT SUB
       idrcnt = spar(i).r
      END IF
nins: FOR i = 0 TO cntr
       cyl = i: head = 0
       fmtrst
       seq = CHR$(1) + CHR$(dens): noop = 1
       IF cyl THEN xf = 1 ELSE xf = 0
       SELECT CASE dens * 2 + xf
        CASE 2: gpl = 13
        CASE 3: gpl = 19
        CASE 6: gpl = 24
        CASE 7: gpl = 28
        CASE 8: gpl = 26
        CASE 9: gpl = 30
       END SELECT
       IF xf = 0 AND oldgpl THEN gpl = 15
       IF perf THEN gpl = 15
       a = nsec * 3 + 3 - xf: x = 0
       IF perf AND dens = 3 THEN x = xf + 2
       IF perf AND dens = 4 THEN x = xf + 3
       a = a + x
       seq = seq + CHR$(3) + CHR$(a) + CHR$(0) + CHR$(gpl) + "ö" + CHR$(0)
       FOR j = 1 TO nsec
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(j) + CHR$(2)
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
       NEXT j
       FOR j = 1 TO x
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
       NEXT j
       IF xf THEN a = idrclr ELSE a = idrinf
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(a) + CHR$(1)
       IF xf = 0 THEN seq = seq + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
       IF xf THEN a = idrcnt ELSE a = idrend
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(a) + CHR$(2)
       noop = noop + 1
       FOR j = 1 TO nsec
        seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(j) + CHR$(2) + CHR$(0) + MKL$(spar(j).offset) + MKI$(512) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
        noop = noop + 1
       NEXT j
       IF cyl = 0 THEN
        s = " !COPYLOCK" + STR$(dens) + STR$(idrinf) + STR$(idrend) + STR$(idrclr) + STR$(idrcnt) + " " + HEX$(qoff) + "H"
        seq = seq + CHR$(0) + CHR$(LEN(s)) + s
        noop = noop + 1
       END IF
       seq = MKL$(spar(1).offset) + CHR$(noop) + seq
       wriupd
      NEXT i
      IF mrktrk THEN
       cyl = mrktrk: head = 0
       SELECT CASE dens
        CASE 1: gpl = 80
        CASE 3: gpl = 84
        CASE 4: gpl = 108
       END SELECT
       s = ""
       FOR i = 1 TO nsec
        s = s + CHR$(cyl) + CHR$(head) + CHR$(i) + CHR$(2)
       NEXT i
       IF perf THEN
        gpl = 80
        MID$(s, 4) = CHR$(6)
        MID$(s, 56) = CHR$(6)
        getpos = 36: putpos = 4
        FOR i = 10 TO nsec
         s = LEFT$(s, putpos) + MID$(s, getpos + 1, 4) + MID$(s, putpos + 1, getpos - putpos) + MID$(s, getpos + 5)
         getpos = getpos + 4: putpos = putpos + 8
        NEXT i
       END IF
       seq = CHR$(1) + CHR$(dens): noop = 1
       seq = seq + CHR$(3) + CHR$(nsec) + CHR$(2) + CHR$(gpl) + "ö" + CHR$(0) + s
       noop = noop + 1
       seq = MKL$(-1) + CHR$(noop) + seq
       wriupd
      END IF
END SUB

SUB clkregen
      POKE &H40, 0
      REDIM qpars(1 TO 6)
      x = fwslabel("COPYLOCK", 6, qpars())
      IF x THEN EXIT SUB
      IF qpars(1) <> 1 AND qpars(1) <> 3 AND qpars(1) <> 4 THEN EXIT SUB
      dens = qpars(1)
      IF qpars(2) < 0 OR qpars(2) > 255 THEN EXIT SUB
      idrinf = qpars(2)
      IF qpars(3) < 0 OR qpars(3) > 255 THEN EXIT SUB
      idrend = qpars(3)
      IF qpars(4) < 0 OR qpars(4) > 255 THEN EXIT SUB
      idrclr = qpars(4)
      IF qpars(5) < 0 OR qpars(5) > 255 THEN EXIT SUB
      idrcnt = qpars(5)
      qoff = qpars(6)
      ERASE qpars
      POKE &H40, 0
      SELECT CASE dens
       CASE 1: nsec = 9
       CASE 3: nsec = 15
       CASE 4: nsec = 18
      END SELECT
      regs.ax = &H4200
      regs.bx = filnum
      regs.cx = e243(qoff)
      regs.dx = e241(qoff)
      interruptx &H21, regs, regs
      regs.ax = &H3F00
      regs.bx = filnum
      regs.cx = 256
      regs.dx = dmaboff
      regs.ds = dmabseg
      interruptx &H21, regs, regs
      s = STRING$(256, 0)
      dmabrd 0, s, 256
      sf = "": posit = 1
      FOR i = 1 TO 254
       x = ASC(MID$(s, posit, 1))
       posit = posit + 1
       x = e121(256 - x)
       sf = sf + CHR$(x)
      NEXT i
      POKE &H40, 0
      st = MID$(sf, 10, 1)
      IF st = "D" OR st = "S" THEN st = " ": MID$(sf, 10) = st
      IF st <> "X" AND dens <> 1 THEN perf = 1 ELSE perf = 0
      IF st <> "X" AND dens = 1 THEN oldgpl = 1 ELSE oldgpl = 0
      mrktrk = ASC(MID$(sf, 179, 1)): pretrk = ASC(MID$(sf, 177, 1))
      x = CVI(MID$(sf, 225, 2))
      IF x THEN cntr = 9: MID$(sf, 225) = MKI$(9) ELSE cntr = 0
      FOR i = 0 TO cntr
       POKE &H40, 0
       cyl = i: head = 0: hdd = drive
       hdpos cyl: IF erflg GOTO er0
       x = ixfr(dens, 2)
       IF cyl THEN x = idrcnt ELSE x = idrend
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = x
       cmd(6) = 2
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, 512
       dbtc 9: IF erflg GOTO er0
       waint
       IF erflg GOTO er0
       n = csfc: IF erflg GOTO er0
       IF perf = 0 AND oldgpl = 0 THEN
        a = 512: b = 2
       ELSE
        IF cyl THEN
         a = 528: b = 2
        ELSE
         a = 288: b = 3
        END IF
       END IF
       x = fdaamf(dmabseg, e241(i2l(dmaboff) + 131), a, b)
       IF cyl THEN p = 224 + cyl * 2 ELSE p = 160
       MID$(sf, p + 1) = MKI$(x)
       IF cyl = 0 THEN
        x = fdaamf(dmabseg, e241(i2l(dmaboff) + 131), a - 16, 2)
        MID$(sf, 163) = MKI$(x)
       END IF
      NEXT i
      IF mrktrk THEN
       POKE &H40, 0
       cyl = mrktrk: head = 0: hdd = drive
       hdpos cyl: IF erflg GOTO er0
       x = ixfr(dens, 2)
       cmd(1) = &H42
       cmd(2) = hdd
       cmd(6) = 7
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, 16384
       dbtc 9: IF erflg GOTO er0
       waint
       IF erflg GOTO er0
       n = csfc: IF erflg GOTO er0
       SELECT CASE dens
        CASE 1: a = &H173E
        CASE 3: a = &H2785
        CASE 4: a = &H2FA8
       END SELECT
       IF perf AND dens = 3 THEN a = &H26E6
       IF perf AND dens = 4 THEN a = &H2EDA
       IF perf = 0 AND oldgpl = 0 THEN b = 512 ELSE b = 544
       x = fdaamf(dmabseg, e241(i2l(dmaboff) + a), b, 3)
       MID$(sf, 181) = MKI$(x)
       IF pretrk THEN
        POKE &H40, 0
        IF perf THEN ntsec = 9 ELSE ntsec = nsec
        sm = STRING$(180, 0)
        erflg = clktim(hdd, ntsec, SSEG(sm), SADD(sm))
        IF erflg GOTO er0
        cyl = pretrk
        IF mrktrk <> pretrk THEN head = 0 ELSE head = 1
        hdd = drive + head * 4
        hdpos cyl: IF erflg GOTO er0
        x = ixfr(dens, 2)
        POKE &H40, 0
        sp = STRING$(180, 0)
        erflg = clktim(hdd, ntsec, SSEG(sp), SADD(sp))
        IF erflg GOTO er0
        FOR i = 0 TO ntsec - 1
         qm = i2l(CVI(MID$(sm, i * 2 + 1, 2)))
         qp = i2l(CVI(MID$(sp, i * 2 + 1, 2)))
         IF qm <> 0 THEN q = (qp - qm) * 65536 \ qm
         x = e241(q)
         MID$(sf, i * 2 + 183) = MKI$(x)
        NEXT i
       END IF
      END IF
      POKE &H40, 0
      s = "": posit = 1: q = 0
      FOR i = 1 TO 254
       x = ASC(MID$(sf, posit, 1))
       posit = posit + 1
       x = e121(256 - x)
       s = s + CHR$(x)
       q = (q + idrinf * 256& + x) AND 65535
      NEXT i
      s = s + MKI$(e241(q))
      cyl = 0: head = 0: hdd = drive
      hdpos cyl: IF erflg GOTO er0
      x = ixfr(dens, 2)
      dmabwr 0, s, 256
      cmd(1) = &H45
      cmd(2) = hdd
      cmd(3) = cyl
      cmd(4) = head
      cmd(5) = idrinf
      cmd(6) = 1
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H4A, 0, 256
      dbtc 9: IF erflg GOTO er0
      waint
      IF erflg GOTO er0
      n = csfc: IF erflg GOTO er0
      
er0:
END SUB

FUNCTION ctrk (BYVAL mingpl, BYVAL noe, sf)
      IF ctwlock = 0 THEN wawind "Compressing Track..."
      IF noe = 0 THEN
       IF spar(1).dt <> 2 AND spar(1).n < 7 THEN
        IF gpltab(spar(1).n) < spar(1).gpl - dop THEN sf = CHR$(2) + CHR$(1) + MKI$(0): noe = 1
       END IF
       FOR i = nos TO 1 STEP -1
        IF spar(i).dt <> 2 THEN
         IF spar(i).n < 8 THEN
          IF gpltab(spar(i).n) > spar(i).gpl + dop THEN sf = sf + CHR$(0) + CHR$(i) + MKI$(0): noe = noe + 1
         END IF
        END IF
       NEXT i
      END IF
      posit = 1
      FOR i = 1 TO noe
       IF ASC(MID$(sf, posit, 1)) = 0 THEN
        IF CVI(MID$(sf, posit + 2, 2)) = 0 THEN
         x = ASC(MID$(sf, posit + 1, 1))
         n = spar(x).n
         IF n > 7 THEN n = 7
         MID$(sf, posit + 2) = MKI$(lensec(n))
        END IF
       END IF
       posit = posit + 4
      NEXT i
      REDIM qid(1 TO nos), gpl(1 TO nos), crc(1 TO nos), dt(1 TO nos), qoff(1 TO nos), dlen(1 TO nos), qaux(1 TO nos)
      REDIM assb(1 TO nos), assn(1 TO nos), minl(1 TO nos), maxl(1 TO nos)
      oldnos = nos
      FOR i = 1 TO oldnos
       qid(i) = CVL(CHR$(spar(i).c) + CHR$(spar(i).h) + CHR$(spar(i).r) + CHR$(spar(i).n))
       gpl(i) = spar(i).gpl
       crc(i) = CVI(CHR$(spar(i).csn) + CHR$(spar(i).crc))
       dt(i) = spar(i).dt
       qoff(i) = spar(i).offset
       dlen(i) = spar(i).datlen
       qaux(i) = CVL(MKI$(spar(i).dif) + MKI$(spar(i).sfb))
      NEXT i
      nos = 0
      FOR i = 1 TO oldnos
       posit = 1: xf = 1
       FOR j = 1 TO noe
        x = ASC(MID$(sf, posit, 1))
        y = ASC(MID$(sf, posit + 1, 1))
        posit = posit + 4
        IF x = 1 AND y = i THEN xf = 0: EXIT FOR
       NEXT j
       IF xf THEN
        nos = nos + 1
        spar(nos).c = e141(qid(i)): spar(nos).h = e142(qid(i)): spar(nos).r = e143(qid(i)): spar(nos).n = e144(qid(i))
        spar(nos).gpl = gpl(i)
        spar(nos).csn = e121(crc(i)): spar(nos).crc = e122(crc(i))
        spar(nos).dt = dt(i)
        spar(nos).offset = qoff(i)
        spar(nos).datlen = dlen(i)
        spar(nos).dif = e241(qaux(i)): spar(nos).sfb = e243(qaux(i))
        assb(nos) = i: assn(i) = nos
       END IF
      NEXT i
      FOR i = 1 TO nos
       FOR j = i TO nos
        IF spar(i).c <> spar(j).c THEN EXIT FOR
        IF spar(i).h <> spar(j).h THEN EXIT FOR
        IF spar(i).r - i <> spar(j).r - j THEN EXIT FOR
        IF spar(i).n <> spar(j).n THEN EXIT FOR
        IF spar(j).n <> 2 THEN EXIT FOR
        IF gpltab(spar(j).n) > spar(j).gpl - dop THEN EXIT FOR
        IF (spar(j).crc AND 4) = 0 THEN EXIT FOR
        IF spar(j).dt THEN EXIT FOR
       NEXT j
       n = j - i
       IF n > 1 THEN
        REDIM newmap(1 TO n)
        x = 1
        FOR j = 1 TO n STEP 2
         newmap(j) = x
         x = x + 1
         IF x > n THEN EXIT FOR
         IF j = n OR j = n - 1 THEN j = 0
        NEXT j
        FOR j = 1 TO n
         x = i + j - 1
         y = i + newmap(j) - 1
         FOR k = 1 TO oldnos
          IF assn(k) = y THEN EXIT FOR
         NEXT k
         spar(x).c = e141(qid(k)): spar(x).h = e142(qid(k)): spar(x).r = e143(qid(k)): spar(x).n = e144(qid(k))
         spar(x).gpl = gpl(k)
         spar(x).csn = e121(crc(k)): spar(x).crc = e122(crc(k))
         spar(x).dt = dt(k)
         spar(x).offset = qoff(k)
         spar(x).datlen = dlen(k)
         spar(x).dif = e241(qaux(k)): spar(x).sfb = e243(qaux(k))
         assb(x) = k
        NEXT j
        ERASE newmap
        i = i + n - 1
       END IF
      NEXT i
      ERASE qid, gpl, crc, dt, qoff, dlen, qaux
      FOR i = 1 TO nos
       x = assb(i)
       assn(x) = i
      NEXT i
      FOR i = 1 TO nos
       IF spar(i).dt = 2 THEN
        x = 60: y = 60
       ELSE
        xf = 0: posit = 1
        FOR j = 1 TO noe
         IF assn(ASC(MID$(sf, posit + 1, 1))) = i THEN xf = 1: EXIT FOR
         posit = posit + 4
        NEXT j
        IF xf THEN
         a = ASC(MID$(sf, posit, 1))
         b = CVI(MID$(sf, posit + 2, 2))
        ELSE
         a = 1
        END IF
        SELECT CASE a
         CASE 0
          x = spar(i).gpl
          y = b + 62 + mingpl
          IF x > y THEN y = x
         CASE 1
          x = 60
          FOR j = 0 TO 7
           IF spar(i).crc AND power(j) THEN x = gpltab(j)
          NEXT j
          IF spar(i).n < 7 THEN
           IF gpltab(spar(i).n) < spar(i).gpl + dop THEN
            y = gpltab(spar(i).n)
            IF y > x THEN x = y
           END IF
          END IF
          x = x + mingpl
          y = x
         CASE 2
          x = spar(i).gpl
          y = x
        END SELECT
       END IF
       minl(i) = x
       maxl(i) = y
      NEXT i
      drt = drttbl(dot)
      lgpl = drt
      REDIM gap(1 TO nos)
      FOR i = 1 TO nos
       x = minl(i)
       gap(i) = x
       lgpl = lgpl - x
      NEXT i
      IF lgpl < 0 THEN
       IF ctwlock = 0 THEN winoff
       ctrk = 1: EXIT FUNCTION
      END IF
      posit = 1
      FOR i = 1 TO noe
       IF ASC(MID$(sf, posit, 1)) = 0 THEN
        n = assn(ASC(MID$(sf, posit + 1, 1)))
        IF spar(n).dt <> 2 THEN
         DO UNTIL gap(n) = maxl(n)
          IF lgpl = 0 THEN EXIT DO
          gap(n) = gap(n) + 1
          lgpl = lgpl - 1
         LOOP
        END IF
       END IF
       posit = posit + 4
      NEXT i
      ERASE assb, assn
      DO WHILE lgpl
       FOR i = 1 TO nos
        IF lgpl = 0 THEN EXIT FOR
        gap(i) = gap(i) + 1
        lgpl = lgpl - 1
       NEXT i
      LOOP
      post = 146
      FOR i = 1 TO nos
       spar(i).post = post
       x = gap(i)
       post = post + x
       spar(i).gpl = x
       spar(i).agr = 0
      NEXT i
      ERASE gap
      IF ctwlock = 0 THEN winoff
      ctrk = 0
END FUNCTION

FUNCTION ctws (linnum)
      seq = "": noop = 0: fflg = 0: xf = 0: ql = -1: fixql = 0
      DO
       posit = 1
       IF EOF(1) THEN EXIT DO
       LINE INPUT #1, v
       IF v = "" GOTO skln
       IF LEFT$(v, 1) = ";" AND LEFT$(v, 2) <> ";;" GOTO skln
       IF UCASE$(RTRIM$(v)) = "END" THEN EXIT DO
       IF fflg THEN
        FOR i = 1 TO 4
         GOSUB fnbc: IF f THEN xf = 1: EXIT FOR
         GOSUB lnum: IF xf THEN EXIT FOR
         seq = seq + CHR$(q)
        NEXT i
        IF xf THEN EXIT DO
        GOSUB fnbc
        IF posit <= LEN(v) THEN xf = 3: EXIT DO
        fflg = fflg - 1
       ELSE
        GOSUB fnbc
        IF f = 0 THEN
         x = INSTR(v, ":"): y = x - posit
         IF y < 3 THEN xf = 2: EXIT DO
         SELECT CASE UCASE$(MID$(v, posit, y))
          CASE "LOW": x = 0
          CASE "DOUBLE": x = 1
          CASE "MEDIUM": x = 2
          CASE "HIGH": x = 3
          CASE "QUAD": x = 4
          CASE ELSE: xf = 2: EXIT DO
         END SELECT
         seq = seq + CHR$(1) + CHR$(x)
         noop = noop + 1
         posit = posit + y + 1
         GOSUB fnbc
         IF posit <= LEN(v) THEN xf = 3: EXIT DO
        ELSE
         s = MID$(v, posit, 1)
         posit = posit + 1
         SELECT CASE UCASE$(s)
          CASE ";"
           seq = seq + CHR$(0) + CHR$(LEN(v) - 2) + MID$(v, 3)
           noop = noop + 1
          CASE "P"
           seq = seq + CHR$(2)
           noop = noop + 1
           GOSUB fnbc
           IF posit <= LEN(v) THEN xf = 3: EXIT DO
           pcmd = 1
          CASE "F"
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 1 OR q > 255 THEN posit = b: xf = 8: EXIT DO
           nosf = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           lsf = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           gplf = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           fb = q
           aflg = 0
           GOSUB fnbc
           IF UCASE$(MID$(v, posit, 1)) = "A" THEN
            IF pcmd = 0 THEN xf = 13: EXIT DO
            posit = posit + 1
            aflg = 1
            GOSUB fnbc: IF f THEN xf = 1: EXIT DO
            GOSUB lnum: IF xf THEN EXIT DO
            IF q < 0 OR q > 255 THEN posit = b: xf = 12: EXIT DO
            xa = q
            GOSUB fnbc: IF f THEN xf = 1: EXIT DO
            GOSUB lnum: IF xf THEN EXIT DO
            IF q < -32768 OR q > 32767 THEN posit = b: xf = 9: EXIT DO
            ca = q
            GOSUB fnbc
            pcmd = 0
           END IF
           IF posit <= LEN(v) THEN xf = 3: EXIT DO
           seq = seq + CHR$(3) + CHR$(nosf) + CHR$(lsf) + CHR$(gplf) + CHR$(fb) + CHR$(aflg)
           IF aflg THEN seq = seq + CHR$(xa) + MKI$(ca)
           noop = noop + 1
           fflg = nosf
          CASE "W"
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           idc = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           idh = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           idr = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           idn = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           s = UCASE$(MID$(v, posit, 1))
           posit = posit + 1
           SELECT CASE s
            CASE "N": dt = 0
            CASE "D": dt = 1
            CASE ELSE: xf = 4: EXIT DO
           END SELECT
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           s = UCASE$(MID$(v, posit, 4))
           IF s = "NONE" THEN
            qoff = -1
            posit = posit + 4
           ELSE
            GOSUB lnum: IF xf THEN EXIT DO
            IF q < 0 THEN posit = b: xf = 11: EXIT DO
            qoff = q
           END IF
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 32767 THEN posit = b: xf = 10: EXIT DO
           dmal = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           dtl = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           cmd7 = q
           GOSUB fnbc: IF f THEN xf = 1: EXIT DO
           GOSUB lnum: IF xf THEN EXIT DO
           IF q < 0 OR q > 255 THEN posit = b: xf = 7: EXIT DO
           cmd8 = q
           aflg = 0: pflg = 0
           GOSUB fnbc
           IF UCASE$(MID$(v, posit, 1)) = "P" THEN
            IF pcmd = 0 THEN xf = 13: EXIT DO
            posit = posit + 1
            pflg = 1
            GOSUB fnbc: IF f THEN xf = 1: EXIT DO
            GOSUB lnum: IF xf THEN EXIT DO
            IF q < 0 OR q > 255 THEN posit = b: xf = 12: EXIT DO
            xp = q
            GOSUB fnbc: IF f THEN xf = 1: EXIT DO
            GOSUB lnum: IF xf THEN EXIT DO
            IF q < -32768 OR q > 32767 THEN posit = b: xf = 9: EXIT DO
            cp = q
            GOSUB fnbc
           END IF
           IF UCASE$(MID$(v, posit, 1)) = "A" THEN
            IF pcmd = 0 THEN xf = 13: EXIT DO
            posit = posit + 1
            aflg = 1
            GOSUB fnbc: IF f THEN xf = 1: EXIT DO
            GOSUB lnum: IF xf THEN EXIT DO
            IF q < 0 OR q > 255 THEN posit = b: xf = 12: EXIT DO
            xa = q
            GOSUB fnbc: IF f THEN xf = 1: EXIT DO
            GOSUB lnum: IF xf THEN EXIT DO
            IF q < -32768 OR q > 32767 THEN posit = b: xf = 9: EXIT DO
            ca = q
            GOSUB fnbc
            pcmd = 0
           END IF
           IF posit <= LEN(v) THEN xf = 3: EXIT DO
           IF qoff <> -1 AND (fixql = 0 OR qoff < ql) THEN ql = qoff: fixql = 1
           seq = seq + CHR$(4) + CHR$(idc) + CHR$(idh) + CHR$(idr) + CHR$(idn) + CHR$(dt) + MKL$(qoff) + MKI$(dmal) + CHR$(dtl) + CHR$(cmd7) + CHR$(cmd8) + CHR$(pflg)
           IF pflg THEN seq = seq + CHR$(xp) + MKI$(cp)
           seq = seq + CHR$(aflg)
           IF aflg THEN seq = seq + CHR$(xa) + MKI$(ca)
           noop = noop + 1
          CASE ELSE: xf = 5: posit = posit - 1: EXIT DO
         END SELECT
        END IF
       END IF
skln:  linnum = linnum + 1
      LOOP
      IF xf THEN
       s = "Error in line" + STR$(linnum)
       SELECT CASE xf
        CASE 1: se = "Expected: tab or space"
        CASE 2: se = "Expected: recording density"
        CASE 3: se = "Expected: end-of-statement"
        CASE 4: se = "Expected: data type"
        CASE 5: se = "Expected: command"
        CASE 6: se = "Expected: number"
        CASE 7: se = "FDC parameter must be of range 0...255"
        CASE 8: se = "Number of sectors must be of range 1...255"
        CASE 9: se = "Positioning/abort time must be of range -32768...32767"
        CASE 10: se = "DMA data length must be of range 0...32767"
        CASE 11: se = "Offset in BIN file must be of range 0...7FFFFFFFH"
        CASE 12: se = "Number of disk full turns must be of range 0...255"
        CASE 13: se = "Timer using without preceeding P command"
       END SELECT
       sf = STRING$(posit - 1, 32) + "^ " + se
       x = 0
       IF LEN(s) > x THEN x = LEN(s)
       IF LEN(v) > x THEN x = LEN(v)
       IF LEN(sf) > x THEN x = LEN(sf)
       IF x > 68 THEN
        a = posit - 1: b = a + LEN(se) - 66
        IF b < 0 THEN b = 0
        FOR i = a TO b + 1 STEP -1
         IF LEN(v) - i > 67 THEN EXIT FOR
        NEXT i
        a = i
        m = (a + b) \ 2
        v = MID$(v, m + 1, 68): sf = MID$(sf, m + 1, 68): x = 68
       END IF
       post = (74 - x) \ 2
       wincre 8, post, 9, x + 6, "Error", 2
       fprint 10, post + 3, s, 7
       fprint 11, post + 3, v, 7
       fprint 12, post + 3, sf, 7
       mkbut 14, 35, 10, "Ok", 1, 12, 18
       mon
ctws1: f = 0: WHILE f = 0: mkey f, a, b, bs: WEND
       IF f = 1 THEN
        IF a = 13 OR a = 27 GOTO ctws2
       ELSE
        IF a = 14 AND b > 34 AND b < 46 GOTO ctws2
       END IF
       GOTO ctws1
ctws2: moff: winoff: ctws = 1: EXIT FUNCTION
      END IF
      seq = MKL$(ql) + CHR$(noop) + seq
      ctws = 0
      EXIT FUNCTION
fnbc: f = 1
      DO
       s = MID$(v, posit, 1)
       IF s = " " OR s = CHR$(9) THEN f = 0 ELSE EXIT DO
       posit = posit + 1
      LOOP
      RETURN
lnum: IF MID$(v, posit, 1) = "" THEN xf = 6: RETURN
      b = posit: fh = 0: fm = 0: fd = 0: sf = ""
      DO
       s = UCASE$(MID$(v, posit, 1))
       IF s = "" OR s = " " OR s = CHR$(9) THEN EXIT DO
       posit = posit + 1
       ff = 1
       IF s = "-" AND fd = 0 THEN fm = 1: ff = 0: fd = 1
       IF (s >= "0" AND s <= "9" OR s >= "A" AND s <= "F") AND fh = 0 THEN sf = sf + s: ff = 0: fd = 1
       IF s = "H" AND fm = 0 AND fd = 1 AND fh = 0 THEN fh = 1: ff = 0
       IF ff THEN xf = 6: EXIT DO
      LOOP
      IF xf THEN posit = b: RETURN
      q = 0: IF fh THEN a = 16 ELSE a = 10
      FOR k = 1 TO LEN(sf)
       x = ASC(MID$(sf, k, 1))
       SELECT CASE x
        CASE 48 TO 57: x = x - 48
        CASE 65 TO 70: x = x - 55
       END SELECT
       IF x >= a THEN xf = 6: EXIT FOR
       q = q * a + x
      NEXT k
      IF fm THEN q = -q
      IF xf THEN posit = b
      RETURN
END FUNCTION

SUB dtws
      posit = 6
      FOR i = 1 TO ASC(MID$(seq, 5, 1))
       x = ASC(MID$(seq, posit, 1))
       posit = posit + 1
       SELECT CASE x
       CASE 0
        x = ASC(MID$(seq, posit, 1))
        posit = posit + 1
        PRINT #1, ";;" + MID$(seq, posit, x)
        posit = posit + x
       CASE 1
        x = ASC(MID$(seq, posit, 1))
        posit = posit + 1
        SELECT CASE x
         CASE 0: s = "Low"
         CASE 1: s = "Double"
         CASE 2: s = "Medium"
         CASE 3: s = "High"
         CASE 4: s = "Quad"
        END SELECT
        PRINT #1, "        " + s + ":"
       CASE 2
        PRINT #1, "P"
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
        s = "F" + STR$(nosf) + STR$(lsf) + STR$(gplf) + STR$(fb)
        v = STR$(ca): IF LEFT$(v, 1) <> " " THEN v = " " + v
        IF aflg THEN s = s + " A" + STR$(xa) + v
        PRINT #1, s
        FOR j = 1 TO nosf
         idc = ASC(MID$(seq, posit, 1))
         idh = ASC(MID$(seq, posit + 1, 1))
         idr = ASC(MID$(seq, posit + 2, 1))
         idn = ASC(MID$(seq, posit + 3, 1))
         posit = posit + 4
         PRINT #1, STR$(idc) + STR$(idh) + STR$(idr) + STR$(idn)
        NEXT j
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
        s = "W" + STR$(idc) + STR$(idh) + STR$(idr) + STR$(idn)
        IF dt THEN s = s + " D" ELSE s = s + " N"
        IF qoff = -1 THEN s = s + " None" ELSE s = s + " " + HEX$(qoff) + "H"
        s = s + STR$(dmal) + STR$(dtl) + STR$(cmd7) + STR$(cmd8)
        v = STR$(cp): IF LEFT$(v, 1) <> " " THEN v = " " + v
        IF pflg THEN s = s + " P" + STR$(xp) + v
        v = STR$(ca): IF LEFT$(v, 1) <> " " THEN v = " " + v
        IF aflg THEN s = s + " A" + STR$(xa) + v
        PRINT #1, s
       END SELECT
      NEXT i
END SUB

FUNCTION fwslabel (sn, BYVAL noe, qpars())
      cyl = 0: head = 0: wrirst
      xf = 1: posit = 6
      FOR i = 1 TO ASC(MID$(seq, 5, 1))
       x = ASC(MID$(seq, posit, 1)): posit = posit + 1
       SELECT CASE x
        CASE 0
         x = ASC(MID$(seq, posit, 1))
         s = MID$(seq, posit + 1, x)
         posit = posit + x + 1
         x = INSTR(s, "!" + sn)
         IF x THEN xf = 0: EXIT FOR
        CASE 1
         x = ASC(MID$(seq, posit, 1)): posit = posit + 1
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
      IF xf THEN fwslabel = 1: EXIT FUNCTION
      posit = posit - LEN(s) + x + LEN(sn)
      f = 0
      FOR i = 1 TO noe
       GOSUB fnbf: IF f THEN EXIT FOR
       GOSUB lnuf: IF f THEN EXIT FOR
       qpars(i) = q
      NEXT i
      fwslabel = f: EXIT FUNCTION
fnbf: f = 1
      DO
       s = MID$(seq, posit, 1)
       IF s = " " OR s = CHR$(9) THEN f = 0 ELSE EXIT DO
       posit = posit + 1
      LOOP
      RETURN
lnuf: IF MID$(seq, posit, 1) = "" THEN f = 1: RETURN
      fh = 0: fm = 0: fd = 0: sf = ""
      DO
       s = UCASE$(MID$(seq, posit, 1))
       IF s = "" OR s = " " OR s = CHR$(9) THEN EXIT DO
       posit = posit + 1
       ff = 1
       IF s = "-" AND fd = 0 THEN fm = 1: ff = 0: fd = 1
       IF (s >= "0" AND s <= "9" OR s >= "A" AND s <= "F") AND fh = 0 THEN sf = sf + s: ff = 0: fd = 1
       IF s = "H" AND fm = 0 AND fd = 1 AND fh = 0 THEN fh = 1: ff = 0
       IF ff THEN f = 1: EXIT DO
      LOOP
      IF f THEN RETURN
      q = 0: IF fh THEN a = 16 ELSE a = 10
      FOR k = 1 TO LEN(sf)
       x = ASC(MID$(sf, k, 1))
       SELECT CASE x
        CASE 48 TO 57: x = x - 48
        CASE 65 TO 70: x = x - 55
       END SELECT
       IF x >= a THEN f = 1: EXIT FOR
       q = q * a + x
      NEXT k
      IF fm THEN q = -q
      RETURN
END FUNCTION

SUB gtdident
      IF ncyl < 40 OR ncyl > 45 AND ncyl < 80 OR ncyl > 90 THEN EXIT SUB
      cyl = 0: head = 0
      fmtrst
      IF nos = 0 OR dtf = 0 THEN EXIT SUB
      SELECT CASE dot
       CASE 0: EXIT SUB
       CASE 1: nsec = 9: npsec = 11: gpl = 20: abtpos = 6238
       CASE 2: EXIT SUB
       CASE 3: nsec = 15: npsec = 18: EXIT SUB ' Due to lack of info
       CASE 4: nsec = 18: npsec = 22: EXIT SUB ' Due to lack of info
      END SELECT
      dens = dot
      IF nos <> npsec THEN EXIT SUB
      REDIM layout(1 TO npsec), dossec(1 TO nsec)
      SELECT CASE dens
       CASE 1
        layout(1) = 11: layout(2) = 1: layout(3) = 6: layout(4) = 2
        layout(5) = 7: layout(6) = 3: layout(7) = 8: layout(8) = 4
        layout(9) = 9: layout(10) = 5: layout(11) = 10
        dossec(1) = 2: dossec(2) = 4: dossec(3) = 6: dossec(4) = 8
        dossec(5) = 10: dossec(6) = 3: dossec(7) = 5: dossec(8) = 7
        dossec(9) = 9
       ' Insert tables for High and Quad here
      END SELECT
      xf = 0
      FOR i = 1 TO npsec
       IF spar(i).c <> cyl THEN xf = 1: EXIT FOR
       IF spar(i).h <> head THEN xf = 1: EXIT FOR
       IF spar(i).r <> layout(i) THEN xf = 1: EXIT FOR
       IF spar(i).n <> 2 THEN xf = 1: EXIT FOR
       IF (spar(i).crc AND 4) = 0 AND i <> npsec THEN xf = 1: EXIT FOR
       IF spar(i).crc AND 4 AND i = npsec THEN xf = 1: EXIT FOR
       IF spar(i).dt THEN xf = 1: EXIT FOR
      NEXT i
      IF xf THEN EXIT SUB
      qoff = spar(1).offset
      OPEN sopdir + "DISK.BIN" FOR BINARY ACCESS READ AS #1
      SEEK #1, qoff + 1
      s = STRING$(512, 0)
      GET #1, , s
      CLOSE #1
      IF MID$(s, 6, 30) <> "ƒàã§®¢ ï â ¬®¦¥­­ ï ¤¥ª« à æ¨ï" THEN EXIT SUB
      IF ASC(RIGHT$(s, 1)) > 15 THEN EXIT SUB
      inscntr = 15
      FOR i = 0 TO inscntr
       cyl = i: head = 0: fmtrst
       seq = CHR$(1) + CHR$(dens): noop = 1: ql = -1: fixql = 1
       seq = seq + CHR$(3) + CHR$(1) + CHR$(1) + CHR$(1) + "ö" + CHR$(0)
       seq = seq + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(0)
       noop = noop + 1
       seq = seq + CHR$(2): noop = noop + 1
       seq = seq + CHR$(3) + CHR$(npsec) + CHR$(2) + CHR$(gpl) + CHR$(13) + CHR$(1) + CHR$(1) + MKI$(abtpos)
       FOR j = 1 TO npsec
        seq = seq + CHR$(cyl) + CHR$(head) + CHR$(layout(j)) + CHR$(2)
       NEXT j
       noop = noop + 1
       FOR j = 1 TO nsec
        q = spar(dossec(j)).offset
        seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(j) + CHR$(2) + CHR$(0) + MKL$(q) + MKI$(512) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
        noop = noop + 1
        IF q < ql OR fixql THEN ql = q: fixql = 0
       NEXT j
       IF cyl = 0 THEN
        s = " !GTD" + STR$(dens) + " " + HEX$(qoff) + "H" + STR$(inscntr)
        seq = seq + CHR$(0) + CHR$(LEN(s)) + s
        noop = noop + 1
       END IF
       seq = MKL$(ql) + CHR$(noop) + seq
       wriupd
      NEXT i
END SUB

DEFSNG A-Q, S-Z
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
SUB gtdregen
      POKE &H40, 0
      REDIM qpars(1 TO 3)
      x = fwslabel("GTD", 3, qpars())
      IF x THEN EXIT SUB
      IF qpars(1) <> 1 AND qpars(1) <> 3 AND qpars(1) <> 4 THEN EXIT SUB
      dens = qpars(1)
      qoff = qpars(2)
      IF qpars(3) < 0 OR qpars(3) > 15 THEN EXIT SUB
      inscntr = qpars(3)
      ERASE qpars
      POKE &H40, 0
      SELECT CASE dens
       CASE 1: nsec = 9: npsec = 11
       CASE 3: nsec = 15: npsec = 18
       CASE 4: nsec = 18: npsec = 22
      END SELECT
      regs.ax = &H4200
      regs.bx = filnum
      regs.cx = e243(qoff)
      regs.dx = e241(qoff)
      interruptx &H21, regs, regs
      regs.ax = &H3F00
      regs.bx = filnum
      regs.cx = 512
      regs.dx = dmaboff
      regs.ds = dmabseg
      interruptx &H21, regs, regs
      s = STRING$(128, 0)
      dmabrd 0, s, 128
      POKE &H40, 0
      xstrcnt = &H4454
      xstrsum = &H4454
      posit = 3
      DO
       IF posit > 128 THEN EXIT DO
       x = ASC(MID$(s, posit, 1))
       IF x = 0 THEN EXIT DO
       IF x > 32 AND x < 127 OR x > 127 AND x < 255 THEN
        q = i2l(xstrcnt) + 1
        xstrcnt = e241(q)
        q = (q + x XOR i2l(xstrsum)) AND 65535
        IF q AND 1 THEN q = q + 65536
        xstrsum = e241(q \ 2)
       END IF
       posit = posit + 1
      LOOP
      s = CHR$(inscntr)
      dmabwr 511, s, 1
      FOR i = 0 TO inscntr
       POKE &H40, 0
       cyl = i: head = 0: hdd = drive
       hdpos cyl: IF erflg GOTO er2
       x = ixfr(dens, 2)
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = npsec - 1
       cmd(6) = 2
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 512, 512
       dbtc 9: IF erflg GOTO er2
       waint
       IF erflg GOTO er2
       n = csfc: IF erflg GOTO er2
       x = fdaamf(dmabseg, e241(i2l(dmaboff) + 512), 512, 3)
       qbx = (515 - x \ 8) * 256& AND 65535
       s = STRING$(512, 0)
       dmabrd 512, s, 512
       qax = findif(s, 512) * 256& AND 65535
       FOR j = 1 TO 8
        a = e141(qax)
        qax = e142(qax) * 256& + e141(qbx)
        qbx = e142(qbx) * 256& + a
        qax = qax \ 2
        qbx = qbx \ 2
       NEXT j
       x = e241(qax * 256 + qbx) XOR xstrsum
       s = MKI$(x)
       dmabwr 0, s, 2
       POKE &H40, 0
       cmd(1) = &H45
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = npsec
       cmd(6) = 2
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H4A, 0, 512
       dbtc 9: IF erflg GOTO er2
       waint
       IF erflg GOTO er2
       n = csfc: IF erflg GOTO er2
      NEXT i
      
er2:
END SUB

FUNCTION obr
      drt = drttbl(dot): dopl = doptbl(dot)
      IF dot < 3 THEN maxlen = 6 ELSE maxlen = 7
      IF dtf GOTO obpt

      seq = MKL$(spar(1).offset) + CHR$(3) + CHR$(1) + CHR$(dot) + CHR$(3) + CHR$(nos) + CHR$(alen) + CHR$(agpl) + "ö" + CHR$(0)
      FOR i = 1 TO nos
       seq = seq + CHR$(cyl) + CHR$(head) + CHR$(i) + CHR$(alen)
      NEXT i
      seq = seq + CHR$(4) + CHR$(cyl) + CHR$(head) + CHR$(1) + CHR$(alen) + CHR$(0) + MKL$(spar(1).offset) + MKI$(nos * lensec(alen)) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
      obr = 0
      EXIT FUNCTION

obpt: aoerr = 0

      FOR idrf = 0 TO 255
       xf = 1
       FOR i = 1 TO nos
        IF spar(i).r = idrf THEN xf = 0: EXIT FOR
       NEXT i
       IF xf THEN EXIT FOR
      NEXT idrf
      IF spar(nos).dt <> 2 THEN
       n = spar(nos).n: IF n > 7 THEN n = 7
       x = gpltab(n)
       'x = gpltab(spar(nos).n) ' WARNING
      ELSE
       x = 22
      END IF
      IF x > spar(nos).gpl - dop + 146 THEN lslf = 1

      REDIM pst(1 TO nos), pstt(1 TO nos)
      FOR i = 1 TO nos
       pst(i) = spar(i).post - spar(1).post
       pstt(i) = CLNG(dopl) * pst(i) / drt + dop
      NEXT i

      REDIM pstx(1 TO nos)
      REDIM xn(1 TO nos * 2), xl(1 TO nos * 2), xg(1 TO nos * 2), xb(1 TO nos * 2)
      REDIM si(1 TO nos * 2), ct(1 TO nos * 2), cf(1 TO nos * 2)
      REDIM bf(1 TO nos * 2), ef(1 TO nos * 2)
      REDIM ncc(1 TO nos), scc(1 TO nos), scn(1 TO nos)
      REDIM ffw(1 TO nos), cid(1 TO nos), idnr(1 TO nos)
      REDIM idnf(0)
      lend = 1: ql = -1: fixql = 1: gplf = 1: minpos = 0
      FOR i = 1 TO nos
       n = spar(i).n: IF n > 7 THEN na = 7 ELSE na = n
       ngpl = pst(i): IF i = nos THEN gap = drt - ngpl - iamlen ELSE gap = pst(i + 1) - ngpl
       agaplen = spar(i).agr \ 4096
       agapsts = spar(i).agr \ 256 AND 3
       agapgpl = spar(i).agr AND 255

       REDIM pg(maxlen, 1 TO 255), mg(maxlen, 1 TO 255)
       mwa = -32768
       FOR j = 0 TO maxlen
        FOR k = 1 TO 255
         xk = gpltab(j) + k
         xf = 0: yf = 0: y = 0: f = 0: minposc = minpos
         FOR l = i TO nos
          yf = yf + y
          IF f THEN EXIT FOR
          a = pst(l) / xk
          IF a >= 255 THEN EXIT FOR
          poscur = a * xk
          x = ABS(poscur - pst(l))
          IF x > pstt(l) THEN EXIT FOR
          IF poscur < minposc THEN EXIT FOR
          xf = xf + x * x
          IF l = nos THEN gapp = drt - pst(l) - iamlen ELSE gapp = pst(l + 1) - pst(l)
          y = 0: IF l = nos THEN nk = 1 ELSE nk = gapp / xk
          IF spar(l).dt = 2 AND (n OR nobug OR l <> 1 OR gapp - gpltab(0) - 1 < dop) THEN f = 1
          IF f = 0 AND nk > 1 THEN
           a = 0
           IF spar(l).n >= maxlen THEN
            mc = 255
           ELSE
            FOR mc = 0 TO mns
             IF gpltab(spar(l).n) - a < 12 THEN EXIT FOR
             a = a + xk
            NEXT mc
           END IF
           IF mc < 2 THEN f = 1
           IF nk > mc AND f = 0 THEN y = 1
          END IF
          IF spar(l).dt <> 2 THEN
           FOR m = maxlen - 1 TO 0 STEP -1
            IF spar(l).crc AND power(m) THEN EXIT FOR
           NEXT m
           IF m >= 0 THEN minposc = poscur + gpltab(m) ELSE minposc = poscur + 60
          ELSE
           minposc = poscur + 22
          END IF
         NEXT l
         a = l - i
         IF a THEN
          pg(j, k) = 1
          x = a * 10 - xf / a - yf
          IF l = nos + 1 AND drt - pst(nos) - iamlen - gpltab(j) > dop THEN x = x + 10
          mg(j, k) = x
          IF x > mwa THEN mwa = x
         END IF
       NEXT k, j

       REDIM le(maxlen), mw(maxlen), gp(maxlen)
       mw(0) = -32768: mw(1) = -32768: mw(2) = -32768: mw(3) = -32768: mw(4) = -32768: mw(5) = -32768: mw(6) = -32768: IF maxlen = 7 THEN mw(7) = -32768
       FOR j = 0 TO maxlen
        FOR k = 1 TO 255
         IF pg(j, k) THEN
          le(j) = 1
          IF mg(j, k) > mw(j) THEN mw(j) = mg(j, k): gp(j) = k
         END IF
       NEXT k, j

       IF lend THEN
        fb = ASC("ö"): fixfb = 0
       END IF

       IF spar(i).dt = 2 THEN
        IF lend = 0 AND pg(lsf, gplf) THEN
         ls = lsf: gpl = gplf
        ELSE
         FOR j = maxlen TO 0 STEP -1
          IF le(j) AND mw(j) = mwa THEN EXIT FOR
         NEXT j
         IF j = -1 THEN aoerr = 1 ELSE ls = j: gpl = gp(j)
        END IF
        IF n = 0 AND nobug = 0 AND i = 1 AND gap - gpltab(0) - 1 > dop THEN fw = 2 ELSE fw = 1
        kir = 1: kc = 1: nl = n
       ELSE
        xf = -32768
        formswi = -90    'Format switching
        idsubst = -90    'ID substitution
        formbrk = -90    'Format break
        noptfor = -90    'Non-optimum format
        normwri = -10    'Normal write
        cabtwri = -70    'Write with abort
        pabtwri = -200   'Write with high-precision abort
        casowri = -10    'Cascade overwrite
        nwrigap = -200   'Unwritten gap
        corrgap = 250    'Correct gap contents
        SELECT CASE chid
         CASE 0: flg = 3
         CASE 1: IF i = 1 THEN flg = 5 ELSE flg = 3
         CASE 2: flg = 5
        END SELECT
        IF lssf AND lslf AND i = nos THEN flg = 3
        FOR j = maxlen TO 0 STEP -1
         IF le(j) THEN
          FOR k = 255 TO 1 STEP -1
           IF pg(j, k) THEN
            xk = gpltab(j) + k
            FOR l = 0 TO flg
             x = 0
             IF l = 0 THEN
              IF gpltab(j) > gap THEN a = gap: b = 1 ELSE a = gpltab(j): b = 0
              IF spar(i).dt GOTO invalsel
              IF i = nos AND lslf GOTO invalsel
              FOR m = 0 TO maxlen - 1
               IF spar(i).csn AND power(m) THEN
                IF m = j AND b = 0 AND (spar(i).crc AND power(m)) = 0 GOTO invalsel
                IF (m <> j OR b <> 0) AND spar(i).crc AND power(m) GOTO invalsel
               END IF
              NEXT m
              IF a < gpltab(na) THEN
               IF b GOTO invalsel
               IF spar(i).dif <> lensec(j) GOTO invalsel
              ELSE
               IF spar(i).dif < lensec(na) GOTO invalsel
              END IF
              IF gap < gpltab(na) THEN a = gap ELSE a = gpltab(n)
              IF a - gpltab(j) > dop OR gap - gpltab(na) > dop AND lensec(na) < spar(i).datlen THEN x = x + nwrigap
              IF lend = 0 AND (j <> lsf OR k <> gplf OR fb <> e121(spar(i).sfb) AND fixfb) THEN x = x + formswi: fsf = 1 ELSE fsf = 0
              IF (lend OR fsf) AND mg(j, k) < mwa THEN x = x + noptfor
              IF i <> nos AND ABS(gap - xk) > dop THEN x = x + formbrk
              IF agapsts = 3 AND agaplen = j AND agapgpl = k THEN x = x + corrgap
              nk = 1: mc = 1: nc = n
             ELSEIF l = 1 THEN
              GOTO invalsel
             ELSE
              SELECT CASE l
               CASE 2
                IF (spar(i).crc AND power(n)) = 0 GOTO invalsel
                IF gpltab(na) - gap > dop GOTO invalsel
                IF n = 0 GOTO invalsel
                IF gap - gpltab(na) > dop AND lensec(na) < spar(i).datlen THEN x = x + nwrigap
                x = x + normwri
                nc = n
               CASE 3
                IF spar(i).crc AND power(n) GOTO invalsel
                IF gap < gpltab(na) THEN a = cabtwri ELSE a = pabtwri
                IF n THEN x = x + a ELSE GOTO invalsel
                IF gap - gpltab(na) > dop AND lensec(na) < spar(i).datlen THEN x = x + nwrigap
                nc = n
               CASE 4
                'nc = n ' WARNING
                FOR nc = n + 1 TO maxlen
                 IF gpltab(nc) - gap > dop THEN EXIT FOR
                NEXT nc
                IF nc THEN nc = nc - 1
                IF nc = n GOTO invalsel
                IF lensec(na) >= spar(i).datlen AND spar(i).crc AND power(n) GOTO invalsel
                IF spar(i).csn AND power(nc) AND (spar(i).crc AND power(nc)) = 0 GOTO invalsel
                IF lensec(nc) < spar(i).datlen THEN a = gpltab(nc) ELSE a = spar(i).datlen + 62
                IF gap < spar(i).datlen + 62 THEN b = gap ELSE b = spar(i).datlen + 62
                IF a < b THEN x = x + nwrigap
                x = x + idsubst + normwri
               CASE 5
                'nc = n ' WARNING
                FOR nc = n TO maxlen
                 IF gpltab(nc) - gap > dop THEN EXIT FOR
                NEXT nc
                IF nc = n GOTO invalsel
                IF lensec(na) >= spar(i).datlen AND spar(i).crc AND power(n) GOTO invalsel
                IF spar(i).crc AND power(nc) GOTO invalsel
                IF gap - gpltab(na) > dop THEN x = x + cabtwri ELSE x = x + pabtwri
                x = x + idsubst
              END SELECT
              IF i = nos THEN nk = 1 ELSE nk = gap / xk: IF nk = 0 THEN nk = 1
              a = 0
              IF nc >= maxlen THEN
               mc = 255
              ELSE
               FOR mc = 0 TO mns
                IF gpltab(nc) - a < 12 THEN EXIT FOR
                a = a + xk
               NEXT mc
              END IF
              IF mc > 1 AND nk > mc THEN x = x + casowri
              fsf = 0
              IF lend = 0 THEN
               IF l < 4 THEN
                IF j <> lsf OR k <> gplf THEN x = x + formswi: fsf = 1
               ELSE
                IF ngpx MOD xk THEN x = x + formswi: fsf = 1
               END IF
              END IF
              IF (lend OR fsf OR l > 3) AND mg(j, k) < mwa THEN x = x + noptfor
              'IF l > 3 AND mg(j, k) < mwa AND (ls <> lsf OR gpl <> gplf) THEN x = x + noptfor ' WARNING
              IF nk > 1 AND mc < 2 OR i <> nos AND ABS(gap - xk * nk) > dop THEN x = x + formbrk
              IF (l = 2 OR l = 4) AND nk = 1 AND agapsts = 2 AND agaplen = nc THEN x = x + corrgap
              IF (l = 2 OR l = 4) AND nk <> 1 AND agapsts = 1 AND agaplen = nc AND agapgpl = k THEN x = x + corrgap
             END IF
             IF x > xf THEN xf = x: ls = j: gpl = k: fw = l: kir = nk: kc = mc: nl = nc: fsflg = fsf
invalsel:   NEXT l
           END IF
          NEXT k
         END IF
        NEXT j
        IF xf < -20000 THEN aoerr = 1
       END IF

       IF aoerr THEN EXIT FOR
       xk = gpltab(ls) + gpl
       IF fw < 2 AND spar(i).dt <> 2 AND fixfb = 0 THEN fb = e121(spar(i).sfb)
       'IF fw < 2 AND spar(i).dt <> 2 THEN ' WARNING
       ' nfb = e121(spar(i).sfb)
       ' IF fixfb = 0 THEN fixfb = 1: fb = nfb
       'ELSE
       ' nfb = fb
       'END IF
       IF lend OR fsflg THEN
bform:  x = ngpl / xk: ngpx = x * xk
        IF nof THEN
         IF labt THEN ct(nof) = ngpx + abtkid
         IF lcf = 2 THEN
          IF ngpx - pstx(i - 1) - ln > gpltab(0) - 44 THEN cf(nof) = ngpx + abtkid
         ELSEIF lcf = 1 THEN
          IF ngpx - pstx(i - 1) - ln > gpltab(0) THEN cf(nof) = ngpx + abtkid
         END IF
        END IF
        nof = nof + 1
        bf(nof) = i
        lsf = ls: gplf = gpl
        xl(nof) = lsf: xg(nof) = gplf
        xn(nof) = x
        FOR j = 1 TO x
         si(nof) = si(nof) + CHR$(0) + CHR$(0) + CHR$(idrf) + CHR$(0)
        NEXT j
       ELSE
        ngpx = ngpx + lxk * lkir
        FOR j = 2 TO lkir
         si(nof) = si(nof) + CHR$(i - 1) + CHR$(j) + CHR$(idrf) + CHR$(idnf(j))
        NEXT j
        xn(nof) = xn(nof) + lkir - 1
        ncc(i - 1) = nck: scc(i - 1) = sr: scn(i - 1) = sn
       END IF
       si(nof) = si(nof) + CHR$(spar(i).c) + CHR$(spar(i).h) + CHR$(spar(i).r) + CHR$(n)
       xn(nof) = xn(nof) + 1
       IF fw = 1 THEN
        IF i = 1 THEN ln = bdmpos ELSE ln = 33
        lcf = 2: x = 0
       ELSEIF fw > 3 THEN
        ln = 33: lcf = 0: x = 0
       ELSE
        ln = gpltab(ls)
        'IF fw = 1 THEN ln = ln - dop ' WARNING
        lcf = 1: x = dop \ 2
       END IF
       labt = 0
       IF gap - ln < dop THEN
        IF i = nos THEN lfabt = 1 ELSE labt = 1
       ELSE
        ln = ln + x
        ct(nof) = ngpx + ln
        IF i = nos AND fw <> 1 AND fw < 4 THEN ct(nof) = 0
       END IF
       IF fw = 2 OR fw = 3 THEN
        a = gpltab(na)
        IF ln < a THEN ln = a
       END IF

       IF fw > 3 THEN
        n = nl: IF n > 7 THEN na = 7 ELSE na = n
        fw = fw - 2: cid(i) = 1
        GOTO bform
       END IF

       ef(nof) = i
       IF fw = 1 THEN lend = 1 ELSE lend = 0
       IF i <> nos THEN
        IF ABS(pst(i + 1) - xk * (xn(nof) - 1 + kir)) > pstt(i + 1) THEN lend = 1
       END IF
       IF xn(nof) + kir > 255 THEN lend = 1
       IF kir > 1 AND kc < 2 THEN lend = 1
       ffw(i) = fw: idnr(i) = n
       IF lend = 0 THEN
        lxk = xk: lkir = kir: nck = 0: sr = "": sn = ""
        REDIM idnf(1 TO kir)
        a = kir - kc: b = kir
        DO WHILE a > 0
         FOR j = 0 TO maxlen - 1
          x = 0
          FOR k = 0 TO mns
           IF gpltab(j) - x < 12 THEN EXIT FOR
           x = x + xk
          NEXT k
          k = k - 1
          IF k - 1 < a THEN xf = j: yf = k
         NEXT j
         a = a - yf: b = b - yf
         idnf(b) = xf
         sr = CHR$(b) + sr: sn = CHR$(xf) + sn: nck = nck + 1
        LOOP
       END IF
       IF spar(i).dt <> 2 THEN
        FOR j = maxlen - 1 TO 0 STEP -1
         IF spar(i).crc AND power(j) THEN EXIT FOR
        NEXT j
        IF j >= 0 THEN minpos = ngpx + gpltab(j) ELSE minpos = ngpx + 60
       ELSE
        minpos = ngpx + 22
       END IF
       xb(nof) = fb
       IF fw > 1 AND spar(i).offset <> -1 AND (spar(i).offset < ql OR fixql) THEN ql = spar(i).offset: fixql = 0
       pstx(i) = ngpx
      NEXT i
      ERASE pst, pstt, pg, mg, le, mw, gp, idnf

      IF aoerr THEN
aoerr: s = " Could not build write sequence for this track"
       seq = MKL$(-1) + CHR$(1) + CHR$(0) + CHR$(LEN(s)) + s
       obr = 1: EXIT FUNCTION
      END IF

      seq = "": noop = 0
      IF iamlen = 0 THEN
       s = " IAM is overwritten on this track"
       seq = seq + CHR$(0) + CHR$(LEN(s)) + s
       noop = noop + 1
      END IF

      xf = 0
      FOR i = 2 TO nos
       IF cid(i) THEN xf = 1: EXIT FOR
      NEXT i
      IF xf THEN
       IF lssf AND lslf THEN ff = 1: fw = 1 ELSE ff = 1: fw = 0
      ELSE
       IF cid(1) THEN ff = 2: fw = 3 ELSE ff = 1: fw = 2
      END IF

      seq = seq + CHR$(1) + CHR$(dot): noop = noop + 1
      FOR i = nof TO ff STEP -1
       IF cf(i) OR i = nof AND (ct(i) OR lfabt) THEN
        IF cf(i) THEN seq = seq + CHR$(2): noop = noop + 1
        seq = seq + CHR$(3) + CHR$(1) + CHR$(1) + CHR$(1) + "ö"
        IF cf(i) THEN seq = seq + CHR$(1) + CHR$(1) + MKI$(cf(i) + 146) ELSE seq = seq + CHR$(0)
        seq = seq + CHR$(0) + CHR$(0) + CHR$(idrf) + CHR$(0)
        noop = noop + 1
       END IF
       IF ct(i) OR lfabt THEN seq = seq + CHR$(2): noop = noop + 1
       seq = seq + CHR$(3) + CHR$(xn(i)) + CHR$(xl(i)) + CHR$(xg(i)) + CHR$(xb(i))
       IF lfabt THEN
        'seq = seq + CHR$(1) + CHR$(2) + MKI$(-iamlen + abtkid + 146): lfabt = 0 ' WARNING
        seq = seq + CHR$(1) + CHR$(2) + MKI$(abtkid): lfabt = 0
       ELSEIF ct(i) THEN
        seq = seq + CHR$(1) + CHR$(1) + MKI$(ct(i) + 146)
       ELSE
        seq = seq + CHR$(0)
       END IF
       seq = seq + si(i)
       noop = noop + 1
       IF fw < 2 THEN
        bf = bf(i): ef = ef(i)
        IF fw = 1 AND ef = nos THEN ef = nos - 1
        GOSUB wrsec
       END IF
      NEXT i

      IF fw THEN
       IF fw = 1 THEN bf = nos ELSE bf = 1
       ef = nos
       GOSUB wrsec
      END IF

      seq = MKL$(ql) + CHR$(noop) + seq

      ERASE ncc, scc, scn, ffw, idnr, cid, pstx, xn, xl, xg, xb, si, ct, cf, bf, ef
      obr = 0
      EXIT FUNCTION

wrsec:
      DO
       xf = 1
       FOR j = bf TO ef
        IF ncc(j) THEN
         seq = seq + CHR$(4) + CHR$(j) + MID$(scc(j), ncc(j), 1) + CHR$(idrf) + MID$(scn(j), ncc(j), 1) + CHR$(0) + MKL$(-1) + MKI$(lensec(ASC(MID$(scn(j), ncc(j), 1)))) + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(0) + CHR$(0)
         noop = noop + 1
         ncc(j) = ncc(j) - 1
         xf = 0
        END IF
       NEXT j
      LOOP UNTIL xf
      xf = 0
      FOR j = 1 TO nos STEP 2
       IF j > bf - 1 AND j < ef + 1 AND ffw(j) = 2 AND (j <> nos OR lslf = 0) THEN
        IF xf = 0 THEN
         seq = seq + CHR$(2)
         noop = noop + 1
         xf = 1
        END IF
        'IF j = nos THEN gap = drt - pstx(nos) - iamlen ELSE gap = pstx(j + 1) - pstx(j) ' WARNING
        seq = seq + CHR$(4) + CHR$(spar(j).c) + CHR$(spar(j).h) + CHR$(spar(j).r) + CHR$(idnr(j))
        seq = seq + CHR$(spar(j).dt AND 1) + MKL$(spar(j).offset) + MKI$(lensec(idnr(j)))
        seq = seq + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(1) + CHR$(1) + MKI$(pstx(j) + 146) + CHR$(0)
        noop = noop + 1
       END IF
      NEXT j
      FOR j = 2 TO nos STEP 2
       IF j > bf - 1 AND j < ef + 1 AND ffw(j) = 2 AND (j <> nos OR lslf = 0) THEN
        IF xf = 0 THEN
         seq = seq + CHR$(2)
         noop = noop + 1
         xf = 1
        END IF
        'IF j = nos THEN gap = drt - pstx(nos) - iamlen ELSE gap = pstx(j + 1) - pstx(j) ' WARNING
        seq = seq + CHR$(4) + CHR$(spar(j).c) + CHR$(spar(j).h) + CHR$(spar(j).r) + CHR$(idnr(j))
        seq = seq + CHR$(spar(j).dt AND 1) + MKL$(spar(j).offset) + MKI$(lensec(idnr(j)))
        seq = seq + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(1) + CHR$(2) + MKI$(pstx(j) + 146) + CHR$(0)
        noop = noop + 1
       END IF
      NEXT j
      FOR j = bf TO ef
       IF ffw(j) = 2 AND j = nos AND lslf THEN
        seq = seq + CHR$(2)
        noop = noop + 1
        'IF j = nos THEN gap = drt - pstx(nos) - iamlen ELSE gap = pstx(j + 1) - pstx(j) ' WARNING
        seq = seq + CHR$(4) + CHR$(spar(j).c) + CHR$(spar(j).h) + CHR$(spar(j).r) + CHR$(idnr(j))
        seq = seq + CHR$(spar(j).dt AND 1) + MKL$(spar(j).offset) + MKI$(lensec(idnr(j)))
        seq = seq + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(1) + CHR$(0) + MKI$(pstx(j) + 146) + CHR$(0)
        noop = noop + 1
       END IF
       IF ffw(j) = 3 THEN
        IF idnr(j) > 7 THEN n = 7 ELSE n = idnr(j)
        IF j = nos THEN x1 = drt - pstx(nos) - iamlen + abtkid ELSE x1 = pstx(j + 1) - pstx(j) + abtkid
        x2 = gpltab(n) + abtcrc
        IF x1 < x2 THEN xf = 1 ELSE xf = 0
        IF j = nos AND xf THEN xf = 2
        seq = seq + CHR$(2)
        noop = noop + 1
        IF j = 1 THEN f = 1 ELSE f = 0
        seq = seq + CHR$(4) + CHR$(spar(j).c) + CHR$(spar(j).h) + CHR$(spar(j).r) + CHR$(idnr(j))
        seq = seq + CHR$(spar(j).dt AND 1) + MKL$(spar(j).offset) + MKI$(lensec(n))
        seq = seq + CHR$(255) + CHR$(255) + CHR$(27) + CHR$(1) + CHR$(f) + MKI$(pstx(j) + 146) + CHR$(1)
        SELECT CASE xf
         CASE 0: seq = seq + CHR$(f) + MKI$(pstx(j) + x2 + 146)
         CASE 1: seq = seq + CHR$(f) + MKI$(pstx(j) + x1 + 146)
         CASE 2: seq = seq + CHR$(f + 1) + MKI$(-iamlen + abtkid + 146)
        END SELECT
        noop = noop + 1
       END IF
       IF j = 1 AND fw = 3 THEN
        seq = seq + CHR$(2)
        noop = noop + 1
        seq = seq + CHR$(3) + CHR$(1) + CHR$(1) + CHR$(1) + "ö" + CHR$(1) + CHR$(1) + MKI$(179) + CHR$(spar(1).c) + CHR$(spar(1).h) + CHR$(spar(1).r) + CHR$(spar(1).n)
        noop = noop + 1
       END IF
      NEXT j
      RETURN
END FUNCTION

