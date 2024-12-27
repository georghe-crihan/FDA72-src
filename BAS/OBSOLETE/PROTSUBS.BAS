DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
FUNCTION keydisk (noc, BYVAL mode)

      'inderr codes:
      'A4DD - Could not determine recording density
      'A4DC - Could not determine number of cylinders
      'A5E3 - Sector 0,1,240,1 read command abnormal termination
      'A3DC - Sector 0,1,240,1 codeline checksum error
      'A3DF - Sector 0,1,240,1 codeline first bytes are not "FDA vX.X"
      'C7A1 - GAP3 or BSA after first sector analysis error on counter track
      'C7B1 - "Read sector" function for sector 146 abn. term. on cntr track
      'C7Dx - Data checking error on cntr track s146, x - error location code
      'C7B2 - "Read sector" function for sector 231 abn. term. on cntr track
      'C7F6 - Cannot find IDAM on counter track
      'C7A4 - Counter track length outside tolerance
      'C7Ex - Data checking error on cntr track s231, x - error location code
      'E3C1 - "Read sector" function for all sectors abnormal term. on aux.
      'E3F6 - Cannot find DATA AM on auxiliary track
      'E3A4 - Auxiliary track length outside tolerance
      'E3Ex - Data checking error on auxiliary track, x - error location code

      'Data checking error location codes:
      '1 - Sector data (except last sector)
      '2 - Data CRC (except last sector)
      '3 - GAP3 or BSA
      '4 - Sector data (last sector)
      '5 - Data CRC (last sector)
      '6 - BTA-1
      '7 - BTA-2

      IF mode = 0 THEN drive = 0
pcx1: q = dmabcre: IF q = 0 THEN inderr = &HFFFE: GOTO exx
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)
      inderr = 0

      IF drive THEN drvtyp = drvbtyp ELSE drvtyp = drvatyp
      SELECT CASE drvtyp
       CASE 1: determ = 0: ncyl = 42: dens = 1
       CASE 2: determ = 3: ncyl = 84
       CASE 3: determ = 0: ncyl = 82: dens = 1
       CASE 4: determ = 1: ncyl = 82
      END SELECT
      IF determ AND 1 THEN diskdens = 0 ELSE diskdens = power(dens)
      x = init(0)
      IF erflg GOTO fail
      IF determ AND 1 THEN
       GOSUB dddp
       IF x THEN inderr = &HA4DD: GOTO exx
       determ = determ AND 2
       IF dens <> 1 THEN determ = determ AND 1
      END IF
      IF determ AND 2 THEN
       GOSUB ncdp
       IF x THEN inderr = &HA4DC: GOTO exx
       determ = determ AND 1
      END IF
      IF ncyl < 60 THEN ncyl = 41 ELSE ncyl = 81

      cyl = 0: head = 1: hdd = drive + 4
      hdpos cyl: IF erflg GOTO fail
      x = ixfr(dens, 0)
      cmd(1) = &H46
      cmd(2) = hdd
      cmd(3) = 0
      cmd(4) = 1
      cmd(5) = 240
      cmd(6) = 1
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 0, 256
      dbtc 9: IF erflg GOTO fail
      waint
      IF erflg GOTO fail
      n = csfc: IF erflg GOTO fail
      IF cstat(0) AND &HC0 THEN inderr = &HA5E3: GOTO exx
      sks = STRING$(256, 0)
      dmabrd 0, sks, 256

      crypt sks, 256, &H9CF659F5
      sf = LEFT$(sks, 254)
      x = chksum2(sf, 254)
      IF x <> CVI(RIGHT$(sks, 2)) THEN inderr = &HA3DC: GOTO exx
      IF LEFT$(sf, 8) <> "FDA v7.1" THEN inderr = &HA3DF: GOTO exx
      noc = ASC(MID$(sf, 9, 1))
      REDIM lng(1)
      posit = 10
      FOR i = 0 TO 1
       lng(i) = CVI(MID$(sf, posit, 2))
       posit = posit + 2
      NEXT i
      IF noc THEN
       REDIM lni(1 TO noc)
       FOR i = 1 TO noc
        lni(i) = CVI(MID$(sf, posit, 2))
        posit = posit + 2
       NEXT i
      END IF

      SELECT CASE dens
       CASE 1: nos = 9: gplstd = 80: nhsec = 5
       CASE 3: nos = 15: gplstd = 84: nhsec = 9
       CASE 4: nos = 18: gplstd = 108: nhsec = 10
      END SELECT

      IF noc = 0 GOTO nmi
      SELECT CASE dens
       CASE 1: gpl = 19
       CASE 3: gpl = 28
       CASE 4: gpl = 30
      END SELECT
      cyl = (noc + 1) \ 2: head = noc + 1 AND 1
      hdd = drive: IF head THEN hdd = hdd + 4
      hdpos cyl: IF erflg GOTO fail
      x = ixfr(dens, 0)
      cmd(1) = &H42
      cmd(2) = hdd
      cmd(6) = 3
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 0, 1024
      dbtc 9: IF erflg GOTO fail
      waint
      IF erflg GOTO fail
      n = csfc: IF erflg GOTO fail
      ln = (gpltab(0) + gpl) * 3 - gpltab(2) + 2
      a = fdaagap(dmabseg, e241(i2l(dmaboff) + 512), 512, ln - 9, ln + 9)
      IF a <> gpl + 256 THEN inderr = &HC7A1: GOTO exx
      cmd(1) = &H46
      cmd(2) = hdd
      cmd(3) = cyl
      cmd(4) = head
      cmd(5) = 146
      cmd(6) = 1
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 0, 256
      dbtc 9: IF erflg GOTO fail
      waint
      IF erflg GOTO fail
      n = csfc: IF erflg GOTO fail
      IF cstat(1) AND 5 THEN inderr = &HC7B1: GOTO exx
      s = STRING$(256, "ö"): dmabwr 256, s, 256
      a = fdacclr(dmabseg, dmaboff, 256, gpl)
      IF a THEN inderr = &HC7D0 + a: GOTO exx
      cmd(1) = &H46
      cmd(2) = hdd
      cmd(3) = cyl
      cmd(4) = head
      cmd(5) = 231
      cmd(6) = 2
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 0, 512
      dbtc 9: IF erflg GOTO fail
      waint
      IF erflg GOTO fail
      n = csfc: IF erflg GOTO fail
      IF cstat(1) AND 5 THEN inderr = &HC7B2: GOTO exx
      a = fdaamf(dmabseg, e241(i2l(dmaboff) + 131), 512, 2)
      IF a = -1 THEN inderr = &HC7F6: GOTO exx
      IF ABS(a - lni(noc)) > 1 THEN inderr = &HC7A4: GOTO exx
      a = fdacsec(dmabseg, dmaboff, 512)
      IF a THEN inderr = &HC7E0 + a: GOTO exx

nmi:  cyl = ncyl - 1
      hdpos cyl: IF erflg GOTO fail
      FOR head = 0 TO 1
       hdd = drive: IF head THEN hdd = hdd + 4
       x = ixfr(dens, 0)
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 1
       cmd(6) = 2
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H42, 0, 512 * nos
       dbtc 9: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       IF cstat(0) AND &HC0 THEN inderr = &HE3C1: GOTO exx
       cmd(1) = &H42
       cmd(2) = hdd
       cmd(6) = maxlen
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, lensec(maxlen)
       dbtc 9: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       a = fdaamf(dmabseg, e241(i2l(dmaboff) + drttbl(dens) - 300), 512, 3)
       IF a = -1 THEN inderr = &HE3F6: GOTO exx
       IF ABS(a - lng(head)) > 2 THEN inderr = &HE3A4: GOTO exx
       a = fdactrk(dmabseg, dmaboff, lensec(maxlen), gplstd, nos)
       IF a THEN inderr = &HE3E0 + a: GOTO exx
      NEXT head
      protbug1 = &H2155: protbug2 = &H2833

      IF mode THEN
       IF noc = 0 THEN inderr = 1: GOTO exx
       cyl = (noc + 1) \ 2: head = noc + 1 AND 1
       hdd = drive: IF head THEN hdd = hdd + 4
       hdpos cyl: IF erflg GOTO fail
       x = ixfr(dens, 0)
       cmd(1) = &H45
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 146
       cmd(6) = 1
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H4A, 0, 256
       dbtc 9: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       IF cstat(1) AND 2 THEN inderr = 2: GOTO exx
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 231
       cmd(6) = 2
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H42, 0, 16
       dbtc 9: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       IF (cstat(1) AND 4) = 0 THEN inderr = 2: GOTO exx
       noc = noc - 1
       sf = STRING$(254, 0)
       s = "FDA v7.1" + CHR$(noc)
       FOR i = 0 TO 1
        s = s + MKI$(lng(i))
       NEXT i
       FOR i = 1 TO noc
        s = s + MKI$(lni(i))
       NEXT i
       MID$(sf, 1) = s
       x = chksum2(sf, 254)
       sks = sf + MKI$(x)
       crypt sks, 256, &H9CF659F5
       cyl = 0: head = 1: hdd = drive + 4
       hdpos cyl: IF erflg GOTO fail
       x = ixfr(dens, 0)
       dmabwr 0, sks, 256
       cmd(1) = &H45
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 240
       cmd(6) = 1
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H4A, 0, 256
       dbtc 9: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       IF cstat(1) AND 2 THEN inderr = 2: GOTO exx
       cmd(1) = &H46
       cmd(2) = hdd
       cmd(3) = cyl
       cmd(4) = head
       cmd(5) = 240
       cmd(6) = 1
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, 256
       dbtc 9: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       s = STRING$(256, 0)
       dmabrd 0, s, 256
       IF s <> sks THEN inderr = 2: GOTO exx
       protbug3 = &H4456
      END IF
      GOTO exx
fail: inderr = &HFFFF
      SELECT CASE erflg
       CASE 1: bioserr = 128
       CASE 2: bioserr = 32
       CASE 3: bioserr = 64
      END SELECT
      erflg = 0
exx:  IF boflg THEN dmabdel: boflg = 0
      stopmot
      IF ((inderr AND &HF000) = &HA000 OR inderr = &HFFFF) AND drive = 0 AND mode = 0 THEN drive = 1: GOTO pcx1
      keydisk = inderr
      EXIT FUNCTION
dddp: cyl = 0: head = 0: hdd = drive
      hdpos cyl: IF erflg GOTO fail
      IF drvtyp = 2 THEN a = 3 ELSE a = 4
      xf = 1
      FOR i = 1 TO 2
       IF i = 1 THEN dens = 1 ELSE dens = a
       x = ixfr(dens, 0)
       cmd(1) = &H4A
       cmd(2) = hdd
       dbtc 2: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       IF (cstat(1) AND 1) = 0 THEN xf = 0: diskdens = power(dens): EXIT FOR
      NEXT i
      x = xf: RETURN
ncdp: cyl = 2: head = 0: hdd = drive
      hdpos cyl: IF erflg GOTO fail
      x = ixfr(dens, 0)
      x = 1
      FOR i = 1 TO mns
       cmd(1) = &H4A
       cmd(2) = hdd
       dbtc 2: IF erflg GOTO fail
       waint
       IF erflg GOTO fail
       n = csfc: IF erflg GOTO fail
       IF cstat(0) AND &HC0 THEN EXIT FOR
       c = cstat(3)
       IF c = 2 THEN x = 0: EXIT FOR
       IF c = 1 THEN ncyl = ncyl \ 2: nods = nods * 2: x = 0: EXIT FOR
      NEXT i
      RETURN
END FUNCTION
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
FUNCTION micheck (smachins)
      s = smachins
      crypt s, 28, &H25662828
      sk = RIGHT$(s, 4): s = LEFT$(s, 24)
      qk = CVL(sk)
      crypt s, 24, qk
      sf = LEFT$(s, 22)
      x = chksum2(sf, 22)
      IF x <> CVI(RIGHT$(s, 2)) THEN inderr = 1: GOTO exy
      IF LEFT$(sf, 8) <> "FDA v7.1" THEN inderr = 2: GOTO exy
      ptype = CVI(MID$(sf, 9, 2))
      qmsiz = CVL(MID$(sf, 11, 4))
      qbver = CVL(MID$(sf, 15, 4))
      qhtbl = CVL(MID$(sf, 19, 4))
      x = cputype
      x1 = e121(x): x2 = e122(x)
      IF x2 = 2 THEN x2 = 1
      x = x2 * 256 + x1
      IF x <> ptype THEN inderr = 3: GOTO exy
      IF qmsiz <> -1 THEN
       q = memsize
       IF q <> -1 THEN
        IF q <> qmsiz THEN inderr = 4: GOTO exy
       END IF
      END IF
      IF biosver <> qbver THEN inderr = 5: GOTO exy
      IF hardtbl <> qhtbl THEN inderr = 6: GOTO exy
      protbug1 = &H2155: protbug2 = &H2833
exy:  micheck = inderr
END FUNCTION
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
FUNCTION sminst
      ptype = cputype
      qmsiz = memsize
      qbver = biosver
      qhtbl = hardtbl
      x1 = e121(ptype): x2 = e122(ptype)
      IF x2 = 2 THEN x2 = 1
      ptype = x2 * 256 + x1
      s = "FDA v7.1" + MKI$(ptype) + MKL$(qmsiz) + MKL$(qbver) + MKL$(qhtbl)
      x = chksum2(s, 22)
      s = s + MKI$(x)
      sk = ""
      FOR i = 0 TO 3
       sk = sk + CHR$(PEEK(&H6C + i))
      NEXT i
      qk = CVL(sk)
      crypt s, 24, qk
      s = s + sk
      IF protbug3 <> &H4456 THEN s = STRING$(28, 57)
      crypt s, 28, &H25662828
      sminst = s
END FUNCTION
