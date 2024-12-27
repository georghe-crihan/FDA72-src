TYPE regtypex
ax AS INTEGER
bx AS INTEGER
cx AS INTEGER
dx AS INTEGER
bp AS INTEGER
si AS INTEGER
di AS INTEGER
flags AS INTEGER
ds AS INTEGER
es AS INTEGER
END TYPE
DECLARE SUB interruptx (intnum%, inregs AS regtypex, outregs AS regtypex)
DECLARE SUB crypt (buf$, BYVAL cntr%, BYVAL ckey&)
DECLARE SUB halt ()
DECLARE SUB dosprin (s$)
DECLARE SUB dmabwr (BYVAL offset%, buf$, BYVAL cntr%)
DECLARE SUB dmach2 (BYVAL dir%, BYVAL offset%, BYVAL cntr%)
DECLARE SUB dmabrd (BYVAL offset%, buf$, BYVAL cntr%)
DECLARE SUB dmabdel ()
DECLARE SUB swinfdcproc CDECL (BYVAL procnum%, xax%, xbx%, xcx%, xdx%, xfl%)
DECLARE SUB fdahook ()
DECLARE SUB fdaunh ()
DECLARE SUB dbtc (BYVAL n%)
DECLARE SUB waint ()
DECLARE SUB stopmot ()
DECLARE SUB hdpos (BYVAL cyl%)
DECLARE SUB abt ()
DECLARE FUNCTION i2l& (BYVAL i%)
DECLARE FUNCTION e121% (BYVAL i%)
DECLARE FUNCTION e122% (BYVAL i%)
DECLARE FUNCTION e141% (BYVAL l&)
DECLARE FUNCTION e142% (BYVAL l&)
DECLARE FUNCTION e143% (BYVAL l&)
DECLARE FUNCTION e144% (BYVAL l&)
DECLARE FUNCTION e241% (BYVAL l&)
DECLARE FUNCTION e243% (BYVAL l&)
DECLARE FUNCTION chksum2% (buf$, BYVAL num%)
DECLARE FUNCTION findif% (buf$, BYVAL num%)
DECLARE FUNCTION runspec$ (s$)
DECLARE FUNCTION dmabcre& ()
DECLARE FUNCTION dmachk& ()
DECLARE FUNCTION bioskey% ()
DECLARE FUNCTION absread% (BYVAL drv%, BYVAL sec&, BYVAL nos%, buf$)
DECLARE FUNCTION fdardid% (BYVAL ctimer%, BYVAL i%, cmd%, xt%, cstat%)
DECLARE FUNCTION fdaagap% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL minlen%, BYVAL maxlen%)
DECLARE FUNCTION fdaamf% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL bnum%)
DECLARE FUNCTION fdacclr% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL gpl%)
DECLARE FUNCTION fdacsec% (BYVAL bseg%, BYVAL boff%, BYVAL blen%)
DECLARE FUNCTION fdactrk% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL gpl%, BYVAL nos%)
DECLARE FUNCTION csfc% ()
DECLARE FUNCTION init% (BYVAL mode%)
DECLARE FUNCTION ixfr% (BYVAL dens%, BYVAL m%)
DECLARE FUNCTION power% (BYVAL num%)
DECLARE FUNCTION cxfr% (BYVAL dens%)
DECLARE FUNCTION drttbl% (dens%)
DECLARE FUNCTION qtrln& (boff%, dens%)
DECLARE FUNCTION getbioserr% ()
DECLARE FUNCTION keydisk% (noc%)
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
CONST dsd0 = 13, dsd1 = 11
CONST mns = 32
DIM SHARED seq
DIM SHARED cyl, head
DIM SHARED drvatyp, drvbtyp
DIM SHARED gpltab(7), lensec(7)
DIM SHARED hvflg, foflg, boflg, enerr, erflg, bioserr, dskchlin
DIM SHARED hdd, drive
DIM SHARED regs AS regtypex, sid AS STRING * 160
DIM SHARED nods, diskdens, drt, qtlen, qdrt, drvtyp
DIM SHARED fwd, dop, maxlen
DIM SHARED ncyl
DIM SHARED cmd(1 TO 9), cstat(10)
DIM SHARED dgroup, bsid, dmabseg, dmaboff, spath

      DEF SEG = &H40
      ON ERROR GOTO er
      q = SETMEM(-65600)
      dgroup = VARSEG(sid): bsid = VARPTR(sid)
      bioserr = 255
      scrlf = CHR$(13) + CHR$(10)

      FOR i = 0 TO 7
       x = power(i) * 128
       lensec(i) = x
       gpltab(i) = x + 62
      NEXT i

      regs.ax = &H800
      regs.bx = 0
      regs.dx = 0
      interruptx &H13, regs, regs
      drvatyp = e121(regs.bx)
      regs.ax = &H800
      regs.bx = 0
      regs.dx = 1
      interruptx &H13, regs, regs
      drvbtyp = e121(regs.bx)

      drive = ASC(RIGHT$(COMMAND$, 1)) - 65

      enerr = 1

      x = keydisk(noc)
      IF x THEN
       dosprin "Error " + HEX$(x) + scrlf
       SOUND 160, 1
       dosprin "Press any key to continue" + scrlf
       a = bioskey
       END 1
      ELSE
       dosprin "Number of machine-installations left:" + STR$(noc) + scrlf
       END 0
      END IF

er:   CLOSE
      IF foflg THEN
       regs.ax = &H3E00
       regs.bx = filnum
       interruptx &H21, regs, regs
       foflg = 0
      END IF
      IF boflg THEN dmabdel: boflg = 0
      stopmot
      SELECT CASE ERR
       CASE 24, 25, 27, 57, 68, 70, 71, 72
        serr = "Drive I/O error": GOTO opcan
       CASE 53, 64, 75, 76
        serr = "File/path not found": GOTO opcan
       CASE 54
        serr = "Access denied": GOTO opcan
       CASE 61, 67
        serr = "Disk full": GOTO opcan
       CASE 62
        serr = "Unexpected end-of-file": GOTO opcan
       CASE 7, 14
        serr = "Out of memory": GOTO opcan
       CASE ELSE
        dosprin "Fatal Error" + STR$(ERR) + scrlf: END 1
        'ERROR ERR
      END SELECT
opcan:
      IF enerr THEN dosprin serr + scrlf
      xe = 1: RESUME NEXT
END

SUB abt
      swinfdcproc 22, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 GOTO er1
      x = dsd0
      xdx = x * 16 + 527
      swinfdcproc 18, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 GOTO er1
      EXIT SUB

er1:  IF xfl AND 128 THEN
       erflg = 3
      ELSE
       IF xfl AND 64 THEN erflg = 1 ELSE erflg = 2
      END IF
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

FUNCTION drttbl (dens)
      SELECT CASE dens
       CASE 0: drttbl = 5208
       CASE 1: drttbl = 6250
       CASE 2: drttbl = 7500
       CASE 3: drttbl = 10417
       CASE 4: drttbl = 12500
      END SELECT
END FUNCTION

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
opc:  dosprin "Number of cylinders and/or recording density not supported by drive" + scrlf: init = 1
END FUNCTION

FUNCTION ixfr (BYVAL dens, BYVAL m)
SHARED initfirst
      dr = cxfr(dens): IF dr = -1 GOTO opc2
      drt = drttbl(dens)
      IF drt < gpltab(6) THEN maxlen = 6 ELSE maxlen = 7
      xdx = dr * 256 + 255
      swinfdcproc 10, xax, xbx, xcx, xdx, xfl
      ixfr = 0: EXIT FUNCTION
opc2: ixfr = 1
END FUNCTION

FUNCTION keydisk (noc)
      q = dmabcre: IF q = 0 THEN inderr = &HFFFE: GOTO exx
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

FUNCTION qtrln (boff, dens)
      x = drttbl(dens) - 300
      a = fdaamf(dmabseg, e241(i2l(dmaboff) + boff + x), 512, 3)
      IF a = -1 THEN qtrln = -1: EXIT FUNCTION
      y = x + 516
      qtrln = (y * 8& - a) * 32&
END FUNCTION

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

SUB waint
      swinfdcproc 12, xax, xbx, xcx, xdx, xfl
      IF xfl AND 1 THEN erflg = 1
END SUB

