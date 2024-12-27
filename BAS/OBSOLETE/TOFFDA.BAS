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
DECLARE FUNCTION absread% (BYVAL drv%, BYVAL sec&, BYVAL nos%, buf$)
DECLARE FUNCTION fdardid% (BYVAL ctimer%, BYVAL i%, cmd%, xt%, cstat%)
DECLARE FUNCTION fdaamf% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL bnum%)
DECLARE FUNCTION csfc% ()
DECLARE FUNCTION init% (BYVAL mode%)
DECLARE FUNCTION ixfr% (BYVAL dens%, BYVAL m%)
DECLARE FUNCTION power% (BYVAL num%)
DECLARE FUNCTION cxfr% (BYVAL dens%)
DECLARE FUNCTION drttbl% (dens%)
DECLARE FUNCTION qtrln& (boff%, dens%)
DECLARE FUNCTION getbioserr% ()
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
CONST dsd0 = 13, dsd1 = 11
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
      stemp = ENVIRON$("TEMP"): IF stemp = "" THEN stemp = "C:\" ELSE IF RIGHT$(stemp, 1) <> "\" THEN stemp = stemp + "\"
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

      drive = ASC(RIGHT$(COMMAND$, 3)) - 65
      dt = ASC(RIGHT$(COMMAND$, 2)) - 48
      noc = ASC(RIGHT$(COMMAND$, 1)) - 48

      enerr = 1

      SELECT CASE dt
       CASE 0: ncyl = 41: dens = 1
       CASE 1: ncyl = 81: dens = 3
       CASE 2: ncyl = 81: dens = 1
       CASE 3: ncyl = 81: dens = 4
      END SELECT
      SELECT CASE dens
       CASE 1: nos = 9: gplstd = 80: nhsec = 5
       CASE 3: nos = 15: gplstd = 84: nhsec = 9
       CASE 4: nos = 18: gplstd = 108: nhsec = 10
      END SELECT

      diskdens = power(dens)
      x = init(1)
      IF erflg GOTO er0
      IF x THEN END 1
      q = dmabcre: IF q = 0 THEN ERROR 7
      dmabseg = e243(q): dmaboff = e241(q): boflg = 1

      POKE &H40, 0
      cyl = ncyl - 1: head = 0: hdd = drive
      hdpos cyl: IF erflg GOTO er0
      REDIM lng(1)
      FOR head = 0 TO 1
       POKE &H40, 0
       hdd = drive: IF head THEN hdd = hdd + 4
       x = ixfr(dens, 2)
       s = ""
       FOR i = 1 TO nos
        s = s + CHR$(cyl) + CHR$(head) + CHR$(i) + CHR$(2)
       NEXT i
       dmabwr 0, s, nos * 4
       cmd(1) = &H4D
       cmd(2) = hdd
       cmd(3) = 2
       cmd(4) = nos
       cmd(5) = gplstd
       cmd(6) = ASC("ö")
       dmach2 &H4A, 0, nos * 4
       dbtc 6: IF erflg GOTO er0
       waint
       IF erflg GOTO er0
       n = csfc: IF erflg GOTO er0
       POKE &H40, 0
       cmd(1) = &H42
       cmd(2) = hdd
       cmd(6) = maxlen
       cmd(7) = 255
       cmd(8) = &H1B
       cmd(9) = &HFF
       dmach2 &H46, 0, lensec(maxlen)
       dbtc 9: IF erflg GOTO er0
       waint
       IF erflg GOTO er0
       n = csfc: IF erflg GOTO er0
       a = fdaamf(dmabseg, e241(i2l(dmaboff) + drttbl(dens) - 300), 512, 3)
       IF a = -1 GOTO cfam
       lng(head) = a
      NEXT head

      POKE &H40, 0
      IF noc = 0 GOTO nmi
      REDIM lni(1 TO noc)
      SELECT CASE dens
       CASE 1: gpl = 19
       CASE 3: gpl = 28
       CASE 4: gpl = 30
      END SELECT
      x = noc \ 2: IF noc AND 1 THEN x = x + 1
      FOR cyl = x TO 1 STEP -1
       POKE &H40, 0
       hdpos cyl: IF erflg GOTO er0
       y = 1
       IF cyl = x AND noc AND 1 THEN y = 0
       FOR head = y TO 0 STEP -1
        POKE &H40, 0
        hdd = drive: IF head THEN hdd = hdd + 4
        x = ixfr(dens, 2)
        cmd(1) = &H46
        cmd(2) = hdd
        cmd(3) = cyl
        cmd(4) = head
        cmd(5) = 1
        cmd(6) = 2
        cmd(7) = 255
        cmd(8) = &H1B
        cmd(9) = &HFF
        dmach2 &H46, 1024, nos * 512
        dbtc 9: IF erflg GOTO er0
        waint
        IF erflg GOTO er0
        n = csfc: IF erflg GOTO er0
        POKE &H40, 0
        s = ""
        FOR i = 1 TO nos
         s = s + CHR$(cyl) + CHR$(head) + CHR$(i) + CHR$(2)
         s = s + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
         s = s + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
        NEXT i
        s = s + CHR$(cyl) + CHR$(head) + CHR$(146) + CHR$(1)
        s = s + CHR$(cyl) + CHR$(head) + CHR$(231) + CHR$(2)
        dmabwr 0, s, nos * 12 + 8
        cmd(1) = &H4D
        cmd(2) = hdd
        cmd(3) = 0
        cmd(4) = nos * 3 + 2
        cmd(5) = gpl
        cmd(6) = ASC("ö")
        dmach2 &H4A, 0, nos * 12 + 8
        dbtc 6: IF erflg GOTO er0
        waint
        IF erflg GOTO er0
        n = csfc: IF erflg GOTO er0
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
        dmach2 &H4A, 1024, nos * 512
        dbtc 9: IF erflg GOTO er0
        waint
        IF erflg GOTO er0
        n = csfc: IF erflg GOTO er0
        POKE &H40, 0
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
        dbtc 9: IF erflg GOTO er0
        waint
        IF erflg GOTO er0
        n = csfc: IF erflg GOTO er0
        a = fdaamf(dmabseg, e241(i2l(dmaboff) + 131), 512, 2)
        IF a = -1 GOTO cfam
        lni(cyl * 2 + head - 1) = a
      NEXT head, cyl

nmi:  POKE &H40, 0
      sf = STRING$(254, 0)
      s = "FDA v7.1" + CHR$(noc)
      FOR i = 0 TO 1
       s = s + MKI$(lng(i))
      NEXT i
      ERASE lng
      IF noc = 0 GOTO nmi2
      FOR i = 1 TO noc
       s = s + MKI$(lni(i))
      NEXT i
      ERASE lni
nmi2: MID$(sf, 1) = s
      x = chksum2(sf, 254)
      sks = sf + MKI$(x)
      crypt sks, 256, &H9CF659F5

      POKE &H40, 0
      SELECT CASE dens
       CASE 1: gpl = 3
       CASE 3: gpl = 13
       CASE 4: gpl = 16
      END SELECT
      cyl = 0: head = 1: hdd = drive + 4
      hdpos cyl: IF erflg GOTO er0
      x = ixfr(dens, 2)
      cmd(1) = &H46
      cmd(2) = hdd
      cmd(3) = cyl
      cmd(4) = head
      cmd(5) = 1
      cmd(6) = 2
      cmd(7) = 255
      cmd(8) = &H1B
      cmd(9) = &HFF
      dmach2 &H46, 1024, nos * 512
      dbtc 9: IF erflg GOTO er0
      waint
      IF erflg GOTO er0
      n = csfc: IF erflg GOTO er0
      POKE &H40, 0
      s = ""
      FOR i = 1 TO nos
       s = s + CHR$(cyl) + CHR$(head) + CHR$(i) + CHR$(2)
       s = s + CHR$(cyl) + CHR$(head) + CHR$(0) + CHR$(0)
      NEXT i
      s = s + CHR$(cyl) + CHR$(head) + CHR$(240) + CHR$(1)
      dmabwr 0, s, nos * 8 + 4
      cmd(1) = &H4D
      cmd(2) = hdd
      cmd(3) = 1
      cmd(4) = nos * 2 + 1
      cmd(5) = gpl
      cmd(6) = ASC("ö")
      dmach2 &H4A, 0, nos * 8 + 4
      dbtc 6: IF erflg GOTO er0
      waint
      IF erflg GOTO er0
      n = csfc: IF erflg GOTO er0
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
      dmach2 &H4A, 1024, nos * 512
      dbtc 9: IF erflg GOTO er0
      waint
      IF erflg GOTO er0
      n = csfc: IF erflg GOTO er0
      POKE &H40, 0
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
      dbtc 9: IF erflg GOTO er0
      waint
      IF erflg GOTO er0
      n = csfc: IF erflg GOTO er0
      
      dmabdel
      boflg = 0
      stopmot
      END 0

cfam: dosprin "Cannot find Address Mark" + scrlf: dmabdel: boflg = 0: stopmot: END 1

er0:  SELECT CASE erflg
       CASE 1: xderr = 200: bioserr = 128
       CASE 2: xderr = 201: bioserr = 32
       CASE 3: xderr = 202: bioserr = 64
       CASE 4: xderr = 203
       CASE 5: xderr = 204
      END SELECT
      erflg = 0: ERROR xderr
      xe = 0: END 1

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
        dosprin "Fatal Error" + STR$(ERR) + scrlf: END 1
        'ERROR ERR
      END SELECT
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

