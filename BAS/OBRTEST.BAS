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
TYPE secint         '---------- Parameters of sector ----------
c AS INTEGER          '# of cyl from ID
h AS INTEGER          '# of head from ID
r AS INTEGER          '# of sector from ID
n AS INTEGER          'Length of sector from ID
gpl AS INTEGER        'Sector-to-Sector distance
post AS INTEGER       'Index-to-Sector distance
csn AS INTEGER        'CRC known flag
crc AS INTEGER        'CRC Ok flag
dt AS INTEGER         'Data type: 0 - Normal, 1 - Deleted, 2 - No data marker
offset AS LONG        'Offset in data file, -1 - None
datlen AS INTEGER     'Length in data file
dif AS INTEGER        'Where differences begin
sfb AS INTEGER        'Sector first bytes
agr AS INTEGER        'GAP3 analysis result
END TYPE
TYPE secpar         '---------- Parameters of sector ----------
c AS STRING * 1       '# of cyl from ID
h AS STRING * 1       '# of head from ID
r AS STRING * 1       '# of sector from ID
n AS STRING * 1       'Length of sector from ID
gpl AS INTEGER        'Sector-to-Sector distance
post AS INTEGER       'Index-to-Sector distance
csn AS STRING * 1     'CRC known flag
crc AS STRING * 1     'CRC Ok flag
dt AS STRING * 1      'Data type: 0 - Normal, 1 - Deleted, 2 - No data marker
offset AS LONG        'Offset in data file, -1 - None
datlen AS INTEGER     'Length in data file
dif AS INTEGER        'Where differences begin
sfb AS INTEGER        'Sector first bytes
agr AS INTEGER        'GAP3 analysis result
END TYPE
TYPE trkpar         '---------- Parameters of track -----------
tdt AS STRING * 1     'Track type: 0 - Standard, 1 - Non-standard
trd AS STRING * 1     'Track recording density
nos AS STRING * 1     'Number of sectors on track
sln AS STRING * 1     'Sector length
gpl AS STRING * 1     'Average GPL
dop AS STRING * 1
abtkid AS STRING * 1
abtcrc AS STRING * 1
bdmpos AS STRING * 1
spar(1 TO 32) AS secpar
END TYPE
TYPE winpar         '---------- Parameters of window ----------
br AS INTEGER          'Starting row
bc AS INTEGER          'Starting column
nr AS INTEGER          'Number of rows
nc AS INTEGER          'Number of columns
tpos AS INTEGER        'Starting column of the title
tit AS STRING * 32     'Title
wintyp AS INTEGER      'Window type: 0 - Primary, 1 - Secondary, 2 - Alert, 3 - Help
END TYPE
TYPE dfdrec         '---------- Parameters of DOS format ------
noc AS INTEGER         'Number of cylinders
nos AS INTEGER         'Number of sectors per track
mdb AS INTEGER         'Media descriptor
noe AS INTEGER         'Number of root directory entries
spc AS INTEGER         'Sectors per cluster
END TYPE
DECLARE SUB dtws ()
DECLARE FUNCTION i2l& (BYVAL i%)
DECLARE FUNCTION e121% (BYVAL i%)
DECLARE FUNCTION e122% (BYVAL i%)
DECLARE FUNCTION e141% (BYVAL l&)
DECLARE FUNCTION e142% (BYVAL l&)
DECLARE FUNCTION e143% (BYVAL l&)
DECLARE FUNCTION e144% (BYVAL l&)
DECLARE FUNCTION e241% (BYVAL l&)
DECLARE FUNCTION e243% (BYVAL l&)
DECLARE FUNCTION obr% ()
DECLARE FUNCTION power% (BYVAL num%)
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
CONST mns = 32
CONST protbug2 = &H2833
DIM SHARED lensec(7), gpltab(7), drttbl(4)
DIM SHARED spar(34) AS secint
DIM SHARED cyl, head, dot, dtf, nos, agpl
DIM SHARED seq
DIM SHARED dop, abtkid, abtcrc, bdmpos, dopl
DIM SHARED mrf, chid, lssf, iamlen

     'длины зоны данных секторов в байтах
DATA 128,256,512,1024,2048,4096,8192,16384
READ lensec(0), lensec(1), lensec(2), lensec(3), lensec(4), lensec(5), lensec(6), lensec(7)
     'полные длины секторов в байтах
DATA 190,318,574,1086,2110,4158,8254,16446
READ gpltab(0), gpltab(1), gpltab(2), gpltab(3), gpltab(4), gpltab(5), gpltab(6), gpltab(7)
     'времена оборота для плотностей: Low, Double, Medium, High, Quad
DATA 5208,6250,7500,10417,12500
READ drttbl(0), drttbl(1), drttbl(2), drttbl(3), drttbl(4)

      mrf = 0
      chid = 2
      lssf = 1
      iamlen = 66

      dtf = 1
      dot = 1
      dopl = 24
      nos = 10
      dop = 6
      abtkid = -2
      abtcrc = -2
      bdmpos = 53
      FOR i = 1 TO nos
       spar(i).r = i
      NEXT i
      'Lengths of sectors from IDs
      DATA 2,2,2,2,2,2,2,2,2,2
      FOR i = 1 TO nos
       READ spar(i).n
      NEXT i
      'Sector-to-Sector distances
      DATA 597,596,596,598,597,597,598,599,598
      xsum = 146
      FOR i = 1 TO nos
       spar(i).post = xsum
       IF i <> nos THEN
        READ x
        spar(i).gpl = x
        xsum = xsum + x
       END IF
      NEXT i
      drt = drttbl(dot)
      spar(nos).gpl = drt - xsum
      'CRC known flags
      DATA 15,7,7,7,7,7,7,7,7,7
      FOR i = 1 TO nos
       READ spar(i).csn
      NEXT i
      'CRC Ok flags
      DATA 4,4,4,4,4,4,4,4,4,0
      FOR i = 1 TO nos
       READ spar(i).crc
      NEXT i
      'Data types
      DATA 0,0,0,0,0,0,0,0,0,0
      FOR i = 1 TO nos
       READ spar(i).dt
      NEXT i
      'Lengths in data file
      DATA 1024,512,512,512,512,512,512,512,512,512
      FOR i = 1 TO nos
       READ spar(i).datlen
      NEXT i
      'Where differences begin
      DATA 512,512,512,512,512,512,512,512,512,1
      FOR i = 1 TO nos
       READ spar(i).dif
      NEXT i
      'Sector first bytes
      DATA &HF6F6,&HF6F6,&HF6F6,&HF6F6,&HF6F6,&HF6F6,&HF6F6,&HF6F6,&HF6F6,511
      FOR i = 1 TO nos
       READ spar(i).sfb
      NEXT i
      'Gap analysis: Status
      DATA 3,0,0,0,0,0,0,0,0,0
      'Gap analysis: Length
      DATA 2,0,0,0,0,0,0,0,0,0
      'Gap analysis: GPL
      DATA 25,0,0,0,0,0,0,0,0,0
      FOR i = 1 TO nos
       READ x
       spar(i).agr = spar(i).agr + x * 256
      NEXT i
      FOR i = 1 TO nos
       READ x
       spar(i).agr = spar(i).agr + x * 4096
      NEXT i
      FOR i = 1 TO nos
       READ x
       spar(i).agr = spar(i).agr + x
      NEXT i

      x = obr
      CLS
      IF x THEN
       PRINT "Could not build write sequence"
      ELSE
       OPEN "E:\OBRTEST.SEQ" FOR OUTPUT AS #1
       dtws
       CLOSE
       SHELL "F:\WPVIEW E:\OBRTEST.SEQ"
      END IF

END

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
        s = s + " " + HEX$(qoff) + "H" + STR$(dmal) + STR$(dtl) + STR$(cmd7) + STR$(cmd8)
        v = STR$(cp): IF LEFT$(v, 1) <> " " THEN v = " " + v
        IF pflg THEN s = s + " P" + STR$(xp) + v
        v = STR$(ca): IF LEFT$(v, 1) <> " " THEN v = " " + v
        IF aflg THEN s = s + " A" + STR$(xa) + v
        PRINT #1, s
       END SELECT
      NEXT i
      PRINT #1, "END"
END SUB

FUNCTION obr
      drt = drttbl(dot)
      IF dot < 3 THEN maxlen = 6 ELSE maxlen = 7
      IF dtf GOTO obpt

      seq = MKL$(spar(1).offset) + CHR$(3) + CHR$(1) + CHR$(dot) + CHR$(3) + CHR$(nos) + CHR$(alen) + CHR$(agpl) + "Ў" + CHR$(0)
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
       x = gpltab(spar(nos).n)
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
      lend = 1: ql = -1: fixql = 1: gplf = 1
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
         xf = 0: yf = 0: y = 0: f = 0
         FOR l = i TO nos
          yf = yf + y
          IF f THEN EXIT FOR
          a = pst(l) / xk
          IF a >= 255 THEN EXIT FOR
          x = ABS(a * xk - pst(l))
          IF x > pstt(l) THEN EXIT FOR
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
        fb = ASC("Ў"): fixfb = 0
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
                nc = n
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
                nc = n
                FOR nc = n + 1 TO maxlen
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
              IF (lend OR fsf) AND mg(j, k) < mwa THEN
               x = x + noptfor
              ELSE
               IF l > 3 AND mg(j, k) < mwa AND (ls <> lsf OR gpl <> gplf) THEN x = x + noptfor
              END IF
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
       IF fw < 2 AND spar(i).dt <> 2 THEN
        nfb = e121(spar(i).sfb)
        IF fixfb = 0 THEN fixfb = 1: fb = nfb
       ELSE
        nfb = fb
       END IF
       IF lend OR fsflg OR nfb <> fb THEN
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
        IF fw = 1 THEN ln = ln - dop
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

      IF protbug2 <> &H2833 THEN IF fw = 2 THEN ff = ff + 1 ELSE fw = 2

      seq = seq + CHR$(1) + CHR$(dot): noop = noop + 1
      FOR i = nof TO ff STEP -1
       IF cf(i) OR i = nof AND (ct(i) OR lfabt) THEN
        IF cf(i) THEN seq = seq + CHR$(2): noop = noop + 1
        seq = seq + CHR$(3) + CHR$(1) + CHR$(1) + CHR$(1) + "Ў"
        IF cf(i) THEN seq = seq + CHR$(1) + CHR$(1) + MKI$(cf(i) + 146) ELSE seq = seq + CHR$(0)
        seq = seq + CHR$(0) + CHR$(0) + CHR$(idrf) + CHR$(0)
        noop = noop + 1
       END IF
       IF ct(i) OR lfabt THEN seq = seq + CHR$(2): noop = noop + 1
       seq = seq + CHR$(3) + CHR$(xn(i)) + CHR$(xl(i)) + CHR$(xg(i)) + CHR$(xb(i))
       IF lfabt THEN
        seq = seq + CHR$(1) + CHR$(2) + MKI$(-iamlen + abtkid + 146): lfabt = 0
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
        IF j = nos THEN gap = drt - pstx(nos) - iamlen ELSE gap = pstx(j + 1) - pstx(j)
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
        IF j = nos THEN gap = drt - pstx(nos) - iamlen ELSE gap = pstx(j + 1) - pstx(j)
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
        IF j = nos THEN gap = drt - pstx(nos) - iamlen ELSE gap = pstx(j + 1) - pstx(j)
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
        seq = seq + CHR$(4) + CHR$(spar(j).c) + CHR$(spar(j).h) + CHR$(spar(j).r) + CHR$(n)
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
        seq = seq + CHR$(3) + CHR$(1) + CHR$(1) + CHR$(1) + "Ў" + CHR$(1) + CHR$(1) + MKI$(179) + CHR$(spar(1).c) + CHR$(spar(1).h) + CHR$(spar(1).r) + CHR$(spar(1).n)
        noop = noop + 1
       END IF
      NEXT j
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

