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
DECLARE SUB gtdregen (send$, sinf$)
DECLARE FUNCTION i2l& (BYVAL i%)
DECLARE FUNCTION e121% (BYVAL i%)
DECLARE FUNCTION e122% (BYVAL i%)
DECLARE FUNCTION e141% (BYVAL l&)
DECLARE FUNCTION e142% (BYVAL l&)
DECLARE FUNCTION e143% (BYVAL l&)
DECLARE FUNCTION e144% (BYVAL l&)
DECLARE FUNCTION e241% (BYVAL l&)
DECLARE FUNCTION e243% (BYVAL l&)
DECLARE FUNCTION findif% (buf$, BYVAL num%)
DECLARE FUNCTION fdaamf% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL bnum%)
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
DIM regs AS regtypex

      send = STRING$(512, 0)
      regs.ax = &H201
      regs.bx = SADD(send)
      regs.cx = 10
      regs.dx = 0
      regs.es = SSEG(send)
      interruptx &H13, regs, regs
      sinf = STRING$(512, 0)
      regs.ax = &H201
      regs.bx = SADD(sinf)
      regs.cx = 11
      regs.dx = 0
      regs.es = SSEG(sinf)
      interruptx &H13, regs, regs
      gtdregen send, sinf
      regs.ax = &H301
      regs.bx = SADD(sinf)
      regs.cx = 11
      regs.dx = 0
      regs.es = SSEG(sinf)
      interruptx &H13, regs, regs


END

SUB gtdregen (send, sinf)
      s = "Грузовая таможенная декларация"
      xstrcnt = &H4454
      xstrsum = &H4454
      posit = 3
      DO
       IF posit > 128 THEN EXIT DO
       x = ASC(MID$(sinf, posit, 1))
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
      x = fdaamf(SSEG(send), SADD(send), 512, 3)
      PRINT HEX$(x)
      IF x = -1 THEN PRINT "Cannot find 1st sector DATA AM": END
      qbx = (515 - x \ 8) * 256& AND 65535
      qax = findif(send, 512) * 256& AND 65535
      FOR i = 1 TO 8
       a = e141(qax)
       qax = e142(qax) * 256& + e141(qbx)
       qbx = e142(qbx) * 256& + a
       qax = qax \ 2
       qbx = qbx \ 2
      NEXT i
      x = e241(qax * 256 + qbx) XOR xstrsum
      MID$(sinf, 1) = MKI$(x)
END SUB

