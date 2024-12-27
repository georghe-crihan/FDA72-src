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
TYPE winpar         '---------- Parameters of window ----------
br AS INTEGER          'Starting row
bc AS INTEGER          'Starting column
nr AS INTEGER          'Number of rows
nc AS INTEGER          'Number of columns
tpos AS INTEGER        'Starting column of the title
tit AS STRING * 64     'Title
wintyp AS INTEGER      'Window type: 0 - Primary, 1 - Secondary, 2 - Alert, 3 - Help
END TYPE
DECLARE SUB interruptx (intnum%, inregs AS regtypex, outregs AS regtypex)
DECLARE SUB crypt (buf$, BYVAL cntr%, BYVAL ckey&)
DECLARE SUB halt ()
DECLARE SUB dmabwr (BYVAL offset%, buf$, BYVAL cntr%)
DECLARE SUB dmach2 (BYVAL dir%, BYVAL offset%, BYVAL cntr%)
DECLARE SUB dmabrd (BYVAL offset%, buf$, BYVAL cntr%)
DECLARE SUB dmabdel ()
DECLARE SUB biosfib ()
DECLARE SUB dosprin (s$)
DECLARE SUB minit ()
DECLARE SUB mon ()
DECLARE SUB mkey (x%, y%, z%, t%)
DECLARE SUB moff ()
DECLARE SUB msav ()
DECLARE SUB mrst ()
DECLARE SUB setattr (BYVAL fattr%, BYVAL lattr%, BYVAL xattr%)
DECLARE SUB setcol (BYVAL table%, BYVAL attr%, BYVAL col%)
DECLARE SUB svscr (BYVAL sr%, BYVAL sc%, BYVAL nr%, BYVAL nc%)
DECLARE SUB mkwind (BYVAL sr%, BYVAL sc%, BYVAL nr%, BYVAL nc%, BYVAL attr%)
DECLARE SUB mkbord1 (BYVAL sr%, BYVAL sc%, BYVAL nr%, BYVAL nc%, BYVAL attr%)
DECLARE SUB mkbord2 (BYVAL sr%, BYVAL sc%, BYVAL nr%, BYVAL nc%, BYVAL attr%)
DECLARE SUB fprint (BYVAL row%, BYVAL col%, s$, BYVAL attr%)
DECLARE SUB chattr (BYVAL row%, BYVAL col%, BYVAL l%, BYVAL attr%)
DECLARE SUB rstscr ()
DECLARE SUB cursor (BYVAL row%, BYVAL col%)
DECLARE SUB curoff ()
DECLARE SUB rvideos ()
DECLARE SUB timert (at1%, at2%, at3%)
DECLARE SUB swinfdcproc CDECL (BYVAL procnum%, xax%, xbx%, xcx%, xdx%, xfl%)
DECLARE SUB fdahook ()
DECLARE SUB fdaunh ()
DECLARE SUB dbtc (BYVAL n%)
DECLARE SUB waint ()
DECLARE SUB wincre (BYVAL br%, BYVAL bc%, BYVAL nr%, BYVAL nc%, stit$, BYVAL wintyp%)
DECLARE SUB winoff ()
DECLARE SUB mkbut (BYVAL br%, BYVAL bc%, BYVAL nc%, s$, BYVAL bt%, BYVAL ab%, BYVAL at%)
DECLARE SUB erwind (s$)
DECLARE SUB warwind (s$)
DECLARE SUB wawind (s$)
DECLARE SUB hlp (BYVAL helpnum%)
DECLARE SUB fmtrst ()
DECLARE SUB s1pos ()
DECLARE SUB trp ()
DECLARE SUB hrp (BYVAL ftyp%)
DECLARE SUB dwp (BYVAL ql&)
DECLARE SUB dtws ()
DECLARE SUB dosform (dfflen%, BYVAL anos%, BYVAL csz%, BYVAL fsz%, BYVAL noe%, BYVAL mdb%, fsys%)
DECLARE SUB stopmot ()
DECLARE SUB blin ()
DECLARE SUB woff ()
DECLARE SUB woff1 ()
DECLARE SUB hdpos (BYVAL cyl%)
DECLARE SUB abt ()
DECLARE SUB btws ()
DECLARE SUB cdens ()
DECLARE SUB dftws (BYVAL dfflen%, x%, q&, qoff&)
DECLARE SUB nomcalc (BYVAL dr%, BYVAL a%, qntl&, qndrt&)
DECLARE SUB sqst (f%, noe%, sf$, s3$)
DECLARE SUB fintran (f%, a%, b%, s$)
DECLARE SUB dcptim (q&, f%, xf%)
DECLARE SUB dcpabs (q&, f%)
DECLARE SUB dcpfwd (a%, ff%)
DECLARE SUB dcppos (a%, ff%)
DECLARE SUB dcpposid (a%, ff%)
DECLARE SUB calreq ()
DECLARE SUB dfpcalc (BYVAL noc%, BYVAL nos%, BYVAL nof%, aflg%, sform$())
DECLARE SUB ldfdfil (sform$(), nof%)
DECLARE SUB dcpbug (a%)
DECLARE SUB dcpmain (s1$, s2$, s3$, s4$)
DECLARE SUB clkident ()
DECLARE SUB clkregen ()
DECLARE SUB cerident ()
DECLARE SUB cerregen ()
DECLARE SUB gtdident ()
DECLARE SUB gtdregen ()
DECLARE SUB temppath ()
DECLARE SUB cerberusrec ()
DECLARE SUB closedisk ()
DECLARE SUB cvtws01 ()
DECLARE SUB cvtff04 (sf$)
DECLARE SUB cvtff05 ()
DECLARE SUB scanwrifile ()
DECLARE SUB scanfmtfile ()
DECLARE SUB wrirst ()
DECLARE SUB wriupd ()
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
DECLARE FUNCTION crc16% (buf$, BYVAL num%, BYVAL dt%)
DECLARE FUNCTION findif% (buf$, BYVAL num%)
DECLARE FUNCTION runspec$ (s$)
DECLARE FUNCTION timerf& (BYVAL dir%)
DECLARE FUNCTION dmabcre& ()
DECLARE FUNCTION dmachk& ()
DECLARE FUNCTION bioskey% ()
DECLARE FUNCTION biosknw% ()
DECLARE FUNCTION doslinp$ ()
DECLARE FUNCTION memsize& ()
DECLARE FUNCTION biosver& ()
DECLARE FUNCTION cputype% ()
DECLARE FUNCTION hardtbl& ()
DECLARE FUNCTION absread% (BYVAL drv%, BYVAL sec&, BYVAL nos%, buf$)
DECLARE FUNCTION fdardid% (BYVAL cmd%, qt&, BYVAL cstat%)
DECLARE FUNCTION fdaspos% (BYVAL cmd%, BYVAL cstat%)
DECLARE FUNCTION fdaagap% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL minlen%, BYVAL maxlen%)
DECLARE FUNCTION fdaamf% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL bnum%)
DECLARE FUNCTION fdacclr% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL gpl%)
DECLARE FUNCTION fdacsec% (BYVAL bseg%, BYVAL boff%, BYVAL blen%)
DECLARE FUNCTION fdactrk% (BYVAL bseg%, BYVAL boff%, BYVAL blen%, BYVAL gpl%, BYVAL nos%)
DECLARE FUNCTION clktim% (BYVAL hdd%, BYVAL ntsec%, BYVAL bseg%, BYVAL boff%)
DECLARE FUNCTION csfc% ()
DECLARE FUNCTION init% (BYVAL mode%)
DECLARE FUNCTION editstr% (BYVAL brow%, BYVAL bcol%, BYVAL l%, BYVAL attr%)
DECLARE FUNCTION msel% (BYVAL br%, BYVAL bc%, BYVAL nr%, BYVAL nc%, BYVAL nm%, BYVAL edf%, la%(), BYVAL dbcl%, BYVAL an%, BYVAL ai%, BYVAL ab%, cl%)
DECLARE FUNCTION eggs% (BYVAL br%, BYVAL bc%, BYVAL nr%, BYVAL nc%, opt%, BYVAL an%, BYVAL ab%)
DECLARE FUNCTION checkbox% (BYVAL br%, BYVAL bc%, BYVAL nc%, opt%, BYVAL an%, BYVAL ab%)
DECLARE FUNCTION buttons% (BYVAL br%, BYVAL bc%, BYVAL nc%, BYVAL nb%, BYVAL aa%, BYVAL ad%, BYVAL ao%)
DECLARE FUNCTION ixfr% (BYVAL dens%, BYVAL m%)
DECLARE FUNCTION drp% (boff%, BYVAL mode%)
DECLARE FUNCTION power% (BYVAL num%)
DECLARE FUNCTION obr% ()
DECLARE FUNCTION ctws% (linnum%)
DECLARE FUNCTION cxfr% (BYVAL dens%)
DECLARE FUNCTION command% (BYVAL comnum%, BYVAL helpnum%)
DECLARE FUNCTION ctrk% (BYVAL mingpl%, BYVAL noe%, sf$)
DECLARE FUNCTION drttbl% (BYVAL dens%)
DECLARE FUNCTION qtrln& (BYVAL boff%, BYVAL dens%)
DECLARE FUNCTION getbioserr% ()
DECLARE FUNCTION fwslabel% (s$, BYVAL noe%, qpars&())
DECLARE FUNCTION prowind% (s$)
DEFINT A-P, X-Z
DEFLNG Q
DEFSTR S-W
CONST mns = 64
CONST ctimer = 32
CONST dsd0 = 13, dsd1 = 11
DIM cmd(1 TO 9), cstat(10)
DIM lensec(7)
DIM gpltab(7)
DIM qcaltim(1, 4), qcalabs(1, 4), dcalfwd(1, 4), dcalpos(1, 4), dcalposid(1, 4)
DIM ctdop(2), ctabtkid(2), ctabtcrc(2), ctbdmpos(2), doptbl(4)
DIM spar(1 TO 64) AS secint
COMMON SHARED qidoffset(), spar() AS secint, qwrioffset(), dwrilen(), seq
COMMON SHARED cyl, head, dot, dtf, nos, alen, agpl, alltrkns
COMMON SHARED drvatyp, drvbtyp
COMMON SHARED qcaltim(), qcalabs(), dcalfwd(), dcalpos(), dcalposid()
COMMON SHARED ctdop(), ctabtkid(), ctabtcrc(), ctbdmpos(), ctmingpl, doptbl()
COMMON SHARED gpltab(), lensec(), dens()
COMMON SHARED hvflg, foflg, filnum, htflg, boflg, enerr, erflg, bioserr, dskchlin
COMMON SHARED hdd, drive, hlpflg, nobug
COMMON SHARED ctchid, ctlssf, chid, lssf, iamlen, ctenao, ctenww
COMMON SHARED stred, mpr, mpc, menuflg, mvflg, dboxflg, oboxflg, srepcrc, scrlf
COMMON SHARED stview, stedit, sbview
COMMON SHARED dfnos, dfrd, otrd, dcrd
COMMON SHARED regs AS regtypex, sid AS STRING * 160
COMMON SHARED nods, diskdens, drt, qtlen, qdrt, drvtyp
COMMON SHARED fwd, posc, poscid, dop, abtkid, abtcrc, bdmpos, maxlen
COMMON SHARED ncyl, disknum, sopdir, diskattr, idflg
COMMON SHARED cmd(), cstat()
COMMON SHARED dgroup, bsid, dmabseg, dmaboff, spath, stemp, ctwlock
