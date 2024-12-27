'$INCLUDE: 'FDA72-G.BI'

      DEF SEG = &H40
      ON ERROR GOTO er                       '**************************
      REDIM la(0)
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
      IF INSTR(COMMAND$, "/C") THEN ctchid = 0
      dboxflg = -1: oboxflg = -1
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

      s = DIR$(spath + "FDA_?.EXE")
      IF s <> "" THEN
       slang = MID$(s, LEN(s) - 4, 1)
       IF slang = "R" THEN flgrus = 1 ELSE flgrus = 0
      ELSE
       flgrus = 0
      END IF

      drive = 0
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
      IF ctchid = 0 THEN
       IF flgrus THEN
        s = "Режим /C включен"
       ELSE
        s = "/C mode activated"
       END IF
       fprint 24, 1, s, 3
      END IF
      fprint 24, 65, "│ FDA 7.2 Auto", 3
      enerr = 1

prow: wincre 5 - flgrus, 12, flgrus + 15, 55, "", dboxflg
      IF flgrus THEN
       fprint 6, 21, "*** Floppy Disk Analyser 7.2 Auto ***", 7
       fprint 7, 17, "Программа предназначена для копирования дискет,", 7
       fprint 8, 15, "защищенных от копирования.", 7
       fprint 10, 15, "Авторы: Михаил и Александр Соколов, МТЦ Мединком", 7
       fprint 11, 15, "E-mail: msokolov@ivan.Harhan.ORG", 7
       fprint 13, 17, "Во время копирования дискета-оригинал будет на-", 7
       fprint 14, 15, "зываться " + CHR$(34) + "источник" + CHR$(34) + ", а дискета для  записи  копии", 7
       fprint 15, 15, "будет называться " + CHR$(34) + "приемник" + CHR$(34) + ".", 7
       mkbut 17, 23, 15, "Копирование", 1, 14, 18
       mkbut 17, 41, 15, "Выход", 0, 16, 18
       a = 17: b = 23: c = 15
      ELSE
       fprint 7, 21, "*** Floppy Disk Analyser 7.2 Auto ***", 7
       fprint 8, 20, "Copyprotected diskette copying program.", 7
       fprint 10, 15, "Written by Michael and Alex Sokolov, MTC Medincom", 7
       fprint 11, 15, "E-mail: msokolov@ivan.Harhan.ORG", 7
       fprint 13, 17, "During copying the original  diskette  will  be", 7
       fprint 14, 15, "called " + CHR$(34) + "source" + CHR$(34) + " and the diskette for copy will be", 7
       fprint 15, 15, "called " + CHR$(34) + "target" + CHR$(34) + ".", 7
       mkbut 17, 26, 12, "Continue", 1, 14, 18
       mkbut 17, 41, 12, "Quit", 0, 16, 18
       a = 17: b = 26: c = 12
      END IF
      mon
prw1: x = buttons(a, b, c, 2, 12, 14, 16)
      ON x + 1 GOTO prw1, prw1, prw3, prw1, prw1, prw2, prw3
prw2: moff: winoff: GOTO copy
prw3: moff: winoff: GOTO quit

copy: moff
      IF drvbtyp = 0 GOTO dtyp
      wincre 6, 21, 12, 38, "", dboxflg
      mkbord1 8, 24, 6, 12, 7
      IF flgrus THEN
       fprint 8, 25, " Дисковод ", 19
       s1 = "Продолжить"
       s2 = "Прекратить"
      ELSE
       fprint 8, 26, " Drive ", 19
       s1 = "Continue"
       s2 = "Cancel"
      END IF
      fprint 9, 35, CHR$(24), 5: mkwind 10, 35, 2, 1, 5: fprint 12, 35, CHR$(25), 5
      fprint 9, 30, "A:", 7: fprint 10, 30, "B:", 7
      mkbut 15, 24, 14, s1, 1, 14, 18
      mkbut 15, 41, 14, s2, 0, 16, 18
      mon
adl1: x = msel(9, 25, 4, 10, 2, 0, la(), 0, 7, 8, 9, drive)
      IF x = 6 THEN
       IF mpr = 15 AND mpc > 23 AND mpc < 39 GOTO adl3
       IF mpr = 15 AND mpc > 40 AND mpc < 56 GOTO adl4
      ELSE : ON x + 1 GOTO adl2, adl2, adl2, adl4, adl1
      END IF
      GOTO adl1
adl2: x = buttons(15, 24, 14, 2, 12, 14, 16)
      IF x = 4 THEN
       IF mpr > 7 AND mpr < 14 AND mpc > 23 AND mpc < 35 THEN
        IF mpr > 8 AND mpr < 11 AND mpc > 24 AND mpc < 34 THEN
         chattr 9 + drive, 25, 10, 7
         drive = mpr - 9
        END IF
        GOTO adl1
       END IF
      ELSE
       ON x + 1 GOTO adl1, adl1, adl4, adl1, adl2, adl3, adl4
      END IF
      GOTO adl2
adl3: x = 0
adl4: moff: winoff
      IF x GOTO prow
dtyp: IF flgwri = 0 THEN ctwlock = 1: sopdir = stemp + "~UNTITLD\": diskattr = -1
      curdsk = -1
      wincre 3, 15, 19, 50, "", oboxflg
      mkbord1 5, 18, 7, 44, 7
      mkbord1 13, 18, 6, 44, 7
      IF flgrus THEN
       fprint 5, 35, " Операции ", 19
       fprint 6, 22, "Определение типа дискеты", 7
       fprint 7, 22, "Калибровка", 7
       fprint 8, 22, "Чтение данных с дискеты-источника", 7
       fprint 9, 22, "Обработка результатов чтения", 7
       fprint 10, 22, "Запись данных на дискету-приемник", 7
       fprint 13, 35, " Прогресс ", 19
      ELSE
       fprint 5, 34, " Operations ", 19
       fprint 6, 22, "Determining diskette type", 7
       fprint 7, 22, "Calibrating", 7
       fprint 8, 22, "Reading data from source diskette", 7
       fprint 9, 22, "Analysing read results", 7
       fprint 10, 22, "Writing data on target diskette", 7
       fprint 13, 35, " Progress ", 19
      END IF
      IF flgwri THEN
       flgwri = 0
       FOR i = 6 TO 9
        fprint i, 20, CHR$(251), 7
       NEXT i
       GOTO wdat
      END IF
      x = 0: GOSUB oprb
      IF drive THEN drvtyp = drvbtyp ELSE drvtyp = drvatyp
      SELECT CASE drvtyp
       CASE 1: determ = 0: ncyl = 42: ncyln = 40: dens = 1
       CASE 2: determ = 3: ncyl = 84: ncyln = 80
       CASE 3: determ = 0: ncyl = 82: ncyln = 80: dens = 1
       CASE 4: determ = 1: ncyl = 82: ncyln = 80
      END SELECT
      IF determ THEN
       IF curdsk THEN
        x = 0: GOSUB adsk: IF x THEN winoff: GOTO prow
        curdsk = 0
       END IF
       IF determ AND 1 THEN diskdens = 0 ELSE diskdens = power(dens)
       x = init(0)
       IF erflg GOTO er0
       IF determ AND 1 THEN
        GOSUB dddp
        IF x THEN
         stopmot
         IF flgrus THEN
          s = "Не удается определить тип диска"
         ELSE
          s = "Cannot determine disk type"
         END IF
         GOSUB erwn
         winoff
         GOTO prow
        END IF
        determ = determ AND 2
        IF dens <> 1 THEN determ = determ AND 1
       END IF
       IF determ AND 2 THEN
        GOSUB ncdp
        IF x THEN
         stopmot
         IF flgrus THEN
          s = "Не удается определить тип диска"
         ELSE
          s = "Cannot determine disk type"
         END IF
         GOSUB erwn
         winoff
         GOTO prow
        END IF
        determ = determ AND 1
       END IF
       stopmot
      END IF
      x = 0: GOSUB opre
dcal: x = 1: GOSUB oprb
      IF dcalfwd(drive, dens) = 0 THEN
       IF curdsk <> 1 THEN
        x = 1: GOSUB adsk: IF x THEN winoff: GOTO prow
        curdsk = 1
       END IF
       dcrd = dens
       x = init(1)
       IF erflg GOTO er0
       y = ixfr(dcrd, 0)
       GOSUB dclp
       IF x THEN
        stopmot
        IF flgrus THEN
         s = "Ошибка калибровки"
        ELSE
         s = "Calibrate failure"
        END IF
        GOSUB erwn
        winoff
        GOTO prow
       END IF
       stopmot
       flgsnd = 1
      END IF
      x = 1: GOSUB opre
rdat: x = 2: GOSUB oprb
      REDIM qidoffset(ncyl - 1, 1)
      IF DIR$(sopdir + "DISK.FMT") <> "" THEN KILL sopdir + "DISK.FMT"
      OPEN sopdir + "DISK.FMT" FOR BINARY ACCESS READ WRITE AS #5
      s = "SMA FF06" + CHR$(disknum) + CHR$(ncyl): PUT #5, , s
      qidoff = 10
      IF curdsk THEN
       x = 0: GOSUB adsk: IF x THEN closedisk: winoff: GOTO prow
       curdsk = 0
      END IF
      x = init(0)
      IF erflg GOTO er0
      GOSUB ddrp
      IF x THEN closedisk: winoff: GOTO prow
      stopmot
ncylcutret:
      flgsnd = 1
      x = 2: GOSUB opre
obrd: x = 3: GOSUB oprb
      REDIM qwrioffset(ncyl - 1, 1), dwrilen(ncyl - 1, 1)
      GOSUB obrp
      IF x THEN closedisk: winoff: GOTO prow
      diskattr = 1
      flgsnd = 1
      x = 3: GOSUB opre
wdat: x = 4: GOSUB oprb
wdt0: flgretry = 1
      IF curdsk <> 1 THEN
       x = 1: GOSUB adsk: IF x THEN flgretry = 0: closedisk: winoff: GOTO prow
       curdsk = 1
      END IF
      x = init(1)
      IF erflg GOTO er0
      flgretry = 0
      GOSUB ddwp
      IF x THEN closedisk: winoff: GOTO prow
      stopmot
      flgsnd = 1
      x = 4: GOSUB opre

eopr: winoff
      GOSUB snds
      IF flgrus THEN
       s = "Записать еще одну копию с того же оригинала?"
      ELSE
       s = "Write one more copy from the same original?"
      END IF
      GOSUB qwnd
      IF x = 0 THEN flgwri = 1: GOTO dtyp
      closedisk
      IF flgrus THEN
       s = "Скопировать другую дискету (другой оригинал)?"
      ELSE
       s = "Copy another diskette (another original)?"
      END IF
      GOSUB qwnd
      IF x = 0 THEN GOTO copy

quit: enerr = 0: rvideos
abtm: IF DIR$(stemp + "~UNTITLD\*.*") <> "" THEN KILL stemp + "~UNTITLD\*.*"
      LSET sid = stemp + "~UNTITLD" + CHR$(0)
      regs.ax = &H3A00
      regs.dx = bsid
      regs.ds = dgroup
      interruptx &H21, regs, regs
      END

adsk: moff
      GOSUB snds
      IF flgrus THEN
       s1 = "Вставьте дискету-источник в дисковод " + CHR$(drive + 65) + ":"
       s2 = "Вставьте дискету-приемник в дисковод " + CHR$(drive + 65) + ":"
       s3 = "Рекомендуется защитить ее от записи!"
       s4 = "Она будет полностью переформатирована!"
       s5 = "Продолжить"
       s6 = "Прекратить"
      ELSE
       s1 = "Insert source diskette in drive " + CHR$(drive + 65) + ":"
       s2 = "Insert target diskette in drive " + CHR$(drive + 65) + ":"
       s3 = "It is recommended to write-protect it!"
       s4 = "It will be fully re-formatted!"
       s5 = "Continue"
       s6 = "Cancel"
      END IF
      IF x THEN sa = s2: sb = s4 ELSE sa = s1: sb = s3
      x1 = LEN(sa): x2 = LEN(sb)
      IF x1 < x2 THEN xl = x2 ELSE xl = x1
      post = (80 - xl) \ 2
      IF x THEN a = 2 ELSE a = dboxflg
      wincre 8, post - 3, 8, xl + 6, "", a
      fprint 10, post, sa, 7
      fprint 11, post, sb, 7
      mkbut 13, 24, 14, s5, 1, 14, 18
      mkbut 13, 41, 14, s6, 0, 16, 18
      mon
dsk1: x = buttons(13, 24, 14, 2, 12, 14, 16)
      ON x + 1 GOTO dsk1, dsk1, dsk3, dsk1, dsk1, dsk2, dsk3
dsk2: x = 0
dsk3: moff: winoff: RETURN
erwn: msav
      moff
      post = (80 - LEN(s)) \ 2
      IF flgrus THEN v = "Ошибка" ELSE v = "Error"
      wincre 9, post - 3, 7, LEN(s) + 6, v, 2
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
erw2: moff: winoff: mrst: RETURN
oprb: fprint x + 6, 20, CHR$(7), 7
      IF x = 1 THEN
       IF flgrus THEN
        fprint 14, 20, "Подождите 25 с", 7
       ELSE
        fprint 14, 20, "Wait for 25 s", 7
       END IF
      END IF
      IF x > 1 THEN
       fprint 15, 20, STRING$(25, 176) + "    %", 7
       IF flgrus THEN
        fprint 15, 51, "завершено", 7
        fprint 17, 20, "Для прерывания нажмите Esc", 7
        chattr 17, 43, 3, 9
       ELSE
        fprint 15, 51, "completed", 7
        fprint 17, 20, "Press Esc to abort", 7
        chattr 17, 26, 3, 9
       END IF
       xp = -1
      END IF
      RETURN
aown: x = (cyl * 2 + head) / (ncyl * 2) * 100
      IF x <> xp THEN
       s = STRING$(4, 32): RSET s = STR$(x)
       fprint 15, 45, s, 9
       FOR i = xp \ 4 + 1 TO x \ 4
        fprint 15, 19 + i, CHR$(219), 7
       NEXT i
       xp = x
      END IF
      mkey f, a, b, bs
      IF f = 1 AND a = 27 THEN
       stopmot
       wincre 9, 21, 7, 37, "", 2
       IF flgrus THEN
        fprint 11, 24, "Операция прервана пользователем", 7
        s1 = "Прервать"
        s2 = "Продолжить"
       ELSE
        fprint 11, 24, "Operation interrupted by user", 7
        s1 = "Abort"
        s2 = "Resume"
       END IF
       mkbut 13, 24, 14, s1, 1, 14, 18
       mkbut 13, 41, 14, s2, 0, 16, 18
       mon
       DO: mkey f, a, b, bs: LOOP WHILE f: f = 1
aow1:  x = buttons(13, 24, 14, 2, 12, 14, 16)
       ON x + 1 GOTO aow1, aow1, aow3, aow1, aow1, aow2, aow3
aow3:  f = -1
aow2:  moff: winoff
      ELSE
       f = 0
      END IF
      RETURN
opre: fprint x + 6, 20, CHR$(251), 7
      mkwind 14, 20, 4, 40, 7
      RETURN
qwnd: moff
      IF flgrus THEN
       s1 = "Да": s2 = "Нет"
      ELSE
       s1 = "Yes": s2 = "No"
      END IF
      post = (80 - LEN(s)) \ 2
      wincre 9, post - 3, 7, LEN(s) + 6, "", dboxflg
      fprint 11, post, s, 7
      mkbut 13, 28, 10, s1, 1, 14, 18
      mkbut 13, 41, 10, s2, 0, 16, 18
      mon
qwn1: x = buttons(13, 28, 10, 2, 12, 14, 16)
      ON x + 1 GOTO qwn1, qwn1, qwn3, qwn1, qwn1, qwn2, qwn3
qwn2: x = 0
qwn3: moff: winoff: RETURN
snds: IF flgsnd THEN
       SOUND 600, 2: SOUND 850, 2: SOUND 900, 2
       flgsnd = 0
      END IF
      RETURN

dddp: cyl = 0: head = 0: hdd = drive
      hdpos cyl: IF erflg THEN RETURN er0
      IF drvtyp = 2 THEN a = 3 ELSE a = 4
      xf = 1
      FOR i = 1 TO 2
       IF i = 1 THEN dens = 1 ELSE dens = a
       x = ixfr(dens, 0)
       cmd(1) = &H4A
       cmd(2) = hdd
       dbtc 2: IF erflg THEN RETURN er0
       waint
       IF erflg THEN RETURN er0
       n = csfc: IF erflg THEN RETURN er0
       IF (cstat(1) AND 1) = 0 THEN xf = 0: diskdens = power(dens): EXIT FOR
      NEXT i
      x = xf: RETURN
ncdp: cyl = 2: head = 0: hdd = drive
      hdpos cyl: IF erflg THEN RETURN er0
      x = ixfr(dens, 0)
      x = 1
      FOR i = 1 TO mns
       cmd(1) = &H4A
       cmd(2) = hdd
       dbtc 2: IF erflg THEN RETURN er0
       waint
       IF erflg THEN RETURN er0
       n = csfc: IF erflg THEN RETURN er0
       IF cstat(0) AND &HC0 THEN EXIT FOR
       c = cstat(3)
       IF c = 2 THEN x = 0: EXIT FOR
       IF c = 1 THEN ncyl = ncyl \ 2: ncyln = ncyln \ 2: nods = nods * 2: x = 0: EXIT FOR
      NEXT i
      RETURN
dclp: q = dmabcre: IF q = 0 THEN ERROR 7
      boflg = 1: dmabseg = e243(q): dmaboff = e241(q)
      cyl = 0: head = 0: hdd = drive
      hdpos cyl: IF erflg THEN RETURN er0
      dcptim q, f, xf: IF erflg THEN RETURN er0
      IF xf THEN
       stopmot
       dmabdel
       boflg = 0
       IF flgrus THEN
        s = "FDA не может работать из-за нестандартного контроллера гибкого диска"
       ELSE
        s = "FDA cannot operate because of non-standard floppy disk controller"
       END IF
       GOSUB erwn
       winoff
       RETURN prow
      END IF
      IF f THEN x = 1: GOTO cend
      qcaltim(drive, dcrd) = q
      dcpabs q, f: IF erflg THEN RETURN er0
      IF f THEN x = 1: GOTO cend
      qcalabs(drive, dcrd) = q
      dcpfwd a, ff: IF erflg THEN RETURN er0
      IF ff THEN x = 1: GOTO cend
      dcalfwd(drive, dcrd) = a
      dcppos a, ff: IF erflg THEN RETURN er0
      IF ff THEN x = 1: GOTO cend
      dcalpos(drive, dcrd) = a
      dcpbug a: IF erflg THEN RETURN er0
      x = 0
cend: dmabdel
      boflg = 0
      RETURN
ddrp: POKE &H40, 0
      qoff = 0
      q = dmabcre: IF q = 0 THEN ERROR 7
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
      aflg = 0
    FOR cyl = 0 TO ncyl - 1
     FOR head = 0 TO 1
      POKE &H40, 0
      hdd = drive
      IF head THEN hdd = hdd + 4
      GOSUB aown
      IF f = 1 THEN
pex1:  aflg = 1: EXIT FOR
      END IF
      IF f = -1 THEN x = init(0): IF erflg THEN RETURN er0
      hdpos cyl: IF erflg THEN RETURN er0
      dot = dens
      x = ixfr(dot, 1)
      ERASE spar
      ncylcutflg = 1
      x = drp(boff, 0): IF erflg THEN RETURN er0
      ncylcutflg = 0
      IF nos <> 0 AND x <> 0 THEN
       x = drp(boff, 1): IF erflg THEN RETURN er0
      END IF
      FOR i = 1 TO nos
       IF spar(i).offset <> -1 THEN spar(i).offset = spar(i).offset + qoff
      NEXT i
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
      qoff = qoff + boff
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
      dmabdel
      boflg = 0
      x = aflg: RETURN
obrp: IF DIR$(sopdir + "DISK.WRI") <> "" THEN KILL sopdir + "DISK.WRI"
      OPEN sopdir + "DISK.WRI" FOR BINARY ACCESS READ WRITE AS #6
      s = "SMA WS02" + CHR$(disknum) + CHR$(ncyl): PUT #6, , s: qoff = 10
      aflg = 0
      FOR cyl = 0 TO ncyl - 1
       FOR head = 0 TO 1
        GOSUB aown
        IF f = 1 THEN aflg = 1: EXIT FOR
        fmtrst
        IF nos THEN
         chid = ctchid: lssf = ctlssf
         xf = 0
         IF spar(nos).dt <> 2 THEN
          IF spar(nos).n < 8 THEN
           IF gpltab(spar(nos).n) > spar(nos).gpl + dop THEN xf = 3
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
       NEXT head
       IF aflg THEN EXIT FOR
      NEXT cyl
      IF aflg THEN x = 1: RETURN
      clkident
      cerident
      gtdident
      x = 0: RETURN
ddwp: POKE &H40, 0
      aflg = 0
      q = dmabcre: IF q = 0 THEN ERROR 7
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
      POKE &H40, 0
   FOR cyl = 0 TO ncyl - 1
    FOR head = 0 TO 1
      GOSUB aown
      IF f = 1 THEN
pex2:  aflg = 1: EXIT FOR
      END IF
      IF f = -1 THEN x = init(1): IF erflg THEN RETURN er0
      wrirst
      IF ASC(MID$(seq, 5, 1)) = 0 GOTO ntrk1
      hdd = drive
      IF head THEN hdd = hdd + 4
      hdpos cyl: IF erflg THEN RETURN er0
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
      dwp ql - 1024: IF erflg THEN RETURN er0
ntrk1:
    NEXT head
      IF aflg THEN EXIT FOR
   NEXT cyl
      IF aflg = 0 THEN
       POKE &H40, 0
       clkregen
       IF erflg THEN RETURN er0
       cerregen
       IF erflg THEN RETURN er0
       gtdregen
       IF erflg THEN RETURN er0
      END IF
      regs.ax = &H3E00
      regs.bx = filnum
      interruptx &H21, regs, regs
      foflg = 0
      dmabdel
      boflg = 0
      x = aflg: RETURN

ncfd: a = 0: GOTO emsg
w386: a = 1
emsg: SELECT CASE a * 2 + flgrus
       CASE 0: s = "At least one AT-compatible floppy disk drive required"
       CASE 1: s = "Требуется хотя бы один AT-совместимый дисковод"
       CASE 2: s = "FDA cannot be run under supervision of Microsoft Windows/386"
       CASE 3: s = "FDA нельзя запускать под управлением Microsoft Windows/386"
      END SELECT
      dosprin s + scrlf: GOTO abtm

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
      IF ERR <> 200 OR ncylcutflg = 0 THEN closedisk
      IF foflg THEN
       regs.ax = &H3E00
       regs.bx = filnum
       interruptx &H21, regs, regs
       foflg = 0
      END IF
      IF htflg THEN qtemp = timerf(1): htflg = 0
      IF boflg THEN dmabdel: boflg = 0
      stopmot
      xtemp = ncylcutflg: ncylcutflg = 0
      SELECT CASE ERR
       CASE 24, 25, 27, 57, 68, 70, 71, 72
        IF flgrus THEN serr = "Ошибка ввода/вывода на диске" ELSE serr = "Drive I/O error"
       CASE 53, 64, 75, 76
        IF flgrus THEN serr = "Файл/путь не найден" ELSE serr = "File/path not found"
       CASE 54
        IF flgrus THEN serr = "Доступ невозможен" ELSE serr = "Access denied"
       CASE 61, 67
        IF flgrus THEN serr = "Диск переполнен" ELSE serr = "Disk full"
       CASE 62
        IF flgrus THEN serr = "Неожиданный конец файла" ELSE serr = "Unexpected end-of-file"
       CASE 7, 14
        IF flgrus THEN serr = "Недостаточно памяти" ELSE serr = "Out of memory"
       CASE 200
        IF xtemp AND cyl >= ncyln THEN ncyl = ncyln: RESUME ncylcutret
        IF flgrus THEN serr = "Таймаут диска" ELSE serr = "Disk timeout"
       CASE 201
        IF flgrus THEN serr = "Ошибка FDC" ELSE serr = "FDC failure"
       CASE 202
        IF flgrus THEN serr = "Ошибка позиционирования" ELSE serr = "Seek failure"
       CASE 203
        IF flgrus THEN serr = "Нет диска в дисководе" ELSE serr = "No disk in drive"
       CASE 204
        IF flgrus THEN serr = "Диск не должен быть защищен от записи" ELSE serr = "Disk must not be write-protected"
       CASE ELSE
        dosprin "Fatal Error" + STR$(ERR) + scrlf: END
        'ERROR ERR
      END SELECT
opcan:
      IF enerr THEN
       s = serr: GOSUB erwn
       IF flgretry THEN curdsk = -1: RESUME wdt0
       woff
       RESUME prow
      END IF
      xe = 1: RESUME NEXT
END

