000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. ADVENTOFCODE7.
000030
000040 DATA DIVISION.
000050 WORKING-STORAGE SECTION.
000060
000070 01  INPUT-LINE   PIC X(99).
000080 01  INPUT-FROM   PIC A.
000090 01  INPUT-TO     PIC A.
000100
000110 01  D            PIC 999.
000120 01  DEPENDENCY   OCCURS 200 TIMES.
000130     05  D-FROM   PIC A.
000140     05  D-TO     PIC A.
000150 01  NUM-DEPS     PIC 999.
000160
000170 01  T            PIC 999.
000180 01  NUM-TASKS    PIC 99.
000190 01  COMPLETED-RECORD.
000200     05  COMPLETED    PIC 9 OCCURS 26 TIMES.
000210
000220 01  IS-DOABLE    PIC 9.
000230 01  ALL-DONE     PIC 9.
000240 01  JF           PIC 99.
000250 01  JT           PIC 99.
000260 01  THE-ALPHABET PIC A(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
000270
000280 01  SECOND       PIC 9999.
000290 01  NUM-WORKERS  PIC 99 VALUE 5.
000300 01  W            PIC 9.
000310 01  WORKERS.
000320     05  WORKER       OCCURS 5 TIMES.
000330         10  WORK-ON    PIC A.
000340         10  WORK-LEFT  PIC 99.
000350 01  WORK-LEFT    PIC 99 OCCURS 5 TIMES.
000360 01  PROGRESS-RECORD.
000370     05  IN-PROGRESS    PIC 9 OCCURS 26 TIMES.
000380
000390
000400 PROCEDURE DIVISION.
000410
000420* READ THE INPUT.
000430 SET D TO 0
000440 PERFORM FOREVER
000450   ACCEPT INPUT-LINE
000460   IF INPUT-LINE = SPACE THEN EXIT PERFORM END-IF
000470   ADD 1 TO D
000480   MOVE INPUT-LINE(6:1) TO D-FROM OF DEPENDENCY(D)
000490   MOVE INPUT-LINE(37:1) TO D-TO OF DEPENDENCY(D)
000500 END-PERFORM
000510 MOVE D TO NUM-DEPS
000520
000530* COUNT HOW MANY TASKS THERE ARE.
000540 MOVE 0 TO NUM-TASKS
000550 PERFORM WITH TEST AFTER VARYING D FROM 1 BY 1 UNTIL D = NUM-DEPS
000560   PERFORM COMPUTE-J
000570   MOVE FUNCTION MAX(NUM-TASKS, JF, JT) TO NUM-TASKS
000580 END-PERFORM
000590
000600* PART 1.
000610 PERFORM PRINT-NEXT-TASK WITH TEST AFTER UNTIL ALL-DONE = 1
000620 DISPLAY " ".
000630
000640* PART 2.
000650 INITIALIZE COMPLETED-RECORD.
000660 INITIALIZE PROGRESS-RECORD.
000670 MOVE 0 TO SECOND
000680 PERFORM DO-WORK WITH TEST AFTER UNTIL ALL-DONE = 1
000710 STOP RUN.
000720
000730
000740 PRINT-NEXT-TASK SECTION.
000750   MOVE 1 TO ALL-DONE
000760   PERFORM WITH TEST AFTER
000770           VARYING T FROM 1 BY 1 UNTIL T = NUM-TASKS
000780     IF COMPLETED(T) = 0 THEN
000790       MOVE 0 TO ALL-DONE
000800       MOVE 1 TO IS-DOABLE
000810       PERFORM CHECK-DOABLE
000820       IF IS-DOABLE = 1 THEN
000830         DISPLAY THE-ALPHABET(T:1) WITH NO ADVANCING
000840         MOVE 1 TO COMPLETED(T)
000850         EXIT PERFORM
000860       END-IF
000870     END-IF
000880   END-PERFORM
000890 EXIT SECTION.
000900
000910 COMPUTE-J SECTION.
000920   COMPUTE JF = FUNCTION ORD(D-FROM OF DEPENDENCY(D)) - 65
000930   COMPUTE JT = FUNCTION ORD(D-TO   OF DEPENDENCY(D)) - 65
000940 EXIT SECTION.
000950
000960 CHECK-DOABLE SECTION.
000970   PERFORM WITH TEST AFTER
000980         VARYING D FROM 1 BY 1 UNTIL D = NUM-DEPS
000990     PERFORM COMPUTE-J
001000     IF (T = JT) AND COMPLETED(JF) = 0 THEN
001010       MOVE 0 TO IS-DOABLE
001020     END-IF
001030   END-PERFORM
001040 EXIT SECTION.
001050
001060 DO-WORK SECTION.
001070   PERFORM WORK-STEP
001080         WITH TEST AFTER VARYING W FROM 1 BY 1
001090         UNTIL W = NUM-WORKERS
001100
001110   MOVE 1 TO ALL-DONE
001120   PERFORM WITH TEST AFTER
001130           VARYING T FROM 1 BY 1 UNTIL T = NUM-TASKS
001140     IF COMPLETED(T) = 0 THEN
001150       MOVE 0 TO ALL-DONE 
001160     END-IF
001170     IF IN-PROGRESS(T) = 0 THEN
001180       MOVE 1 TO IS-DOABLE
001190       PERFORM CHECK-DOABLE
001200       IF IS-DOABLE = 1 THEN
001210         PERFORM ASSIGN-TASK
001220       END-IF
001230     END-IF
001240   END-PERFORM
001250
001260   DISPLAY SECOND, " ", WORKER(1)
001270                 , " ", WORKER(2)
001280                 , " ", WORKER(3)
001290                 , " ", WORKER(4)
001300                 , " ", WORKER(5)
001310   ADD 1 TO SECOND
001320 EXIT SECTION.
001330
001340 WORK-STEP SECTION.
001350   IF WORK-LEFT OF WORKER(W) > 0 THEN
001360     SUBTRACT 1 FROM WORK-LEFT OF WORKER(W)
001370     IF WORK-LEFT OF WORKER(W) = 0 THEN
001380       COMPUTE T = FUNCTION ORD(WORK-ON OF WORKER(W)) - 65
001390       MOVE SPACE TO WORK-ON OF WORKER(W)
001400       MOVE 1 TO COMPLETED(T)
001410     END-IF
001420   END-IF
001430 EXIT SECTION.
001440
001450 ASSIGN-TASK SECTION.
001460   PERFORM
001470     WITH TEST AFTER VARYING W FROM 1 BY 1
001480     UNTIL W = NUM-WORKERS
001490
001500     IF WORK-LEFT OF WORKER(W) = 0
001510       MOVE THE-ALPHABET(T:1) TO WORK-ON OF WORKER(W)
001520       MOVE T TO WORK-LEFT OF WORKER(W)
001530       ADD 60 TO WORK-LEFT OF WORKER(W)
001540       MOVE 1 TO IN-PROGRESS(T)
001550       EXIT SECTION
001560     END-IF
001570   END-PERFORM
001580 EXIT SECTION.
001590
