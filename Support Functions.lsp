(defun VAdd (A B)
  (list
    (+ (nth 0 A) (nth 0 B))
    (+ (nth 1 A) (nth 1 B))
    (+ (nth 2 A) (nth 2 B))
  ) ; list
) ; defun


(defun VSubtr (A B)
  (list
    (- (nth 0 A) (nth 0 B))
    (- (nth 1 A) (nth 1 B))
    (- (nth 2 A) (nth 2 B))
  ) ; list
) ; defun


(defun VDot (A B)
  (+
    (* (nth 0 A) (nth 0 B))
    (* (nth 1 A) (nth 1 B))
    (* (nth 2 A) (nth 2 B))
  ) ; list
) ; defun


(defun VMult (A x)
	(list 
    (* (nth 0 A) x)
    (* (nth 1 A) x)
    (* (nth 2 A) x)
  ) ; list
) ; defun


(defun VCross (A B)
  (list
    (- (* (nth 1 A) (nth 2 B)) (* (nth 2 A) (nth 1 B)))
    ;(- (- (* (nth 0 A) (nth 2 B)) (* (nth 2 A) (nth 0 B))))
    (- (* (nth 2 A) (nth 0 B)) (* (nth 0 A) (nth 2 B)))
    (- (* (nth 0 A) (nth 1 B)) (* (nth 1 A) (nth 0 B)))
  ) ; list
) ; defun


(setq XUnitVect (list 1 0 0))
(setq YUnitVect (list 0 1 0))
(setq ZUnitVect (list 0 0 1))

(setq PI/2 (/ pi 2))

(defun Row (n A)
	(nth n A)
) ; defun

(defun Col (n A / Ctr RetVal)
	(setq RetVal (list))
	(setq Ctr (1- (length A)))
	(while (<= 0 Ctr)
		(setq RetVal (cons (nth n (nth Ctr A)) RetVal))
		(setq Ctr (1- Ctr))
	) ; while
	RetVal
) ; defun

(defun MVDot (A B / RetVal)
	(setq RetVal 0)
	(setq ALen (Length A) BLen (Length B))
	(if (/= ALen BLen) (exit))
	
	(setq Ctr 0)
	(while (< Ctr BLen)
		(setq RetVal (+ RetVal (* (nth Ctr A) (nth Ctr B))))
		(setq Ctr (1+ Ctr))
	) ; while
	RetVal
) ; defun


(defun MMult (A B / RetVal RowMax ColMax RowCtr ColCtr) ; Main matrix multiplication routine
	
	(if (/= (length (nth 0 A)) (length B))
		(progn
			(princ "\nCannot Multiply these 2 matrices.  Exiting")
			(exit)
		) ; progn
	) ; if
	(setq RowMax (1- (length B)))
	(setq ColMax (1- (length (nth 0 B))))
	(setq RowCtr RowMax)
	
	(while (>= RowCtr 0)
		(setq ColCtr ColMax)
		(setq CurrRow (list))
		(setq ColCtr ColMax)
		(while (>= ColCtr 0)
			(Setq DotProd (MVDot (Row RowCtr A) (Col ColCtr B)))
			(setq CurrRow (cons DotProd CurrRow))
			(setq ColCtr (1- ColCtr))
		) ; while
		(setq RetVal (cons CurrRow RetVal))
		(setq RowCtr (1- RowCtr))
	) ; while
	RetVal
		
) ; defun


;;;(setq A (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
;;;(setq B (list (list 1 2) (list 3 4) (list 5 6)))


(defun AppendSelectionSets (SS1 SS2)
	(if (= SS1 nil)
		SS2
		(if (= SS2 nil)
			SS1
			(progn
				(setq NumEnts (SSLength SS2))
				(setq Ctr 0)
				(while (< Ctr NumEnts)
					(setq CurrEnt (ssname SS2 Ctr))
					(ssadd CurrEnt SS1)
					(setq Ctr (1+ Ctr))
				) ; while
				SS1
			) ; progn
		) ; if
	) ; if
) ; defun


(defun FindInList (Item ExistList / NumItems Ctr Continue)
	(setq NumItems (length ExistList))
	(setq Ctr 0)
	(setq Continue t)
	(while (and Continue (< Ctr NumItems))
		(if (= Item (nth Ctr ExistList))
			(setq Continue nil)
		) ; if
		(setq Ctr (1+ Ctr))
	) ; while
	(if Continue nil (1- Ctr))
) ; defun


(defun GetDefunsInFile (FullFilename / RetVal)
	(setq RetVal (list))
	(setq ThisFile (File->List FullFilename))
	(foreach Line ThisFile
		(if (= (substr Line 1 6) "(defun")
			(setq RetVal (cons (substr Line 8) RetVal))
		) ; if
	) ; foreach
	(reverse RetVal)
) ; defun


(defun List->SSet (EntList / Ctr RetVal)
	(setq RetVal (ssadd))
	(setq Ctr -1)
	(foreach Ent EntList (setq RetVal (ssadd (nth (setq Ctr (1+ Ctr)) EntList) RetVal)))
	RetVal
) ; defun


(defun RecursiveForceBlockColor (BlockName Color / doc blk)
	;(setq Color (cdr (assoc 62 (acad_truecolordlg 2))))
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(setq blk (vla-item (vla-get-blocks doc) (vla-get-Effectivename BlockName)))
	(vlax-for x blk
		(if (eq (vla-get-objectname x) "AcDbBlockReference")
			 (progn (RecursiveForceBlockColor x)) ; recursion
			 (vla-put-color x Color)
		) ; if
	) ; vlax-for
) ; defun


(defun GetEffectiveBlockName (obj)
	(vlax-get-property obj
		(if (vlax-property-available-p obj 'effectivename)
		    (progn 'effectivename)
		    'name
		) ; if
	) ; vlax-get-property
) ; defun
	

(defun UpdateBlockThruEditor (Block)
	;(setq blockname (GetEffectiveBlockName Block))
	(command "-BEDIT" "" Block)
	(command "zoom" "e")
	(command "_BCLOSE" "SAVE")
) ; defun


(defun File->List (FullFilename / RetVal FilePtr CurrLine)
	(setq RetVal (list))
	(setq FilePtr (open FullFilename "r"))
	(while (setq CurrLine (read-line FilePtr))
		(setq RetVal (cons CurrLine RetVal))
	) ; while
	(reverse RetVal)
) ; defun


(defun List->File (TextList FullFilename / RetVal FilePtr CurrLine)
	(setq FilePtr (open FullFilename "w"))
	(while TextList
		(setq CurrLine (car TextList))
		(setq TextList (cdr TextList))
		(write-line CurrLine FilePtr)
	) ; while
	(close FilePtr)
) ; defun

(defun Parse (str del / len lst pos)
    (setq len (1+ (strlen del)))
    (while (setq pos (vl-string-search del str))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))
        )
    )
    (reverse (cons str lst))
);

(defun CreateDictionary ()
	(entmakex '((0 . "DICTIONARY")(100 . "AcDbDictionary")))
) ; defun


(defun InvSin (y)
	(atan (/ y (sqrt (- 1 (Sqr y)))))
) ; defun

(defun Sqr (x) (* x x))


(defun LoadStandardErrorRoutine ()
	(defun *error* (msg)
		(princ (strcat "\nError: " msg))
		(setvar "osmode" OSMODE)
	) ; defun
) ; defun

(defun UnloadStandardErrorRoutine ()
	(setq *error* nil)
) ; defun


(defun ClearUserInput ()
	(setq OSMODE (getvar "osmode"))
	(setvar "osmode" 0)
) ; defun


(defun SetUserInput ()
	(setvar "osmode" OSMODE)
) ; defun
