(if (not Vadd) (load "Support Functions"))
(if (not c:SliceX) (load "Solid Modeling Utilities"))
(if (not vlax-ldata-set) (vl-load-com))

; Load up the LPDictionary
(if (not (setq SLEWBEARINGDICTIONARY (cdr (assoc -1 (dictsearch (namedobjdict) "LPDictionary")))))
	(dictadd (namedobjdict) "LPDictionary"(setq SLEWBEARINGDICTIONARY (CreateDictionary)))
) ; if


(defun NewtRap (Fn Guess / DeltaBeta CurrBeta CurrBetaEval CurrBetaPlusDelta CurrBetaPlusDeltaEval Gradient NextBeta ResultantDelta)
	(setq DeltaBeta 0.0001)
	(setq CurrBeta Guess)
	(while
		(progn
			(setq CurrBetaEval (apply
													 Fn (list CurrBeta)))
			(setq CurrBetaPlusDelta (+ CurrBeta DeltaBeta))
			(setq CurrBetaPlusDeltaEval (apply Fn (list CurrBetaPlusDelta)))
			(setq Gradient (/ (- CurrBetaPlusDeltaEval CurrBetaEval) DeltaBeta))
			(setq NextBeta (- CurrBeta (/ CurrBetaEval Gradient)))
			(setq ResultantDelta (abs (- CurrBeta NextBeta)))
			(> ResultantDelta DeltaBeta)
		) ; progn
		(setq CurrBeta NextBeta)
	) ; while
	CurrBeta
) ; defun


(defun 2xAnd1 (X) (+ (* 2 X) 1))
(defun XSquaredMinus2XMinus1 (X) (- (* 2 (Sqr X)) 1))


(defun FindAlphaBetaAndEtaFromNumberOfRollerPairs (NumOfRollerPairs BetaAndEta Beta Eta Alpha)
	(setq Alpha (/ pi NumOfRollerPairs 2))
	(setq BetaAndEta (FindBetaAndEtaForAlpha Alpha))
	(setq Beta (nth 0 BetaAndEta))
	(setq Eta (nth 1 BetaAndEta))
	(list Alpha Beta Eta)
) ; defun


(defun DrawThrustBearing ()
	; First get the angles
	(setq NumOfRollerPairs 5)
	(Setq Angles (FindAlphaBetaAndEtaFromNumberOfRollerPairs NumOfRollerPairs))
	(Princ Angles)
) ; defun


(defun GraphBetaFunction ()
	(setq GLOBALALPHA (/ Pi 10))
	(setq LastPt (list 0 0 0))
	(setq CurrX 0)
	(while (< CurrX pi)
		(setq CurrXEval (BetaFunctionToFindRootOf CurrX))
		(if CurrXEval
			(progn
				(command ".line" LastPt (list CurrX CurrXEval 0) "")
				(setq LastPt (getvar "lastpoint"))
			) ; progn
		) ; if
		(setq CurrX (+ CurrX 0.02))
	) ; while
) ; defun


(defun FindBetaAndEtaForAlpha (Alpha)
	(setq GLOBALALPHA Alpha)
	(setq Beta (NewtRap 'BetaFunctionToFindRootOf 0.3589))
	(setq Eta (EtaFunction Alpha Beta))
	(list Beta Eta)
) ; defun


(defun BetaFunctionToFindRootOf (Beta / tmp3)
	(setq tmp3 (EtaFunctionWithConstantGLOBALALPHA Beta))
	(if tmp3
		(- (* Beta 2) (/ pi 2) tmp3)
		nil
	) ; if
) ; defun

(defun EtaFunctionWithConstantGLOBALALPHA (Beta)
	(EtaFunction GLOBALALPHA Beta)
) ; setq


(defun EtaFunction (Alpha Beta / a b c)
	(setq a (cos Alpha))
	(setq b (/ a (cos Beta)))
	(setq c (Sin Alpha))
	(InvSin (/ c b))
			
) ; setq


(defun c:PointLabeler ()
	(setq Prefix (getstring "\nEnter point prefix: "))
	(setq StartNum (getint "\nEnter start number: "))
	(while t
		(command ".text" "j" "mc" (list 0 0 0) 30 (strcat Prefix (atoi CurrNum)) "")
		(command ".move" (entlast) "" (list 0 0 0) PAUSE )
		(setq Currnum (1+ CurrNum))
	) ; while
					
) ; defun

;;;	(setq BearingDesignRadius 1773.5)
;;;	(setq SlideGap 160)
;;;	(setq FitGap 160)
;;;	(setq BearingGap1 95)
;;;	(setq BearingGap2 95)
;;;	(setq BearingThickness 3165.0)
;;;	(setq InnerRadius 0.1)
;;;	(setq OuterRadius 3498)
;;;	(setq RollerFilletRadius 55)


(Defun MirrorVecAtYCoord (Vec YCoord)
	(List
		(nth 0 Vec)
		(+ YCoord (- YCoord (nth 1 Vec)))
		(nth 2 Vec)
	) ; list
) ; defun

(defun c:PBreak ()
	(setq Ent (entsel "\nPick entity to break at one point: "))
	(setq pt (getpoint "\nPick the point to break it at: "))
	(command ".break" Ent "f" pt pt)
) ; defun


(defun MidPoint (V1 V2)
	(VMult (VAdd V1 V2) 0.5)
) ; defun


(defun DrawBearing ( / RetVal)

		(setq bOnePieceOuterRace t)
	(setq bOnePieceInnerRace f)


	(command ".point" (list 0 0 0))
	(setq Marker (entlast))
	(setvar "facetres" 1)

	; Parameters must be initialized at this point

	(ClearUserInput)
	
	; Find the angles needed
	(setq BetaAndEta (FindBetaAndEtaForAlpha (/ pi NumOfRollerPairs)))
	(setq Beta (nth 0 BetaAndEta))
	(setq Eta (nth 1 BetaAndEta))
	(setq BearingALeftAngle (+ Beta Eta))
	(setq BearingARightAngle (- Beta Eta))
	(setq BearingBLeftAngle (- 0 BearingALeftAngle))
	(setq BearingBRightAngle (- 0 BearingARightAngle))
	; Now define the shapes of the rollers
	(setq P0 (list 0 0 0))
	(setq P1 (polar P0 BearingARightAngle 1))
	(setq YCoordOfMirror (nth 1 P1))
	(setq P2 (MirrorVecAtYCoord P0 YCoordOfMirror))
	(setq P3 (polar P0 BearingALeftAngle 1))
	(setq P4 (polar P2 (- 0 Beta Eta) 1))
	(setq P5 (inters P0 P3 P2 P4 nil))
	(setq P6 (inters P5 (polar P5 BearingBRightAngle 1) P0 (polar P0 BearingARightAngle 1) nil))

	;(command ".Pline" P5 P6 P1 P3 "c")

	(setq P7 (MirrorVecAtYCoord P6 YCoordOfMirror))
	(setq P8 (MidPoint P1 P3))

	;(command ".Pline" P7 P5 P4 P1 "c")

	(setq NondimensionalizedBearingRadius (nth 0 P8))
	(setq Multiplier (/ BearingDesignRadius NondimensionalizedBearingRadius))
;;;	(setq NonDimensionalizedRollerPoints
;;;		(3Sp-4Sp (list P0 P1 P2 P3 P4 P5 P6 P7 P8))
;;;	) ; setq
	(setq P0 (VMult P0 Multiplier))
	(setq P1 (VMult P1 Multiplier))
	(setq P2 (VMult P2 Multiplier))
	(setq P3 (VMult P3 Multiplier))
	(setq P4 (VMult P4 Multiplier))
	(setq P5 (VMult P5 Multiplier))
	(setq P6 (VMult P6 Multiplier))
	(setq P7 (VMult P7 Multiplier))
	(setq P8 (VMult P8 Multiplier))
	(setq YCoordOfMirror (* YCoordOfMirror Multiplier))
	
	(setq ARightAngPerp (- BearingARightAngle (/ pi 2)))
	;(setq RollerPoints (Scale4SpVectors NonDimensionalizedRollerPoints Multiplier))


	(setq PA (polar P1 ARightAngPerp SlideGap))
	(setq PB (polar P6 ARightAngPerp SlideGap))
	(setq PC (MirrorVecAtYCoord PA YCoordOfMirror))
	(setq PD (MirrorVecAtYCoord PB YCoordOfMirror))
	(setq P9 (inters PA PB PC PD nil))
	
	(setq ALeftAngPerp (+ BearingALeftAngle (/ pi 2)))
	(setq BLeftAngPerp (- BearingBLeftAngle (/ pi 2)))


	(setq PE (polar P3 ALeftAngPerp SlideGap))
	(setq PF (polar P5 BLeftAngPerp SlideGap))
	(setq PG (MirrorVecAtYCoord PF YCoordOfMirror))
	(setq PH (MirrorVecAtYCoord PE YCoordOfMirror))
	(setq PJ (inters PE PF PG PH nil))
	(setq PK (inters
			PJ (VAdd PJ (list -1 0 0))
			(list InnerRadius 0 0) (list InnerRadius 1 0)
			nil
			) ; inters
	) ; setq
	(setq P10 (polar PK (- (/ pi 2)) (/ FitGap 2.0))) 
	(setq P11 (MirrorVecAtYCoord P10 YCoordOfMirror))
	(setq P12 (inters P10 (VAdd P10 (list 1 0 0)) PJ PH nil)) 
	(setq P13 (MirrorVecAtYCoord P12 YCoordOfMirror))
	(setq P14 (polar PK (- (/ pi 2)) (/ BearingThickness 2.0))) 
	(setq P15 (MirrorVecAtYCoord P14 YCoordOfMirror))
	(setq PL (inters P14 (VAdd P14 (list 1 0 0)) P4 (VAdd P4 (list 0 -1 0)) nil)) 
	(setq P16 (VAdd PL (list (- BearingGap1) 0 0)))
	(setq P17 (MirrorVecAtYCoord P16 YCoordOfMirror))
	(setq P18 (VAdd PL (list BearingGap2 0 0)))
	(setq P19 (MirrorVecAtYCoord P18 YCoordOfMirror))
	(setq P20 (inters P14 (VAdd P14 (list 1 0 0))
			(list OuterRadius 0 0) (list OuterRadius 1 0)
			nil
			) ; inters
	) ; setq
	(setq P21 (MirrorVecAtYCoord P20 YCoordOfMirror))
	(setq P22 (inters PA PB P18 (VAdd P18 (list 0 1 0))	nil))
	(setq P23 (MirrorVecAtYCoord P22 YCoordOfMirror))
	(setq P24 (inters PG PH P16 (VAdd P16 (list 0 1 0))	nil))
	(setq P25 (MirrorVecAtYCoord P24 YCoordOfMirror))
	(setq P26 (MirrorVecAtYCoord P8 YCoordOfMirror))
	(setq P27 (inters P5 P6 P8 P0	nil))
	(setq P28 (MirrorVecAtYCoord P27 YCoordOfMirror))
	(setq PM (VAdd P20 (list 0 (/ BearingThickness 2.0) 0)))
	(setq P29 (VAdd PM (list 0 (/ FitGap -2.0) 0)))
	(setq P30 (inters P29 (VAdd P29 (list -1 0 0))  P22 PA nil))
	; Now draw the points

	; First the rollers
	(command ".Pline" P28 P5 P4 P26 "")
	(command ".fillet" "r" RollerFilletRadius ".fillet" "p" (entlast))
	(command ".Revolve" (entlast) "" P28 P26 "")
	(setq Roller1Ent (entlast))
	(command ".change" Roller1Ent "" "p" "c" 5 "")
	(command ".Pline" P27 P6 P1 P8 "")
	(command ".fillet" "r" RollerFilletRadius ".fillet" "p" (entlast))
	(command ".Revolve" (entlast) "" P8 P27 "")
	(setq Roller2Ent (entlast))
	(command ".change" Roller2Ent "" "p" "c" 5 "")

	; Now the inner race
	(if bOnePieceInnerRace
		(progn
			(command ".Pline" P14 P16 P24 PJ P25 P17 P15 "c")
			(command ".Revolve" (entlast) "" (list 0 0 0) (list 0 1 0) "")
			(command ".change" (entlast) "" "p" "c" 1 "Tr" RaceTranparency "")
			(setq InnerRace (ssadd (entlast)))
		) ; progn
		(progn ; 2-piece inner race
			(command ".Pline" P14 P16 P24 P12 P10 "c")
			(command ".Revolve" (entlast) "" (list 0 0 0) (list 0 1 0) "")
			(command ".change" (entlast) "" "p" "c" 1 "Tr" RaceTranparency "")
			(setq InnerRace (ssadd (entlast)))

			(command ".Pline" P11 P13 P25 P17 P15 "c")
			(command ".Revolve" (entlast) "" (list 0 0 0) (list 0 1 0) "")
			(command ".change" (entlast) "" "p" "c" 1 "Tr" RaceTranparency "")
			(setq InnerRace (ssadd (entlast) InnerRace))
		) ; progn
	) ; if

	(if bOnePieceOuterRace
		(progn
			(command ".Pline" P18 P20 P21 P19 P23 P9 P22 "c")
			(command ".Revolve" (entlast) "" (list 0 0 0) (list 0 1 0) "")
			(setq OuterRace (ssadd (entlast)))
			(command ".change" OuterRace "" "p" "c" 3 "Tr" RaceTranparency "")

		) ; progn
		(progn ; 2-piece Outer race
			(command ".Pline" P18 P20 P29 P30 P22 "c")
			(command ".Revolve" (entlast) "" (list 0 0 0) (list 0 1 0) "")
			(setq OuterRace (ssadd (entlast)))
			(command ".change" OuterRace "" "p" "c" 3 "Tr" RaceTranparency "")
			(command ".mirror" OuterRace "" P1 PM "n")
			(setq OuterRace (ssadd (entlast) OuterRace))
		) ; progn
	) ; if

	(command ".move" Roller1Ent Roller2Ent InnerRace OuterRace ""
		(MidPoint P0 P2)
		(list 0 0 0)
	) ; command
	(c:UY)
	(command ".rotate" "p" "" (list 0 0 0) 90)
	(c:UP)
	(command ".array" Roller1Ent "" "p" (list 0 0 0) (/ NumOfRollerPairs 2.0) "" "")
	(command ".rotate" Roller2Ent "" (list 0 0 0) (/ 360.0 NumOfRollerPairs))
	(command ".array" Roller2Ent "" "p" (list 0 0 0) (/ NumOfRollerPairs 2.0) "" "")
	(SetUserInput)

	(setq RetVal (ssadd))
	(setq CurrDrawnEnt Marker)
	(while (setq CurrDrawnEnt (entnext CurrDrawnEnt))
		(ssadd CurrDrawnEnt RetVal)
	) ; while
	(entdel marker)
	RetVal
) ; defun


(defun GetDictionaryRealValue (Key)
	(atof (vlax-ldata-get SLEWBEARINGDICTIONARY Key))
) ; defun



(defun SetDictionaryRealValue (Key Value)
	(vlax-ldata-put SLEWBEARINGDICTIONARY Key (rtos Value))
) ; defun


; ******* Dialog Section ********

(defun GetParametersFromSlewBearingDictionary ()
	(setq NumOfRollerPairs (GetDictionaryRealValue "NumOfRollerPairs"))
	(setq BearingDesignRadius (GetDictionaryRealValue "BearingDesignRadius"))
	(setq BearingThickness (GetDictionaryRealValue "BearingThickness"))
	(setq InnerRadius (GetDictionaryRealValue "InnerRadius"))
	(setq OuterRadius (GetDictionaryRealValue "OuterRadius"))
	;(setq bOnePieceOuterRace f)
	;(setq bOnePieceInnerRace t)
	(setq RollerFilletRadius (GetDictionaryRealValue "RollerFilletRadius"))
	(setq BearingGap1 (GetDictionaryRealValue "BearingGap1"))
	(setq BearingGap2 (GetDictionaryRealValue "BearingGap2"))
	;(setq SlideGap 0.3)
	;(setq FitGap 0.1)
	;(setq RaceTranparency 50)
) ; defun


(defun SetParametersToSlewBearingDictionary ()
	(SetDictionaryRealValue "NumOfRollerPairs" NumOfRollerPairs)
	(SetDictionaryRealValue "BearingDesignRadius" BearingDesignRadius)
	(SetDictionaryRealValue "BearingThickness" BearingThickness)
	(SetDictionaryRealValue "InnerRadius" InnerRadius)
	(SetDictionaryRealValue "OuterRadius" OuterRadius)
	;(setq bOnePieceOuterRace f)
	;(setq bOnePieceInnerRace t)
	(SetDictionaryRealValue "RollerFilletRadius" RollerFilletRadius)
	(SetDictionaryRealValue "RollerFilletRadius" RollerFilletRadius)
	(SetDictionaryRealValue "RollerFilletRadius" RollerFilletRadius)
	;(setq SlideGap 0.3)
	;(setq FitGap 0.1)
	;(setq RaceTranparency 50)
) ; defun


(defun SetParametersToDefaults ()
	(setq NumOfRollerPairs 40)
	(setq BearingDesignRadius 70)
	(setq BearingThickness 20)
	(setq InnerRadius 51)
	(setq OuterRadius 83)
	(setq bOnePieceOuterRace f)
	(setq bOnePieceInnerRace t)
	(setq RollerFilletRadius 1.5)
	(setq BearingGap1 1)
	(setq BearingGap2 3)
	(setq SlideGap 0.3)
	(setq FitGap 0.1)
	(setq RaceTranparency 50)
) ; defun



(defun StartBearingDialog ()
  (if (not (new_dialog "SlewBearingDialog" dcl_id "" DialogScreenPosition))
    (exit)
  ) ; if
	(if (= nil NumOfRollerPairs)
		(if (vlax-ldata-get SLEWBEARINGDICTIONARY "NumOfRollerPairs")
			(GetParametersFromSlewBearingDictionary)
			(SetParametersToDefaults)
		) ; if
	) ; if
	(set_tile "txtNumRollerPairs" (rtos NumOfRollerPairs))
	(set_tile "txtBearingDesignRadius" (rtos BearingDesignRadius))
	(set_tile "txtBearingThickness" (rtos BearingThickness))
	(set_tile "txtInnerRadius" (rtos InnerRadius))
	(set_tile "txtOuterRadius" (rtos OuterRadius))
	(set_tile "txtRollerFilletRadius" (rtos RollerFilletRadius))
	(set_tile "txtBearingGap1" (rtos BearingGap1))
	(set_tile "txtBearingGap2" (rtos BearingGap2))
	;(action_tile "btnDraw"
  (start_dialog)
		
) ; defun


(defun c:BearingDialog ( / dcl_id )
	(if (= DialogScreenPosition nil) (setq DialogScreenPosition (list -1 -1)))
  (setq dcl_id (load_dialog "Slew Bearing Design.dcl"))
	(While (= (setq DlgRetVal (StartBearingDialog)) 1)
		(progn
			;(TransferParametersFromDialog)
	 		(if LastDrawnBearingEntities
				(command ".erase" LastDrawnBearingEntities "")
			); if
			(SetParametersToSlewBearingDictionary)
			(setq LastDrawnBearingEntities (DrawBearing))
		) ; progn
	) ; while
	(if (= DlgRetVal 0)
		(progn
			;(TransferParametersFromDialog)
			(SetParametersToSlewBearingDictionary)
			;(setq LastDrawnBearingEntities (DrawBearing))
			(unload_dialog dcl_id)
		) ; progn
	) ; if

 (princ)
) ; defun


(defun TransferParametersFromDialog ()
	(setq NumOfRollerPairs (atof (get_tile "txtNumRollerPairs")))
	(setq BearingDesignRadius (atof (get_tile "txtBearingDesignRadius")))
	(setq BearingThickness (atof (get_tile "txtBearingThickness")))
	(setq InnerRadius (atof (get_tile "txtInnerRadius")))
	(setq OuterRadius (atof (get_tile "txtOuterRadius")))
	(setq RollerFilletRadius (atof (get_tile "txtRollerFilletRadius")))
	(setq BearingGap1 (atof (get_tile "txtBearingGap1")))
	(setq BearingGap2 (atof (get_tile "txtBearingGap2")))

) ; defun

(defun btnDrawPressed ()

	; check data:
	(cond
		((< (atof (get_tile "txtNumRollerPairs")) 9)
		 (msgbox "\nNumber of Rollers out of range")
		)
		((<= (atof (get_tile "txtBearingDesignRadius")) 0)
		 (msgbox "\nBearing Design Radius out of range")
		)
		((<= (atof (get_tile "txtBearingThickness")) 0)
		 (msgbox "\nBearing Thickness out of range")
		)
		((<= (atof (get_tile "txtInnerRadius")) 0)
		 (msgbox "\nInner Radius out of range")
		)
		((<= (atof (get_tile "txtOuterRadius")) 0)
		 (msgbox "\nOuter Radius out of range")
		)
		((<= (atof (get_tile "txtRollerFilletRadius")) 0)
		 (msgbox "\nRoller Fillet Radius out of range")
		)
		((<= (atof (get_tile "txtBearingGap1")) 0)
		 (msgbox "\nBearing Gap 1 out of range")
		)
		((<= (atof (get_tile "txtBearingGap2")) 0)
		 (msgbox "\nBearing Gap 2 out of range")
		)
		(t
			(TransferParametersFromDialog)
			(setq DialogScreenPosition (done_dialog 1))
		)
	) ; cond
) ; defun


(defun btnCancelPressed ()
	(TransferParametersFromDialog)
	(setq DialogScreenPosition (done_dialog 0))
	
) ; defun

; ******* END Dialog Section ********
