(ns optcase.core
  (:gen-class :main true)
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [clojure.math.numeric-tower :refer :all]
            [optcase.attachwithvectors :refer :all]))


(spit "things/post-demo.scad" ;cleans file
     nil )

;;;;;;;;;;;;
;GETTING THE PLATE MATRIX SET UP
;;;;;;;;;;;;

(def keywidthForSpacing 	14.4)
(def keySpacing 			5.05)
(def arrXWid				8 )
(def arrYLen				5 )

(defn createarray [x y] ;x is across, y is down
	(vec(for [ycoin (range y)]
		(vec (for [xcoin (range x)]
			{:xcoord xcoin, 
			 :ycoord ycoin,
			 :cpntPos [ (* xcoin (+ keySpacing keywidthForSpacing)) (* ycoin (+ keySpacing keywidthForSpacing)) 0], 
			 :cpntVec [0 0 1],
			 :cpntAng 0}
			)
		))
	))


(defn retr [arr x y]
	((arr y) x)
)



(defn moveonXYZ  [arr xmove ymove zmove columnorrow number]
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData)
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				correct 	(case columnorrow
								:row ycoin
								:col xcoin)
				]
				

				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos (if (= correct number) 
				 				[ (+ (cpntP 0) xmove) (+ (cpntP 1) ymove) (+ (cpntP 2) zmove)]
				 				cpntP)
				 :cpntVec cpntV,
				 :cpntAng cpntA}
				

				)

			)
		))
	)
	)



;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;
(def plate-thickness 4)
(def dsa-length 18.25)
(def dsa-double-length 37.5)
(def heightbaseofkeycap 6.1 )
(def dsa-cap 
	{1 (let [bl2 (/ 18.5 2)
                     m (/ 18 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 1.9]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 7.9])))]
                 (->> key-cap
                      (translate [0 0 heightbaseofkeycap])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ dsa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

(defn showkeycaps [arr]
		(for [ycoin (range arrYLen) xcoin (range arrXWid)]
			(let [
			pntData 	(retr arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
				]
			(when (pntData :existence)
				(attach [cpntP cpntV cpntA]
						[[0 0 0] [0 0 1] 0]
						(union (dsa-cap 1) keyswitch)
					))
)))

(defn showconnectors [arr]
		(for [ycoin (range arrYLen) xcoin (range arrXWid)]
			(let [
			pntData 	(retr arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
				]

			(connector [cpntP cpntV cpntA]
				)
	)))

;
;Making web
;

(def leftedgepadding 3)
(def rightedgepadding 3)
(def topedgepadding 3)
(def bottedgepadding 3)

(def mount-hole-width 14)
(def mount-hole-height 14)


(def web-thickness plate-thickness )
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (/ web-thickness -2)])))
(def edgepost (scale [1 1 3] web-post))


(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-hole-width  2) post-adj) (- (/ mount-hole-height  2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-hole-width -2) post-adj) (- (/ mount-hole-height  2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-hole-width -2) post-adj) (+ (/ mount-hole-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-hole-width  2) post-adj) (+ (/ mount-hole-height -2) post-adj) 0] web-post))

(def web-post-edge-tr (translate [(- (/ mount-hole-width  2) post-adj) (- (/ mount-hole-height  2) post-adj) 0] edgepost))
(def web-post-edge-tl (translate [(+ (/ mount-hole-width -2) post-adj) (- (/ mount-hole-height  2) post-adj) 0] edgepost))
(def web-post-edge-bl (translate [(+ (/ mount-hole-width -2) post-adj) (+ (/ mount-hole-height -2) post-adj) 0] edgepost))
(def web-post-edge-br (translate [(- (/ mount-hole-width  2) post-adj) (+ (/ mount-hole-height -2) post-adj) 0] edgepost))

(defn triangle-hulls [& shapes]
	"TBH I didn't write this. Adereth did. Its just a nice hulling function that makes 
	multiple hulls instead of one big hull. I guess hulls in sets of three shapes as 
	three points will always form a plane. This way the hulls will always be planes (flat)"
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))



(defn dual-hull [shapes]
	;(prn (partition 2 1 shapes))

	(apply union
		(map (partial hull)
			 (partition 2 1 shapes))))


(defn getcolPntData [arr x y]
	"Similar to getrowPntData but this deals with x. See getrowPntData and smartretrPntData"
	(cond 
		(= x arrXWid)
			(retr arr (dec x) y)
		(= x -1)
			(retr arr (inc x) y)
		(< x -1)
			(retr arr 0 y)
		:else
			(retr arr x y)
		)
	)

(defn getrowPntData [arr x y]
	"Retrieves the pntData at x y in arr and makes sure that y is in the correct range. See smartretrPntData 
	for what it does when y is out of bounds. This only deals with y as it gets getcolPntData to deal with x."
	(cond 
		(= y arrYLen)
			(getcolPntData arr x (dec y))
		(= y -1)
			(getcolPntData arr x (inc y))
		(< y -1)
			(getcolPntData arr x 0)
		:else 
			(getcolPntData arr x y) 
			)

	)

(defn smartretrPntData [arr x y]
	"This is used to make the edges of the plate. Because the webbing loops through -1
	to arrXWid (or arrYLen), you need to be careful not to go outside of the array.
	this function catches when you are at -1 or arrYLen, and returns the closest good position
	+ edge padding. For instance if you call the array with arrXWid, this will find arrXWid - 1,
	then it will return arrXWid -1 with an updated x pos. This update will include edge padding.

	getrowPntData is used because inside of getrowPntData it calls getcolPntData. You can't also 
	call getrowPntData inside getcolPntData because then you will get an infinite loop."
	
	(getrowPntData arr x y))

(defmacro make-fn [m] 
	"This is used to turn macros like and / or into fn"
  `(fn [& args#] 
    (eval `(~'~m ~@args#))))

(defn putupapost [arr xcoin ycoin pos callingfrom makingwhat callingto buildedgesornot plateorbase]
	(let [
		pntData (retrforbase arr xcoin ycoin callingfrom callingto makingwhat)
		;pntData  (smartretrPntData arr xcoin ycoin)
		cpntP 		(:cpntPos pntData)
		cpntV 		(:cpntVec pntData)
		cpntA 		(:cpntAng pntData)
		exists		(:existence pntData)

		xtrans (cond 
				(= xcoin -1)
					;(- 0 leftedgepadding mount-hole-width (if (= plateorbase :base) -2 0))
					(- keySpacing leftedgepadding)
					;0
				(= xcoin arrXWid)
					;(+ rightedgepadding mount-hole-width (if (= plateorbase :base) -2 0))
					(- rightedgepadding keySpacing)
					;0
				(= exists false)
					(cond 
						(and (= :callfromthisone callingfrom) (or (= :makingrows makingwhat)))
							(- keySpacing leftedgepadding (if (= plateorbase :base) -2 0))

						(and (= :callfromthisone callingfrom) (or (= :makingdiag makingwhat)))
							(- keySpacing leftedgepadding (if (= plateorbase :base) -2 0))
						
						(and (= :callfromleft callingfrom) (or (= :makingrows makingwhat)))
							(- rightedgepadding keySpacing (if (= plateorbase :base) 2 0))

						(and (= :callfromleft callingfrom) (or (= :makingdiag makingwhat)))
							(- rightedgepadding keySpacing (if (= plateorbase :base) 2 0))

						(and (= :makingcolumns makingwhat) (or (= :bl pos) (= :tl pos)))
							(- rightedgepadding keySpacing (if (= plateorbase :base) 2 0))

						(and (= :makingcolumns makingwhat) (or (= :br pos) (= :tr pos)))
							(- keySpacing leftedgepadding (if (= plateorbase :base) 2 0))

						(and (= :callfromleftbelow callingfrom) (= :makingdiag makingwhat))
							(- rightedgepadding keySpacing (if (= plateorbase :base) 2 0))

						(and (= :callfrombelow callingfrom) (= :makingdiag makingwhat))
							(- keySpacing leftedgepadding (if (= plateorbase :base) -2 0))

						:else
							0
						)

					
				:else
					0
				)
		ytrans (cond 
				(= ycoin -1) 
					;(- 0 bottedgepadding mount-hole-height (if (= plateorbase :base) -2 0))
					;bottedgepadding
					(- keySpacing bottedgepadding)
				(= ycoin arrYLen) 
					;(+ topedgepadding mount-hole-height  (if (= plateorbase :base) -2 0))
					;topedgepadding
					(- topedgepadding keySpacing)
					;0
				(= exists false)
					(cond 
						(and (= :callfromthisone callingfrom) (or (= :makingdiag makingwhat)))
							(- keySpacing topedgepadding (if (= plateorbase :base) 2 0))

						(and (= :callfromthisone callingfrom) (or (= :makingcolumns makingwhat)))
							(- keySpacing topedgepadding (if (= plateorbase :base) -2 0))

						(and (= :callfrombelow callingfrom) (or (= :makingcolumns makingwhat)))
							(- bottedgepadding keySpacing (if (= plateorbase :base) 2 0))

						(and (= :callfrombelow callingfrom) (or (= :makingdiag makingwhat)))
							(- bottedgepadding keySpacing (if (= plateorbase :base) -2 0))

						(and (= :makingrows makingwhat) (or (= :tr pos) (= :tl pos)))
							(- keySpacing topedgepadding (if (= plateorbase :base) 2 0))

						(and (= :makingrows makingwhat) (or (= :br pos) (= :bl pos)))
							(- bottedgepadding keySpacing (if (= plateorbase :base) -2 0))

						(and (= :callfromleftbelow callingfrom) (= :makingdiag makingwhat))
							(- bottedgepadding keySpacing (if (= plateorbase :base) 2 0))

						(and (= :callfromleft callingfrom) (= :makingdiag makingwhat))
							(- keySpacing topedgepadding (if (= plateorbase :base) 2 0))

						:else
							0
						)
				:else
					0
				)

		ztrans (if (= plateorbase :base)
					(+ -12 plate-thickness)
					;-12
					0
			)

		edge    ;this determines if the post should be an edge post or not.
				;This is easy if x or y is -1 or arrxwid or arrylen.
				;also easy if x or y is 0 and being called from the left or below. 
				;Remember that the webbing starts with one key and then awakens keys to the right or above it.
				;A littl trickier is if x or y is one less than their max. They have to be making columns or rows 
				;or diags and being called from either this one or from a suitable thing.
				;For instance, diagonals being called from below shouldn't be edged because this would create edge posts one key in from the actual edge.
				;Then, if any key in the currently forming web should be non existent, there needs to be an edge post.
				;For example, |n|c|, n is nonexistent, c is current key. That current key needs to look to the left and see that key exists. It doesn't, so put an edge post up.)
				;Also |c|n| would need to check to the right.
				;This is only two combinations for either row or column, but for diagonals, 4 different keys and each one needs to check 3 keys.
				 
				(or 
				    (= xcoin -1 ) 
					(= ycoin -1 ) 
					(= xcoin arrXWid) 
					(= ycoin arrYLen) 
					(and (= xcoin 0) (= callingfrom :callfromleft))
					(and (= xcoin 0) (= callingfrom :callfromleftbelow ))
					(and (= ycoin 0) (= callingfrom :callfrombelow))
					(and (= ycoin 0) (= callingfrom :callfromleftbelow ))

					(and (= ycoin (dec arrYLen)) (or    (and (= makingwhat :makingcolumns) (= callingfrom :callfromthisone))
														(and (= makingwhat :makingdiag) ( or (= callingfrom :callfromthisone) (= callingfrom :callfromleft)))
														))
					(and (= xcoin (dec arrXWid)) (or    (and (= makingwhat :makingrows) (= callingfrom :callfromthisone))
														(and (= makingwhat :makingdiag) (= callingfrom :callfromthisone))
														(and (= makingwhat :makingdiag) (= callingfrom :callfrombelow))))

					(let [
					neighbours 	(cond
									(= makingwhat :makingcolumns)
										(cond
											(= callingfrom :callfromthisone)
												[(smartretrPntData arr xcoin ycoin) (smartretrPntData arr xcoin (inc ycoin))]
											(= callingfrom :callfrombelow)
												[(smartretrPntData arr xcoin (dec ycoin)) (smartretrPntData arr xcoin ycoin)]
											)
									(= makingwhat :makingrows)
										(cond
											(= callingfrom :callfromthisone)
												[(smartretrPntData arr xcoin ycoin) (smartretrPntData arr (inc xcoin) ycoin)]
											(= callingfrom :callfromleft)
												[(smartretrPntData arr (dec xcoin) ycoin) (smartretrPntData arr xcoin ycoin)]
											)
									(= makingwhat :makingdiag)
										(cond
											(= callingfrom :callfromthisone)
												[(smartretrPntData arr xcoin ycoin)
												 (smartretrPntData arr (inc xcoin) ycoin)
												 (smartretrPntData arr xcoin (inc ycoin))
												 (smartretrPntData arr (inc xcoin) (inc ycoin))]
											(= callingfrom :callfromleft)
												[(smartretrPntData arr (dec xcoin) ycoin)
												 (smartretrPntData arr xcoin ycoin)
												 (smartretrPntData arr (dec xcoin) (inc ycoin))
												 (smartretrPntData arr xcoin (inc ycoin))]
											(= callingfrom :callfrombelow)
												[(smartretrPntData arr xcoin (dec ycoin))
												 (smartretrPntData arr (inc xcoin) (dec ycoin))
												 (smartretrPntData arr xcoin ycoin)
												 (smartretrPntData arr (inc xcoin) ycoin)]
											(= callingfrom :callfromleftbelow)
												[(smartretrPntData arr (dec xcoin) (dec ycoin))
												 (smartretrPntData arr xcoin (dec ycoin))
												 (smartretrPntData arr (dec xcoin) ycoin)
												 (smartretrPntData arr xcoin ycoin)]
											)
								)
						]
						
						(->>(map #(get %1 :existence) neighbours) 
						(map (make-fn not))
						(apply (make-fn or))
							)))
		
		; xtrans 		(if (and edge (= plateorbase :base))
		; 				(cond 
		; 					(and (or (= pos :tr) (= pos :br)))
		; 						(+ xtrans 2)
		; 					(and (or (= pos :tl) (= pos :bl)))
		; 						(- xtrans 2)
		; 					:else
		; 						xtrans
		; 					)
		; 				xtrans)

		; ytrans 		(if (and edge (= plateorbase :base))
		; 				(cond 
		; 					(or (= pos :tl) (= pos :tr))
		; 						(+ ytrans 2)
		; 					(or (= pos :br) (= pos :bl))
		; 						(- ytrans 2)
		; 					)
		; 				ytrans)
						
		post (cond 
				(and buildedgesornot edge) 
					(cond
						(= pos :tl) (partial web-post-edge-tl)
						(= pos :bl) (partial web-post-edge-bl)
						(= pos :tr) (partial web-post-edge-tr)
						(= pos :br) (partial web-post-edge-br)
						) 
				:else
					(cond
						(= pos :tl) (partial web-post-tl)
						(= pos :bl) (partial web-post-bl)
						(= pos :tr) (partial web-post-tr)
						(= pos :br) (partial web-post-br)
						)
					)
		]


		(attach 
			[cpntP cpntV cpntA]
			[[0 0 0] [0 0 1] 0]
			(translate [xtrans ytrans ztrans] post)
			))
		)

(defn neigbhourtoexistence? [arr xcoin ycoin buildingwhat]

	(let [existenceofthisone 			( (smartretrPntData arr xcoin ycoin) :existence)
		  existenceofnextrow 			( (smartretrPntData arr xcoin (inc ycoin)) :existence)
		  existenceofnextcol 			( (smartretrPntData arr (inc xcoin) ycoin) :existence)
		  existenceofnextcolandrow		( (smartretrPntData arr (inc xcoin) (inc ycoin)) :existence)]
		  (cond 
			(= buildingwhat :buildingcolumnconnects)
				(or existenceofthisone existenceofnextrow)
			(= buildingwhat :buildingrowsconnects)
				(or existenceofthisone existenceofnextcol)
			(= buildingwhat :buildingdiagonalsconnects)
				(or existenceofthisone existenceofnextcolandrow existenceofnextrow existenceofnextcol)
			)
		))

(defn makeconnectors [arr plateorbase] 
	"Creates posts at the corner of each key switch and hulls them with the posts on other keycaps.
	The edges and corners are created by going one less than the array and one more than the array.
	When the variable is out of bounds, it is caught in the smartretrPntData. If the key doesn't exist 
	it might still be used because it is involved in making and edge or corner for a key that does exist.
	This is dealt with in neigbhourtoexistence."
	(let [

		buildedges 			(cond
								(= plateorbase :plate)
								 	true
								(= plateorbase :base)
									false
							)
		]
	(apply union
		(concat
			;Row connectors
			(for [ycoin (range arrYLen)]
				(color [1 (rand) 1 1] 
				(for  [xcoin (range -1  arrXWid)]
				(when (neigbhourtoexistence? arr xcoin ycoin :buildingrowsconnects)
					(triangle-hulls
						(putupapost arr xcoin       ycoin :tr :callfromthisone :makingrows :here buildedges plateorbase)
						(putupapost arr xcoin       ycoin :br :callfromthisone :makingrows :here buildedges plateorbase)
						(putupapost arr (inc xcoin) ycoin :tl :callfromleft    :makingrows :right buildedges plateorbase)
						(putupapost arr (inc xcoin) ycoin :bl :callfromleft    :makingrows :right buildedges plateorbase)
						)
					))))

			;Columns connectors
			(for [ycoin (range -1 arrYLen)]
				(color [(rand) 1 1 1] 
				(for [xcoin (range arrXWid)]
				(when (neigbhourtoexistence? arr xcoin ycoin :buildingcolumnconnects) 
					(triangle-hulls
						(putupapost arr xcoin       ycoin :tr :callfromthisone :makingcolumns :here buildedges plateorbase)
						(putupapost arr xcoin (inc ycoin) :br :callfrombelow   :makingcolumns :above buildedges plateorbase)
						(putupapost arr xcoin       ycoin :tl :callfromthisone :makingcolumns :here buildedges plateorbase)
						(putupapost arr xcoin (inc ycoin) :bl :callfrombelow   :makingcolumns :above buildedges plateorbase)
						)
					))))

			;Diagonal connectors
			(for [ycoin (range -1 arrYLen)]
				(color [0.2 0.2 (rand) 1] 
				(for [xcoin (range -1 arrXWid)]
				(when (neigbhourtoexistence? arr xcoin ycoin :buildingdiagonalsconnects)
					(triangle-hulls
						(putupapost arr xcoin       ycoin       :tr :callfromthisone :makingdiag :here buildedges plateorbase)
						(putupapost arr xcoin       (inc ycoin) :br :callfrombelow   :makingdiag :above buildedges plateorbase)
						(putupapost arr (inc xcoin) ycoin       :tl :callfromleft    :makingdiag :right buildedges plateorbase)
						(putupapost arr (inc xcoin) (inc ycoin) :bl :callfromleftbelow :makingdiag :aboveleft buildedges plateorbase)
						)
					))))

			;key connector for base
			(when (= plateorbase :base)
				(for [ycoin (range arrYLen)]
				(color [0.2 0.2 (rand) 1] 
				(for [xcoin (range arrXWid)]
				(when ((retr arr xcoin ycoin) :existence)
					(hull
						(putupapost arr xcoin ycoin :tr :callfromthisone :makingkeycentre :here buildedges plateorbase)
						(putupapost arr xcoin ycoin :br :callfromthisone :makingkeycentre :here buildedges plateorbase)
						(putupapost arr xcoin ycoin :tl :callfromthisone :makingkeycentre :here buildedges plateorbase)
						(putupapost arr xcoin ycoin :bl :callfromthisone :makingkeycentre :here buildedges plateorbase)
						)
					))))
				)

			))))

(defn attachpoint [[pos vect ang] [x y z]]
	"Like the attach module except this returns the attached coordinates of a point instead of an attached shape.
	this is useful when you want to get the attached coordinates for things like polyhedron."
	(let [ 
		 
	 	u 				(unitv vect)
	 	a 				(u 0)
	 	b 				(u 1)
	 	c 				(u 2)
	 	d 				(modofvec [0 b c])

	 	ConD 			(/ c d)
	 	BonD 			(/ b d)
		

	 	yaxisinv    	[
	 					 (+ (* x d) (* z a))
	 					 y
	 					 (- (* z d) (* x a))
	 					]

	 	xaxisinv   		[
	 					 (yaxisinv 0)
	 					 (+ (* (yaxisinv 1) ConD) (* (yaxisinv 2) BonD))
	 					 (- (* (yaxisinv 2) ConD) (* (yaxisinv 1) BonD))
	 					]
        
	 	finalpos		[
	 					(+ (xaxisinv 0) (pos 0))
	 					(+ (xaxisinv 1) (pos 1))
	 					(+ (xaxisinv 2) (pos 2))
	 					]
	 	]
	 	finalpos
	))

(defn average [numbers]
    (/ (apply + numbers) (count numbers)))

(defn averageofcoord [& more]
	;(prn more)
	[
		(average (map first more))
		(average (map second more))
		(average (map last more))
	])

(def smartcontinuationofedges true)

(defn retrforbase [arr xcoin ycoin callingfrom callingto makingwhat]
	(retrforbasegoodx arr xcoin ycoin callingfrom callingto makingwhat)
	)

(defn retrforbasegoodx [arr xcoin ycoin callingfrom callingto makingwhat]
	(cond 
		(= xcoin -1)
			(let [ referencepnt		(retrforbase arr 0 ycoin callingfrom callingto makingwhat)]
				(assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [(- 0 mount-hole-width keySpacing ) 0 0])))
		
		(= xcoin arrXWid)
			(let [referencepnt		(retrforbase arr (dec arrXWid) ycoin callingfrom callingto makingwhat)]
				(assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [(+ mount-hole-width keySpacing ) 0 0])))

		:else
			(retrforbasegoody arr xcoin ycoin callingfrom callingto makingwhat)
		)
	)

(defn retrforbasegoody [arr xcoin ycoin callingfrom callingto makingwhat]
	;(prn xcoin ycoin callingfrom callingto makingwhat)
	(cond 
		(= -1 ycoin) 
			(let [
				referencepnt		(retrforbase arr xcoin 0 callingfrom callingto makingwhat)
				](assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [0 (- 0 mount-hole-width keySpacing ) 0])))

		(= arrYLen ycoin)
			(let [
				referencepnt		(retrforbase arr xcoin (dec arrYLen) callingfrom callingto makingwhat)
				](assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [0 (+ mount-hole-width keySpacing ) 0])))
	
		(and (not ((retr arr xcoin ycoin) :existence)) smartcontinuationofedges)
							(let [newkeyandpos  (case callingfrom
													:callfromthisone
														(case makingwhat
															:makingcolumns [(retrforbase arr xcoin (inc ycoin) callingfrom callingto makingwhat) 	[0 (- 0 mount-hole-height keySpacing) 0]] 
															:makingrows    [(retrforbase arr (inc xcoin) ycoin callingfrom callingto makingwhat) 	[(- 0 mount-hole-height keySpacing) 0 0]] 
															:makingdiag    (cond 
																																				((smartretrPntData arr xcoin (inc ycoin)) 		:existence) [(smartretrPntData arr xcoin (inc ycoin) ) 			[0 (- 0 mount-hole-height keySpacing) 0]]

																				((smartretrPntData arr (inc xcoin) ycoin) 		:existence) [(smartretrPntData arr (inc xcoin) ycoin  	)		[(- 0 mount-hole-height keySpacing) 0 0]]
																				
																				((smartretrPntData arr (inc xcoin) (inc ycoin)) :existence) [(smartretrPntData arr (inc xcoin) (inc ycoin)) 	[(- 0 mount-hole-height keySpacing) (- 0 mount-hole-height keySpacing) 0]]

																				)
															;[(retr arr xcoin ycoin) [0 0 0]]
															)
													:callfromleft	
														(case makingwhat
															:makingrows    [(retrforbase arr (dec xcoin) ycoin callingfrom callingto makingwhat) 	[(+ mount-hole-height keySpacing) 0 0]] 
															:makingdiag    (cond 
																				((smartretrPntData arr (dec xcoin) ycoin) 		:existence) [(smartretrPntData arr (dec xcoin) ycoin  	)		[(+ mount-hole-height keySpacing) 0 0]]
																				((smartretrPntData arr xcoin (inc ycoin)) 		:existence) [(smartretrPntData arr xcoin (inc ycoin) ) 			[0 (- 0 mount-hole-height keySpacing) 0]]
																				((smartretrPntData arr (dec xcoin) (inc ycoin)) :existence) [(smartretrPntData arr (dec xcoin) (inc ycoin)) 	[(+ mount-hole-height keySpacing) (- 0 mount-hole-height keySpacing) 0]]

																				)
															;[(retr arr xcoin ycoin) [0 0 0]]
															)
													:callfrombelow	
														(case makingwhat
															:makingcolumns [(retrforbase arr xcoin (dec ycoin) callingfrom callingto makingwhat) 	[0 (+ mount-hole-height keySpacing) 0]] 
															:makingdiag    (cond 
																				((smartretrPntData arr (inc xcoin) ycoin) 		:existence)  [(retrforbase arr (inc xcoin) ycoin callingfrom callingto makingwhat) 			[(- 0 mount-hole-height keySpacing) 0 0]]
																				((smartretrPntData arr xcoin (dec ycoin)) 		:existence)  [(retrforbase arr xcoin (dec ycoin) callingfrom callingto makingwhat) 			[0 (+ mount-hole-height keySpacing) 0]]
																				((smartretrPntData arr (inc xcoin) (dec ycoin)) :existence)  [(retrforbase arr (inc xcoin) (dec ycoin) callingfrom callingto makingwhat) 	[(- 0 mount-hole-height keySpacing) (+ mount-hole-height keySpacing) 0]]
																				)
															;[(retr arr xcoin ycoin) [0 0 0]]
															)
													:callfromleftbelow	
														(case makingwhat
															:makingdiag    (cond 
																				((smartretrPntData arr (dec xcoin) ycoin) 		:existence)  [(retrforbase arr (dec xcoin) ycoin callingfrom callingto makingwhat) 			[(+ mount-hole-height keySpacing) 0 0]]
																				((smartretrPntData arr xcoin (dec ycoin)) 		:existence)  [(retrforbase arr xcoin (dec ycoin) callingfrom callingto makingwhat) 			[0 (+ mount-hole-height keySpacing) 0]]
																				((smartretrPntData arr (dec xcoin) (dec ycoin)) :existence)  [(retrforbase arr (dec xcoin) (dec ycoin) callingfrom callingto makingwhat) 	[(+ mount-hole-height keySpacing) (+ mount-hole-height keySpacing) 0]]
																				)
															)

													;[(retr arr xcoin ycoin) [0 0 0]]
													)
									]	
								(assoc (newkeyandpos 0) :cpntPos (attachpoint [((newkeyandpos 0) :cpntPos) ((newkeyandpos 0) :cpntVec) ((newkeyandpos 0) :cpntAng)] (newkeyandpos 1)))
								)
									
		:else
			(retr arr xcoin ycoin))
	)	



(defn centreofcomponentpost [arr component xval yval pos uporlow]
	(let [
		currentPnt 		(smartretrPntData arr xval yval)
		halfwid 		(/ mount-hole-width 2) 			
		adjacentPnts	(cond 
							(= component :diag)	
							(cond 
								(= pos :tr) [(retrforbase arr xval (inc yval)) (retrforbase arr (inc xval) yval) (retrforbase arr (inc xval) (inc yval))]
								(= pos :br) [(retrforbase arr xval (dec yval)) (retrforbase arr (inc xval) yval) (retrforbase arr (inc xval) (dec yval))]
								(= pos :tl) [(retrforbase arr xval (inc yval)) (retrforbase arr (dec xval) yval) (retrforbase arr (dec xval) (inc yval))]
								(= pos :bl) [(retrforbase arr xval (dec yval)) (retrforbase arr (dec xval) yval) (retrforbase arr (dec xval) (dec yval))]
								)
							(= component :row)
							(cond 
								(= pos :tr) [(retrforbase arr (inc xval) yval)]
								(= pos :br) [(retrforbase arr (inc xval) yval)]
								(= pos :tl) [(retrforbase arr (dec xval) yval)]
								(= pos :bl) [(retrforbase arr (dec xval) yval)]
								)
							(= component :col)
							(cond 
								(= pos :tr) [(retrforbase arr xval (inc yval))]
								(= pos :br) [(retrforbase arr xval (dec yval))]
								(= pos :tl) [(retrforbase arr xval (inc yval))]
								(= pos :bl) [(retrforbase arr xval (dec yval))]
								)
							)

		corners 		(cond 
							(= component :diag)
							(cond 
								(= pos :tr) [[halfwid halfwid 0] [halfwid (- halfwid) 0] [( - halfwid) halfwid 0] [(- halfwid) (- halfwid) 0]]
								(= pos :br) [[halfwid (- halfwid) 0] [halfwid halfwid 0] [( - halfwid) (- halfwid) 0] [(- halfwid) halfwid 0]]
								(= pos :tl) [[(- halfwid) halfwid 0] [(- halfwid) (- halfwid) 0] [halfwid halfwid 0]  [halfwid (- halfwid) 0]]
								(= pos :bl) [[(- halfwid) (- halfwid) 0] [(- halfwid) halfwid 0] [halfwid (- halfwid) 0] [halfwid halfwid 0]]
								)
							(= component :row)
							(cond 
								(= pos :tr) [[halfwid halfwid 0] [(- halfwid) halfwid 0]]
								(= pos :br) [[halfwid (- halfwid) 0] [(- halfwid) (- halfwid) 0]]
								(= pos :tl) [[(- halfwid) halfwid 0] [halfwid halfwid 0]]
								(= pos :bl) [[(- halfwid) (- halfwid) 0] [halfwid (- halfwid) 0]]
							)
							(= component :col)
							(cond 
								(= pos :tr) [[halfwid halfwid 0] [halfwid (- halfwid) 0]]
								(= pos :br) [[halfwid (- halfwid) 0] [halfwid halfwid 0]]
								(= pos :tl) [[(- halfwid) halfwid 0] [(- halfwid) (- halfwid) 0]]
								(= pos :bl) [[(- halfwid) (- halfwid) 0] [(- halfwid) halfwid 0]]
							)
						)
							
		attachedcorners (cond 
					(= component :diag)
						[(attachpoint [(currentPnt :cpntPos) (currentPnt :cpntVec) (currentPnt :cpntAng)] (corners 0))
						(attachpoint [((adjacentPnts 0) :cpntPos) ((adjacentPnts 0) :cpntVec) ((adjacentPnts 0) :cpntAng)] (corners 1))
						(attachpoint [((adjacentPnts 1) :cpntPos) ((adjacentPnts 1) :cpntVec) ((adjacentPnts 1) :cpntAng)] (corners 2))
						(attachpoint [((adjacentPnts 2) :cpntPos) ((adjacentPnts 2) :cpntVec) ((adjacentPnts 2) :cpntAng)] (corners 3))]
					(or (= component :row ) (= component :col))
						[(attachpoint [(currentPnt :cpntPos) (currentPnt :cpntVec) (currentPnt :cpntAng)] (corners 0))
						(attachpoint [((adjacentPnts 0) :cpntPos) ((adjacentPnts 0) :cpntVec) ((adjacentPnts 0) :cpntAng)] (corners 1))]
					(= component :key)
						[(currentPnt :cpntPos)]
						
					)


		cornersaveraged (cond
							(= component :key)
								(currentPnt :cpntPos)
							:else
								(apply averageofcoord attachedcorners)
								)
		averagevec 		(cond 
							(= component :diag)
								(averageofcoord (currentPnt :cpntVec) ((adjacentPnts 0) :cpntVec) ((adjacentPnts 1) :cpntVec) ((adjacentPnts 2) :cpntVec))
							(or (= component :row) (= component :col))
								(averageofcoord (currentPnt :cpntVec) ((adjacentPnts 0) :cpntVec))
							(= component :key)
								(currentPnt :cpntVec)
						)
		post 			(cube 0.1 0.1 3)	
		depthbeneath 	8.1
		offset			-0.1
		slightrans 		(case pos
							:br [(- offset) offset 0]
							:bl [offset offset 0]
							:tr [(- offset) (- offset) 0]
							:tl [offset (- offset) 0]
							:centre [0 0 0]
							)
		; slightrans 		[0 0 0]
		]
		;(prn cornersaveraged)
		;(attach [cornersaveraged averagevec 0] [[0 0 13] [0 0 1] 0] post)


		(case uporlow
			:upper
				(attachpoint [cornersaveraged averagevec 0] [(slightrans 0) (slightrans 1) (- 0 depthbeneath)])

			:lower

				(attachpoint [cornersaveraged averagevec 0] [(slightrans 0) (slightrans 1) (- 0 depthbeneath plate-thickness)])
			)
		
		)


	)

(defn makenewbase [arr]
	(apply union
	(concat
	;(for [ycoin (range arrYLen) xcoin (range arrXWid)]
	(for [ycoin (range 0 arrYLen) xcoin [0 1 2 3 4 5 6 7]]
		(when ((retr arr xcoin ycoin) :existence)
			; (union
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :tl)
			; 	(centreofcomponentpost arr :col xcoin ycoin :tl)
			; 	(centreofcomponentpost arr :row xcoin ycoin :tl)
			; )
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :bl)
			; 	(centreofcomponentpost arr :col xcoin ycoin :bl)
			; 	(centreofcomponentpost arr :row xcoin ycoin :bl)
			; 	)
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :br)
			; 	(centreofcomponentpost arr :col xcoin ycoin :br)
			; 	(centreofcomponentpost arr :row xcoin ycoin :br)
			; 	)
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :tr)
			; 	(centreofcomponentpost arr :col xcoin ycoin :tr)
			; 	(centreofcomponentpost arr :row xcoin ycoin :tr)
			; 	)
			; (hull
			; 	(centreofcomponentpost arr :col xcoin ycoin :tl)
			; 	(centreofcomponentpost arr :col xcoin ycoin :tr)
			; 	(centreofcomponentpost arr :col xcoin ycoin :br)
			; 	(centreofcomponentpost arr :col xcoin ycoin :bl)

			; 	(centreofcomponentpost arr :row xcoin ycoin :tl)
				
			; 	(centreofcomponentpost arr :row xcoin ycoin :bl)
				
			; 	(centreofcomponentpost arr :row xcoin ycoin :br)
				
			; 	(centreofcomponentpost arr :row xcoin ycoin :tr)
			; 	))

			(let [
				points [
					(centreofcomponentpost arr :diag xcoin ycoin :tl :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :tl :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :tr :upper)
					(centreofcomponentpost arr :diag xcoin ycoin :tr :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :tr :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :br :upper)
					(centreofcomponentpost arr :diag xcoin ycoin :br :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :br :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :bl :upper)
					(centreofcomponentpost arr :diag xcoin ycoin :bl :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :bl :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :tl :upper)

					(centreofcomponentpost arr :diag xcoin ycoin :tl :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :tl :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :tr :lower)
					(centreofcomponentpost arr :diag xcoin ycoin :tr :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :tr :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :br :lower)
					(centreofcomponentpost arr :diag xcoin ycoin :br :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :br :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :bl :lower)
					(centreofcomponentpost arr :diag xcoin ycoin :bl :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :bl :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :tl :lower)

					(centreofcomponentpost arr :key  xcoin ycoin :centre :upper) ;24
					(centreofcomponentpost arr :key  xcoin ycoin :centre :lower) ;25

					]

				faces [
					; [0 1 2 3 4 5 6 7 8 9 10 11]
					; [23 22 21 20 19 18 17 16 15 14 13 12]
					[0 1 11] [2 3 4] [5 6 7] [8 9 10] [1 2 24] [2 4 24] [4 5 24] [5 7 24] [7 8 24] [8 10 24] [10 11 24] [11 1 24]
					[13 12 23] [16 15 14] [19 18 17] [22 21 20] [25 14 13] [25 16 14] [25 17 16] [25 19 17] [25 20 19] [25 22 20] [25 23 22] [25 13 23]
					[0 12 13]
					[1 0 13]
					[1 13 14]
					[1 14 2]
					[2 14 15]
					[2 15 3]
					[3 15 16]
					[3 16 4]
					[4 16 17]
					[4 17 5]
					[5 17 18]
					[5 18 6]
					[6 19 7]
					[6 18 19]
					[7 19 20]
					[7 20 8]
					[8 20 21]
					[8 21 9]
					[9 21 22]
					[9 22 10]
					[10 22 23]
					[10 23 11]
					[11 23 12]
					[11 12 0]
				]]
			
			;(translate [(* (rand) 10) (rand) (rand)]
			;(resize [20 20 3]
			;(union
			;(translate [(* 10 xcoin) 0 0]
			(polyhedron points faces ););)
				
		)))

		)))
(defn makelegs [arr]
	(let [
		topright ((retr arr (- arrXWid 1) (- arrYLen 1)) :cpntPos)
		topleft ((retr arr 1 (- arrYLen 1)) :cpntPos)
		bottomright ((retr arr (- arrXWid 1) 1) :cpntPos)
		bottomleft ((retr arr 1 1) :cpntPos)
		]
		(union
			(translate [(topright    0) (topright    1) 0] (cylinder 5 50))
			(translate [(topleft     0) (topleft     1) 0] (cylinder 5 50))
			(translate [(bottomright 0) (bottomright 1) 0] (cylinder 5 50))
			(translate [(bottomleft  0) (bottomleft  1) 0] (cylinder 5 50))
			)
		)
	)

(defn makenewbasewithextras [arr]
	(union
	(makenewbase arr)
	(difference
	(makelegs arr)

	(translate [0 0 1] (makenewbase arr)
	(translate [0 0 (* (- plate-thickness 0.5) 1)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 2)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 3)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 4)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 5)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 6)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 7)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 8)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 9)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 10)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 11)] (makenewbase arr))

	))))

(defn findnewvec [[x1 y1 z1] [x2 y2 z2]]
	"Simple function to find vector between two points."
	[(- x2 x1) (- y2 y1) (- z2 z1)]
	)

(defn curveitbaby [arr]
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData)
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				

				newPos 		[ (cpntP 0) (cpntP 1) (* (sqrt (+ (expt (cpntP 0) 2) (expt (cpntP 1) 2))) 0.1) ]
				focuspnt	[ 30 0 150]

				newVec 		(findnewvec  [(newPos 0) (newPos 1) (newPos 2)] focuspnt )
				]
				

				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos newPos, 
				 :cpntVec newVec,
				 :cpntAng cpntA}

		))))))

(defn centrearray [arr]
	(let [
		xcoords  (for [ycoin arr pntData ycoin] 
					((pntData :cpntPos) 0) 
					)
		ycoords  (for [ycoin arr pntData ycoin] 
					((pntData :cpntPos) 1) 
					)
		minx 	(apply min xcoords)
		miny 	(apply min ycoords)
		maxx 	(apply max xcoords)
		maxy 	(apply max ycoords)

		halfrangex (/ (- maxx minx) 2)
		halfrangey (/ (- maxy miny) 2)]

		
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData) ; coordinate in array, not coord in 3d
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				]
				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos [(+ (- (cpntP 0) maxx) halfrangex)
				           (+ (- (cpntP 1) maxy) halfrangey) 
				           (cpntP 2)] , 
				 :cpntVec cpntV,  
				 :cpntAng cpntA}


				)

			))))

	

	)

	)

(defn apply3dequation [arr fxy fpartialx fpartialy xmulti ymulti zmulti]
	"this is a general function. It takes fxy and makes z into fxy. 
	fxy uses xmulti and ymulti to change the scale of the 3d plot. 
	It does not change the actual coordinates as this would interfere
	with switch placement. zmulti is applied to the z coordinates 
	after the function has been applied to get the scale correct.
	fpartialx is the partial derivative of fxy with respect to x.
	fpartialy is the partial derivative of fxy with respect to y.

	The point of including the fpartials is to orientate the switches
	along the normal vector at that point in the equation so it looks 
	like a gradual curve.

	It returns an array like the other array transformationFunctions."
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData)
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				

				newPos 		[
							(cpntP 0) 
							(cpntP 1) 
							(+ (cpntP 2) (* (fxy (* (cpntP 0) xmulti) (* (cpntP 1) ymulti)) zmulti)) ]


				newVec 		[
							(- (* (fpartialx (* (cpntP 0) xmulti) (* (cpntP 1) ymulti)) zmulti) ) 
							(- (* (fpartialy (* (cpntP 1) ymulti) (* (cpntP 0) xmulti)) zmulti))
							1 ]
				]
				

				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos newPos, 
				 :cpntVec newVec,
				 :cpntAng cpntA}

		)

	))))
	)

(defn gradualcurve [arr rowangle columnangle]
		"This curves the array gradually."
	(vec (for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]

			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData)
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				xmod		(int (/ arrXWid 2))
				ymod		(int (/ arrYLen 2))

				betterxval  (- xval xmod)
				betteryval  (inc (- yval xmod))

				currentcolangle (* betteryval columnangle)
				currentrowangle (* betterxval columnangle)
				 		

				rowrotated	[
							(cpntP 0) 
							(- (* (cpntP 1) (Math/cos currentcolangle))  (* (cpntP 2) (Math/sin currentcolangle)) ) 
							(* (+ (* (cpntP 1) (Math/sin currentcolangle))  (* (cpntP 2) (Math/cos currentcolangle)) ) 1)
							]

				fullyrotated [
							(- (* (rowrotated 0) (Math/cos currentrowangle))  (* (rowrotated 2) (Math/sin currentrowangle)) ) 
							(rowrotated 1)
							(+ (* (rowrotated 2) (Math/cos currentrowangle))  (* (rowrotated 0) (Math/sin currentrowangle)) ) 

							]

				newVec 		(findnewvec fullyrotated [0 0 1] ) 
				]
				
				(prn betteryval)
				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos fullyrotated, 
				 :cpntVec newVec,
				 :cpntAng cpntA}



			))))))

(defn newcurveitbaby [arr]
	(apply3dequation
		arr
		(partial  #(- (sqrt (- 1000 (expt %1 2) (expt %2 2)))))
		(partial  #(/ %1 (sqrt (- 1000 (expt %1 2) (expt %2 2)))))
		(partial  #(/ %1 (sqrt (- 1000 (expt %1 2) (expt %2 2)))))
		0.25
		0.2
		5
		)
	)

(defn curvexaxisy [arr]
	(let [
		arguments [arr (partial #(* (+ (* (expt %1 2) 0.25) ( expt %2 2) ) 0.008))

					
					(partial #(+ (* %1 2 0.008 0.25)  (* %2 0) ))
					(partial #(+ (* %1 2 0.008)     (* %2 0) ))	
					
					1
					1
					1 ]]

	(apply apply3dequation arguments)))

(defn showfunction [fxy xrange yrange]
	(for [x (range (- xrange) xrange) y (range (- yrange) yrange)] (
		
		(spit "things/post-demo.scad"
      		(point x y (fxy x y) )) :append true)
		)
	)

(defn keyExistence [arr]
	(let [existencearray
					 [
					[false true true false false false true true] 
					[false true true true true true true true] 
					[false true true true true true true true] 
					[true true true true true true true true] 
					[true true true false false false false true] ;as seem from origin looking in pos x, pos y

					]]
		(vec (for [ycoin (range arrYLen)]
			(vec (for [xcoin (range arrXWid)]
				(assoc (retr arr xcoin ycoin) :existence (get-in existencearray [(- (dec arrYLen) ycoin) xcoin]))
		)))))
	)

(defn alignkeys [arr & more]
	;(prn (nth more 0))
	(let [
	 	vecofkeys 		(nth more 0)
	 	movingkey 		(nth vecofkeys 0)

 		direction		(vecofkeys 2)

	 	anchorkey		(retr arr (get-in vecofkeys [1 0]) (get-in vecofkeys [1 1]))
	 	anchkeypos		(anchorkey :cpntPos)
	 	anchkeyvec		(anchorkey :cpntVec)
	 	anchkeyang		(anchorkey :cpntAng)

	 	u 				(unitv anchkeyvec)
	 	a 				(u 0)
	 	b 				(u 1)
	 	c 				(u 2)
	 	d 				(modofvec [0 b c])

	 	ConD 			(/ c d)
	 	BonD 			(/ b d)
		
	 	startingpnt 	(case direction	
	 						:ontheleft  [-19 0 0]
	 						:ontheright [19  0 0]
	 						)



	 	yaxisinv    	[
	 					 (+ (* (startingpnt 0) d) (* (startingpnt 2) a))
	 					 (startingpnt 1)
	 					 (- (* (startingpnt 2) d) (* (startingpnt 0) a))
	 					]

	 	xaxisinv   		[
	 					 (yaxisinv 0)
	 					 (+ (* (yaxisinv 1) ConD) (* (yaxisinv 2) BonD))
	 					 (- (* (yaxisinv 2) ConD) (* (yaxisinv 1) BonD))
	 					]
        
	 	finalpos		[
	 					(+ (xaxisinv 0) (anchkeypos 0))
	 					(+ (xaxisinv 1) (anchkeypos 1))
	 					(+ (xaxisinv 2) (anchkeypos 2))
	 					]

  	    updatedpos 		(assoc-in arr [ (movingkey 1) (movingkey 0) :cpntPos ] finalpos)


		updatedvec 		(assoc-in updatedpos [(movingkey 1) (movingkey 0) :cpntVec] anchkeyvec)
 

		]
		;(prn u (modofvec u))

		;(prn finalpos anchkeypos yaxisinv xaxisinv)
		updatedvec
		)

	)

(defn usbcutouts [positiveornegativeshape]
	(let [
		internalwidth 		28
		wallwdith 			4
		internalheight	 	7
		depth				13]

	
			(->>
			(case positiveornegativeshape
				:positive 
					(hull
						(translate [(- 0 (/ internalwidth 2) (/ wallwdith 2)) 0 0] (cube wallwdith depth internalheight)) 
						(translate [(+   (/ internalwidth 2) (/ wallwdith 2)) 0 0] (cube wallwdith depth internalheight))
						(translate [0 -2.5 (- 3)] (cube internalwidth depth internalheight)))
				:negative
					(translate [0 0 4] (cube (inc internalwidth) 40 10)))
					
			(translate [0 20 -6])
			(rotate -0.2 [1 0 0])
		
		))
	)

(defn sidenub []
	(->> (cylinder 1 2.75)
  		 (with-fn 30)
		 (rotate (/ π 2) [1 0 0])
		 (translate [(+ (/ mount-hole-width 2)) 0 1])
		 (hull (->> (cube 0.01 2.75 plate-thickness)
			         (translate [(+ (/ 0.01 2) (/ mount-hole-width 2))
			                     0
			                     (/ plate-thickness 2)])))

		 )
	)

(defn makesidenubs [arr]
	(for [ycoin (range arrYLen) xcoin (range arrXWid)]
		(let [
			pntData (retr arr xcoin ycoin)
			; xval		(:xcoord pntData)
			; yval  	(:ycoord pntData)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
			exist 		(:existence pntData)
			]
			(when exist
				(union
				(attach
					[cpntP cpntV cpntA]
					[[0 0 plate-thickness] [0 0 1] 0]
					(sidenub))

				(attach
					[cpntP cpntV cpntA]
					[[0 0 plate-thickness] [0 0 1] 0]
					(rotate Math/PI [0 0 1] (sidenub)))
				))


			)

		)

	)


(defn angleKey [arr colOrRow points angle]; in x axis
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData)
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				condition 	(case colOrRow
								:row (= points yval)
								:col (= points xval)
								:colrow (and (= (points 0) xval) (= (points 1) yval)))
				]
				

				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos cpntP,
				 :cpntVec (if condition 
				 				;((prn rownum)
					 			[
					 			(cpntV 0)
					 			(- (* (cpntV 1)  (Math/cos angle)) (* (cpntV 2) (Math/sin angle)))
					 			(+ (* (cpntV 1)  (Math/sin angle)) (* (cpntV 2) (Math/cos angle)))

					 			];)
					 		cpntV)
					 		,
				 :cpntAng cpntA}
				

				)

			)
		))
	))

(defn base-dual-hulls [shapes]
	 (apply union
         (map (partial apply hull)
              (partition 2 1 shapes))))

(defn newbase [arr & more]
	
	;(base-dual-hulls
	;(concat
		(for [ycoin (range arrYLen) xcoin (range arrXWid)]

		(let [
			pntData 	(retr arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
			existence (:existence pntData)
			pntDataAbv 	(smartretrPntData arr xcoin (inc ycoin))
			cpntPAbv 		(:cpntPos pntDataAbv)
			cpntVAbv 		(:cpntVec pntDataAbv)
			cpntAAbv 		(:cpntAng pntDataAbv)
			existenceAbv (:existence pntDataAbv)
			pntDataRght 	(smartretrPntData arr (inc xcoin) ycoin)
			cpntPRght 		(:cpntPos pntDataRght)
			cpntVRght 		(:cpntVec pntDataRght)
			cpntARght 		(:cpntAng pntDataRght)
			existenceRght (:existence pntDataRght)
			pntDataRghtAbv 	(smartretrPntData arr (inc xcoin) (inc ycoin))
			cpntPRghtAbv 		(:cpntPos pntDataRghtAbv)
			cpntVRghtAbv 		(:cpntVec pntDataRghtAbv)
			cpntARghtAbv 		(:cpntAng pntDataRghtAbv)
			existenceRghtAbv (:existence pntDataRghtAbv)
			baseThickness 4
			depth		14
			]


			(union
			(when (and existence existenceAbv)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntP cpntV cpntA] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(color [1 0.5 0 1])
					(attach [cpntPAbv cpntVAbv cpntAAbv] [[0 0 depth] [0 0 1] 0]))
			
			))
			(when (and existence existenceRght)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntP cpntV cpntA] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPRght cpntVRght cpntARght] [[0 0 depth] [0 0 1] 0]))
			
			))
			(when (and existence existenceRghtAbv)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntP cpntV cpntA] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPRghtAbv cpntVRghtAbv cpntARghtAbv] [[0 0 depth] [0 0 1] 0]))
			
			))
			(when (and existenceAbv existenceRght)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPAbv cpntVAbv cpntAAbv] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPRght cpntVRght cpntARght] [[0 0 depth] [0 0 1] 0]))
			
			))


			)))
	);))
	
(defn transformationFunctions [arr & more]
	"these are the functions that rewrite the array"
	(-> 
		(centrearray arr)
		

		(moveonXYZ 0 15  -2 :col 0)
		(moveonXYZ 0 15  -2 :col 1)
		(moveonXYZ 0 15  -2 :col 2)
		(moveonXYZ 0 19 -5 :col 3)
		(moveonXYZ 0 8 -2.5  :col 4)
		(moveonXYZ 0 0 -8  :row 0)

		(curvexaxisy)

		; (angleKey :row 0 		(/ Math/PI 6))
		; (angleKey :colrow [0 1] (/ Math/PI 6))
		
		(alignkeys [[0 0] [1 0] :ontheleft])
		(alignkeys [[7 0] [6 0] :ontheright])
		(alignkeys [[7 1] [6 1] :ontheright])
		(alignkeys [[7 2] [6 2] :ontheright])
		(alignkeys [[7 3] [6 3] :ontheright])
		(alignkeys [[7 4] [6 4] :ontheright])
		(keyExistence)

		)
	)
	

(def keyswitch 
	(let [
		hw (/ 15.6 2)
		points 	[
					[hw hw 0] [hw (- hw) 0] [(- hw) (- hw) 0] [(- hw) hw 0]
					[hw hw 1] [hw (- hw) 1] [(- hw) (- hw) 1] [(- hw) hw 1]
					[(- hw 2) (- hw 2) -5] [(- hw 2) (- 0 hw -2) -5] [(- 0 hw -2) (- 0 hw -2) -5] [(- 0 hw -2) (- hw 2) -5]
					[(- hw 3) (- hw 3) 6.6] [(- hw 3) (- 0 hw -3) 6.6] [(- 0 hw -3) (- 0 hw -3) 6.6] [(- 0 hw -3) (- hw 3) 6.6]
					]

		faces	[
				;	[3 2 1 0] 
				;	[4 5 6 7]
					[0 1 5 4]
					[2 3 7 6]
					[1 2 6 5]
					[3 0 4 7]
					[11 10 9 8]
					[12 13 14 15]
					[8 9 1 0]
					[9 10 2 1]
					[10 11 3 2]
					[11 8 0 3]

					[4 5 13 12]
					[5 6 14 13]
					[6 7 15 14]
					[7 4 12 15]

					]
		]
	(union
	(polyhedron points faces)
	(translate [0 0 1] (cylinder (/ 3.30 2) 18.5))
	)))

(defn doesnttoucharrayFunctions [arr & more]
	"These functions only read the array hence the arr parameter being 
	passed to each of them individually. Unlike the threading of the transformationFunctions"
	;(let [base (makeconnectors arr :base)]
	(union 
		;(putsquareinarr arr)

		
		(makeconnectors arr :plate)
		;(scale [0.97 0.95 1] 
		(makeconnectors arr :base)
		;(makelegs arr)
		;(hull
		;(makenewbasewithextras arr);)
		;(translate [0 0 (- 0 plate-thickness -0.1)] (makenewbase arr)))

		; (difference
		; 	(union (newbase arr))
		; 	(union (translate [0 0 4] (newbase arr)))
		; )
		; (difference
		; 	(union
		; 		base
		; 		(usbcutouts :positive)
		; 		)
		; 	(union
		; 		(usbcutouts :negative)
		; 		;(translate [0 0 (+ plate-thickness 1.5)] base)
		; 	))
		;(usbcutouts base)
		;(makesidenubs arr)
		;(showkeycaps arr)
		;(showconnectors arr)
		;keyswitch
		;)
	))


(defn buildarray []
		 (-> (createarray arrXWid arrYLen)
		 	 (transformationFunctions)
		 	 (doesnttoucharrayFunctions) ;the outcome of this should be code for scad-clj
		 	 )
	)

(spit "things/post-demo.scad"
      (write-scad 
      	(->>
      	(buildarray)
      	;(rotate (/ Math/PI 15) [0 1 0]) 
      ;	(rotate (/ Math/PI 10) [1 0 0])

      		))  :append true)