;; ==============================================
;; Script: r3d2.r
;; downloaded from: www.REBOL.org
;; on: 20-May-2014
;; at: 15:49:29.132413 UTC
;; owner: crazyaxe [script library member who can
;; update this script]
;; ==============================================
REBOL [ 
	Title: "r3D"
	Author: [ "Massimiliano Vessi" "Andrew Hoadley" "FranÃ§ois Jouen"]
	Email: %maxint--tiscali--it 
	Date: 09-Oct-2012 
	version: 2.2.6
	file: %r3d2.r 
	Purpose: {"3D Library, you can use also images.
		Look a the end of the file to see how are made 3D model.} 		
		 ;following data are for www.rebol.org library 
		 ;you can find a lot of rebol script there 
	 library: [ 
		level: 'intermidiate 
		platform: 'all 
		type: [ tool] 
		domain: [animation external-library graphics] 
		tested-under: [windows linux] 
		support: none 
		license: [gpl] 
		see-also: none 
		] 
	] 
	





; Copyright (c) 2006, Andrew Hoadley

; Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
; and associated documentation files (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all copies or substantial 
; portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT 
; NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES 
; OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


; ---------------------------------------------------------
; Contents of r3d-matrix.r - matrix and vector library
;

; create different types of transformations

r3d-identity: [
            1 0 0 0
            0 1 0 0 
            0 0 1 0 
            0 0 0 1 
	]

r3d-perspective: func [
	"Create a perspective matrix with a vanishing point d units from the camera"
	d [number!] "d is the distance to the vanishing point - don't set it to zero !!"
	][reduce [
		1 0 0 0
		0 1 0 0 
		0 0 0 0 
		0 0 1 / d 1 
		]
	];end of r3d-perspective


r3d-translate: func [
	"Create a translation matrix"
	X [number!] "translate X units along the x axis"
	Y [number!] "translate Y units along the y axis"
	Z [number!] "translate Z units along the z axis"
	][  reduce [
		1 0 0 X 
		0 1 0 Y 
		0 0 1 Z 
		0 0 0 1
		]
	]; end of r3d-translate

r3d-scale: func [
	"Create a scale matrix"
	X [number!] "scale object by a factor of X along the x axis"
	Y [number!] "scale object by a factor of Y along the y axis"
	Z [number!] "scale object by a factor of Y along the z axis"
	][    reduce [
		X 0 0 0 
		0 Y 0 0 
		0 0 Z 0 
		0 0 0 1 
		]
	]; end of r3d-scale

r3d-rotateX: func [
	X [number!] 
	][	; calculate once and store the sin and cos values
	sineX:   sine X
	cosineX: cosine X
	reduce [
		1     0         0         0
		0     cosineX     (- sineX)  0
		0     sineX       cosineX     0
		0     0         0         1
		]
	]; end of r3d-rotateX

r3d-rotateY: func [
	Y [number!] 
	][		; calculate once and store the sin and cos values
	sineY:   sine Y
	cosineY: cosine Y
	reduce [
		cosineY     0     sineY     0
		0         1     0       0
		(- sineY)   0     cosineY  0
		0         0     0       1
		]
	]; end of r3d-rotateY


r3d-rotateZ: func [
	Z [number!]
	][    ; calculate once and store the sin and cos values
	sineZ:   sine Z
	cosineZ: cosine Z
	reduce [
		cosineZ     (- sineZ)   0     0
		sineZ       cosineZ     0     0
		0         0         1     0
		0         0         0     1 
		]
	]; end of r3d-rotateZ

r3d-face-direction: func [
	DOF     [block!] "Vector3 specifying direction of flight"
	up      [block!] "Up vector may be in any direction except parallel to DOF"
	/local xaxis yaxis zaxis
	][
	zaxis: r3d-normalise DOF
	xaxis: r3d-normalise r3d-crossproduct up dOF
	yaxis: r3d-normalise r3d-crossproduct DOF xaxis  
	reduce [ 
		xaxis/1    yaxis/1     zaxis/1     0
		xaxis/2    yaxis/2     zaxis/2     0
		xaxis/3    yaxis/3     zaxis/3     0
		0              0              0               1
		]
	];end of r3d-face-direction


r3d-position-object: func [
	object_position     [block!] ; all 3 parameters are 3 dimensional vectors
	position_to_look_at [block!] 
	up                  [block!]
	/local DOF
	][
	DOF: r3d-Subtract position_to_look_at  object_position 
	zaxis: r3d-normalise DOF
	xaxis: r3d-normalise r3d-crossproduct DOF up
	yaxis: r3d-normalise r3d-crossproduct DOF xaxis  	
	reduce [ 
		xaxis/1    yaxis/1     zaxis/1     object_position/1
		xaxis/2    yaxis/2     zaxis/2     object_position/2
		xaxis/3    yaxis/3     zaxis/3     object_position/3
		0             0               0               1 
		]
	]; endo of r3d-position-object


r3d-m4xm4: func [
	"Matrix product between two 4x4 matrices"
	m1 [block!] "First matrix, length must be 16 (4x4)"
	m2 [block!] "Second matrix, length must be 16 (4x4)"
	/local result
	][
	result: copy []
	foreach [a b c d] m1 [
		append result reduce [ 
			(a * m2/1) + (b * m2/5) + (c * m2/9) + (d * m2/13)          
			(a * m2/2) + (b * m2/6) + (c * m2/10) + (d * m2/14) 
			(a * m2/3) + (b * m2/7) + (c * m2/11) + (d * m2/15) 
			(a * m2/4) + (b * m2/8) + (c * m2/12) + (d * m2/16) 
			]        
		]
	result
	]; end of r3d-m4xm4


r3d-m4xv3: func [
	"Matrix product between a 4x4 matrix and a 4x1 ([A B C 1]) vector. This function will append the last 1 to the vector 3x1"
	m [block!] "A 4x4 metrix (length 16)"
	v [block!] " A 3x1 vector, this function will tranform it in a 4x1 appnding a 1"
	/local result 
	][
	tempres: copy []
	result: copy []
	foreach [a b c d] m [
		append tempres reduce [ 
			(a * v/1) + (b * v/2) + (c * v/3) + d 
			]        
		]
	; create 1/w so we only divide once and then multiply by a fraction (faster) 
	inv-w: reduce (1 / tempres/4)
	append result reduce tempres/1 * inv-w
	append result reduce tempres/2 * inv-w
	append result reduce tempres/3 * inv-w
	append result 1
	return result
	];end of r3d-m4xv3

r3d-m4xv3-array: func [
	"Thi function return the same block of vertices, multiplied for the transformation matrix"
	vertices [block!] "This must be a block of vertices coordinates, like [ [ 1 2 3] [-2 5.2  4] ]"
	m4 [block!] "This is a 4x4 matrix"
	/local result vertex
	][
	result: copy []
	foreach vertex vertices [
		append result reduce [ r3d-m4xv3 m4 vertex ]
		]
	return result
	]; end of r3d-m4xv3-array


;Is this to compose all translation and rotation?
r3d-compose-m4: func [
	"Take a block containing 4x4 matrices and moltiplicate each other obtaining a single 4x4 matrix"
	matrixlist [block!] ; a list of at least 1 4x4 matrix
	/local result
	][
	cm4-len: length? matrixlist
	; if there are no entries return the identity matrix
	if cm4-len = 0 [ 	return r3d-identity ]
	
	; initialise the result with the first entry in the list
	result: copy matrixlist/1
	; if there is one entry return that entry
	if cm4-len = 1 [  return result  ]

	; for each rentry past the first
	for cm4-i 2 cm4-len 1 [
		;multiply the previous result by the next element
		result: r3d-m4xm4 pick matrixlist cm4-i result  
		]   
	return result
	]; end of r3d-compose-m4

r3d-transpose-m4: func [
	"Transpose a 4x4 matrix"
	m [block!] "Matrix to transpose (4x4)"
	/local result
	][
	; transpose a 4x4 matrix
	result: reduce [ m/1 m/5 m/9 m/13 m/2 m/6 m/10 m/14 m/3 m/7 m/11 m/15 m/4 m/8 m/12 m/16]
	]

r3d-inverse-m4: func [
	"Inverse of a 4x4 matrix"
	m [block!] "Matrix 4x4"
	/local result pairs m4inv-res det
	][
	; ok here we go...
	; calculate pairs for first 8 elements
	pairs: reduce [ 
		m/11 * m/16     
		m/12 * m/15     
                    m/10 * m/16
                    m/12 * m/14
                    m/10 * m/15
                    m/11 * m/14
                    m/9  * m/16
                    m/12 * m/13
                    m/9  * m/15
                    m/11 * m/13
                    m/9  * m/14
                    m/10 * m/13
		]
	; calculate first 8 elements
	m4inv-res: reduce [
		((pairs/1 * m/6) + (pairs/4 * m/7) + (pairs/5 * m/8)) -
		((pairs/2 * m/6) + (pairs/3 * m/7) + (pairs/6 * m/8))
	
		((pairs/2 * m/5) + (pairs/7 * m/7) + (pairs/10 * m/8)) -
		((pairs/1 * m/5) + (pairs/8 * m/7) + (pairs/9 * m/8))

		((pairs/3 * m/5) + (pairs/8 * m/6) + (pairs/11 * m/8)) -
		((pairs/4 * m/5) + (pairs/7 * m/6) + (pairs/12 * m/8)) 
                
		((pairs/6 * m/5) + (pairs/9 * m/6) + (pairs/12 * m/7)) -
		((pairs/5 * m/5) + (pairs/10 * m/6) + (pairs/11 * m/7)) 
	
		((pairs/2 * m/2) + (pairs/3 * m/3) + (pairs/6 * m/4)) -
		((pairs/1 * m/2) + (pairs/4 * m/3) + (pairs/5 * m/4)) 

		((pairs/1 * m/1) + (pairs/8 * m/3) + (pairs/9 * m/4)) -
		((pairs/2 * m/1) + (pairs/7 * m/3) + (pairs/10 * m/4)) 

		((pairs/4 * m/1) + (pairs/7 * m/2) + (pairs/12 * m/4)) -
		((pairs/3 * m/1) + (pairs/8 * m/2) + (pairs/11 * m/4)) 

		((pairs/5 * m/1) + (pairs/10 * m/2) + (pairs/11 * m/3)) -
		((pairs/6 * m/1) + (pairs/9 * m/2) + (pairs/12 * m/3)) 
		]
	; calculate pairs for second 8 elements 
	pairs: reduce [ 
		m/3 * m/8
                    m/4 * m/7       
                    m/2 * m/8
                    m/4 * m/6
                    m/2 * m/7
                    m/3 * m/6
                    m/1 * m/8
                    m/4 * m/5
                    m/1 * m/7
                    m/3 * m/5
                    m/1 * m/6
                    m/2 * m/5
		]

	; calculate second 8 elements (cofactors)
	m4inv-res: append m4inv-res reduce [
		((pairs/1 * m/14) + (pairs/4 * m/15) + (pairs/5 * m/16)) -
		((pairs/2 * m/14) + (pairs/3 * m/15) + (pairs/6 * m/16))

		((pairs/2 * m/13) + (pairs/7 * m/15) + (pairs/10 * m/16)) -
		((pairs/1 * m/13) + (pairs/8 * m/15) + (pairs/9 * m/16))

		((pairs/3 * m/13) + (pairs/8 * m/14) + (pairs/11 * m/16)) -
		((pairs/4 * m/13) + (pairs/7 * m/14) + (pairs/12 * m/16)) 
                
		((pairs/6 * m/13) + (pairs/9 * m/14) + (pairs/12 * m/15)) -
		((pairs/5 * m/13) + (pairs/10 * m/14) + (pairs/11 * m/15)) 
                    
		((pairs/3 * m/11) + (pairs/6 * m/12) + (pairs/2 * m/10)) -
		((pairs/5 * m/12) + (pairs/1 * m/10) + (pairs/4 * m/11)) 

		((pairs/9 * m/12) + (pairs/1 * m/9) + (pairs/8 * m/11)) -
		((pairs/7 * m/11) + (pairs/10 * m/12) + (pairs/2 * m/9)) 

		((pairs/7 * m/10) + (pairs/12 * m/12) + (pairs/4 * m/9)) -
		((pairs/11 * m/12) + (pairs/3 * m/9) + (pairs/8 * m/10)) 

		((pairs/11 * m/11) + (pairs/5 * m/9) + (pairs/10 * m/10)) -
		((pairs/9 * m/10) + (pairs/12 * m/11) + (pairs/6 * m/9))
		]

	; calculate determinate
	det: (m/1 * m4inv-res/1) + 
		(m/2 * m4inv-res/2) + 
		(m/3 * m4inv-res/3) + 
		(m/4 * m4inv-res/4)

	; invert to avoid doing multiple divisions
	det: 1.0 / det

	result: copy []
	foreach x m4inv-res [ append result reduce ( x * det ) ]
	return r3d-transpose-m4 result  
	] ; end of r3d-inverse-m4


r3d-dotproduct: func [
	"Returns the dot product (a number) between two 3x1 vector"
	v1 [block!] "Vector  (3x1)"
	v2 [block!] "Vector (3x1)"
	/local result
	][result: reduce (v1/1 * v2/1) + (v1/2 * v2/2) + (v1/3 * v2/3)]


r3d-crossproduct: func [
	"Returns a cross product (vector, 3x1) between 2 vectors (3x1)"
	v1 [block!] "3x1 vector"
	v2 [block!] "3x1 vector"
	/local result
	][
	result: reduce [ 
		(v1/2 * v2/3) - (v1/3 * v2/2)
                    (v1/3 * v2/1) - (v1/1 * v2/3)
                    (v1/1 * v2/2) - (v1/2 * v2/1) 
		    ]
	return result            
	]

r3d-length: func [
	"Returns the disctance between origin and a point (3x1)"
	v [block!] "Point coordinate (3x1)"
	/local result
	][
	result: reduce square-root ( (v/1 * v/1) + (v/2 * v/2) + (v/3 * v/3) )
	]

r3d-Add: func [
	"Add a to b - a and b can be either matrices or vectors but types must match"
	a [block!]
	b [block!]
	/local item result
	][
	result: copy []
	for item 1 length? a 1 [	append result reduce ( pick a item ) + ( pick b item ) ]
	return result
	]

r3d-Subtract: func [
	"Subtract b from a - a and b can be either matrices or vectors but types must match"
	a [block!]
	b [block!]
	/local item result
	][
	result: copy []
	for item 1 length? a 1 [ append result reduce ( pick a item ) - ( pick b item ) ]
	return result
	]

r3d-Multiply: func [
	"Multiply a by n - a and b can be either a matrix or vector, n is decimal"
	a [block!]
	n [decimal!]
	/local item result
	][
	result: copy []
	for item 1 length? a 1 [  append result reduce ( pick a item ) * n  ]
	return result
	]

r3d-Divide: func [
	"Divide a by n - a and b can be either a matrix or vector, n is non-zero decimal"
	a [block!]
	n [decimal!]
	/local item result div
	][
	result: copy []
	if n = 0.0 [ Print "Divide by zero attempted" return a ]
	div: 1.0 / n    
	for item 1 length? a 1 [  append result reduce ( pick a item ) * div  ]
	return result
	]


r3d-normalise: func [
	"Normalize a vector, its module becomes 1"
	v [block!] "Vector 3x1"
	/local result
	][
	; try to normalise a zero vector and you will get a zero vector back
	len: r3d-length v
	either ( len = 0 ) [ 
		result: [ 0.0 0.0 0.0 ]
		] [
		result: reduce [
			v/1 / len 
			v/2 / len
			v/3 / len 
			]
		]
	return result   
	]

;Debug functions?
r3d-print-m4: func [
	m [block!]
	][
	foreach [ a b c d ] m [	print [ a b c d ]    ]
	print []
	]

r3d-print-v3: func [
	v [block!]
	][
	foreach elem v [  print elem  ]
	print []
	]

;-------------------------------------------------
;
; contents of r3d-engine.r  - r3d render engine
;


Render: func [
	"Main function that render 3D objects creating a VID DRAW"
	world [block!] "List of all object to render and their position"
	camera [block!] "Camera position, where it look and where is the UP"
	projection [block!] "Type of projection to use, 4x4 matrix, in doubt use:  r3d-perspective 250. You can create funny distortions..."  
	canvasSize [pair!] "Window size of the rendering"
	/no-cull
	/local result transvert trans2d model modelworld triangles
	][
	result: copy []
	triangles: copy []
	cameraInverse: r3d-inverse-m4 camera
	foreach r3dobject world [
		model: r3dObject/1
		modelworld: r3dobject/2
		objcolor: r3dObject/3
		ModelCamera: r3d-m4xm4  cameraInverse modelWorld 

		; transform the vertices into 3d space relative to the camera
		transVert: r3d-m4xv3-array model/1 modelcamera
		faces: model/2

		; faceinfo contains 2 blocks of n entries, a) face normals and b) furthest Z offset
		faceInfo: r3d-CalculateFaceNormals transVert faces

		; transform the vertices again using the projection matrix
		trans2d: r3d-m4xv3-array transvert projection
		;culling or not back face culling?
		either no-cull [append triangles r3d-Render2dTriangles-simple/no-cull trans2d faces faceInfo canvasSize objcolor
			][append triangles r3d-Render2dTriangles-simple trans2d faces faceInfo canvasSize objcolor]
		]
	; depth sort    
	triangles: sort/reverse triangles
	;probe triangles
	foreach triangle triangles [
		fillcolour: last triangle
		temptriang: copy triangle		
		remove temptriang
		reverse temptriang
		remove temptriang
		reverse temptriang
	
		either tuple? fillcolour [			
			; face is filled with a colour
			append result reduce ['pen fillcolour 'fill-pen fillcolour ]
			append result 'polygon
			foreach item temptriang [
				append result item
				]
			][	;image case				 			
				either (length? temptriang) = 4 [
					append result 'image
					foreach item temptriang [
						append result item
						]
					append result fillcolour	
					][
					append result reduce ['pen none 'fill-pen fillcolour ]
					append result 'polygon
					foreach item temptriang [
						append result item
						]
					]
				]
		;append result 'polygon
		;append result triangle/2
		;append result triangle/3
		;append result triangle/4			
		]
	;print form result
	return result
	]


r3d-CalculateFaceNormals: func [
	vertices [block!] 
	faces [block!]
	/local result depthvals v1 v2 v3 vtmp1 vtmp2 vcp largest 
	][
	result: copy []
	depthvals: copy []
	foreach face faces [
		; get the vertices pointed to by each index in the face
		v1: pick vertices face/1 
		v2: pick vertices face/2
		v3: pick vertices face/3
		; get face normal
		vtmp1: r3d-subtract v2 v1
		vtmp2: r3d-subtract v3 v2
        
		vcp: r3d-crossproduct vtmp1 vtmp2
		vcp: r3d-normalise vcp

		append/only result  vcp

		; get furthest z co-ord
		largest: -10000.0
		if v1/3 > largest [ largest: v1/3 ]
		if v2/3 > largest [ largest: v2/3 ]
		if v3/3 > largest [ largest: v3/3 ]
		append depthvals largest        
		]
	reduce [ result depthvals ]
	]

r3d-Render2dTriangles-Simple: func [
	transformedVertices [block!] 
	faces [block!]
	faceInfo [block!]
	canvasSize [pair!]
	objColor [tuple! image!]
	/no-cull
	/local result temptriangle v face index origin count facez
	][
	result: copy []
	; todo - accurate lighting
	; determine the origin
	origin: (canvasSize * 0.5)
	faceNormals: faceInfo/1
	depthvals: faceinfo/2
	count: 1    
	foreach face faces [
		; check if this face needs to be backface culled    
		facenormal: pick faceNormals count
		;print ["facenormal: " facenormal]
		depthval: pick depthvals count
		;print ["depthval: " depthval]
		count: count + 1
		faceZ: facenormal/3  ;backface culling (removing back faces)
		if no-cull [faceZ:  negate abs facez ] ;max
		if faceZ <= 0 [
			; make the depthval the first entry in the block so that the block will be depth sorted by this value
			temptriangle: copy []
			append temptriangle depthval
			foreach index face [
				; get the vertex pointed to by this index
				v: pick transformedVertices index 
				append temptriangle reduce ( origin + as-pair v/1 v/2 )
				]            
			; todo lighting
			facez: 0.1 - facez 
			
			either tuple? objColor [
				append temptriangle ( objColor * facez ) ;if it's a color
				][  
					facez2: to-integer (  facez * facez * 150)					
					append temptriangle to-image  layout/tight compose/deep [image (objColor)  effect [luma (facez2)]] ;else it's an image
					]
			append/only result temptriangle
			]
		]
	result
	]



r3d-Load-OFF: func [
	"Load 3D OFF files, and create the correspective model"
	file [file!] 
	/local bloc result verts faces numVerts numFaces numEdges temp_bl maxx minn
	] [
	bloc: read/lines file
	; first of all remove empty lines that can be found in old OFF files
	remove-each value bloc [empty? value]
	if not found? find bloc/1 "OFF" [Alert "Block is not an OFF file" return none exit]
	fileType: bloc/1
	bloc: next bloc ; next line
	;process possible header comment lines starting with #
	while [bloc/1/1 = #"#" ] [bloc: next bloc]
	;now we have the number of vertexes
	numVerts: to-integer first parse bloc/1 none		
	numfaces: to-integer second parse bloc/1 none
	;do we need it?
	numedges: to-integer third parse bloc/1 none
	
	result: copy []
	verts: copy []
	faces: copy []
	
	maxx: 0
	minn: 0
	loop numVerts [
		temp_bl: copy []
		bloc: next bloc ; next line
		line: parse bloc/1 none
		foreach item line [
			temp: to-decimal item	
			if temp > maxx [maxx: temp]
			if temp < minn [minn: temp]
			append temp_bl temp
			]
		append/only verts temp_bl
		]
	
	append/only result verts ; the first part of the model is done
	
	loop numfaces [
		temp_bl: copy []
		bloc: next bloc ; next line
		line: parse bloc/1 none	
		nv: to-integer line/1 ;number of vertexes to add
		loop nv [
			line: next line
			;off models start to counter from zero!
			append temp_bl ((to-integer line/1) + 1)
			]
		append/only faces temp_bl	
		]
	append/only result faces ; the second part of the model is done	
	append result (maxx - minn) ;this give an hit for scaling the model
	result 
	]


;----------------------------------------
;
; Load the base model 
;
;
; Note: R3d supports loading the .OFF file format through the r3d-load-off function
;
;How to create base model:
;all model are made of triangles, you must give point and triangles to show
;a model is a block with inside 2 blocks: vertices  and faces
;vertices contains vertices coordinates [x y z]
;faces set the triangle to show
;so [ 1 2 3] means to fill a triangle using point 1, 2 and 3
;THE ORDER OF THE POINTS IN FACE DESCRIPTION IS VERY IMPORTANT 
; [1 2 3] IS DIFFERENT THAN [ 3 2 1 ] 
;USE faces - anticlockwise winding





cube-model: [ 
    ; vertices
    [
        [ 1 1 0 ] ;point 1
        [ 1 -1 0 ] ;point 2
        [ -1 -1 0 ] ;point 3 and so on...
        [ -1 1 0 ]
        [ 1 1 1 ]
        [ 1 -1 1 ]
        [ -1 -1 1 ]
        [ -1 1 1 ]

    ]
    ; faces - anticlockwise winding
    [
        [ 1 5 6 ]
        [ 1 6 2 ]
        [ 2 6 7 ]
        [ 2 7 3 ]
        [ 3 7 8 ]
        [ 3 8 4 ]
        [ 4 8 5 ]
        [ 4 5 1 ]
        [ 8 7 6 ]
        [ 8 6 5 ]
        [ 1 2 3 ]
        [ 1 3 4 ]
    ]
]


cube2-model: [ 
    ; vertices
    [
        [ 0 0 0 ] ;point 1
        [ 1 0 0 ] ;point 2
        [ 1 1 0 ] ;point 3 and so on...
        [ 0 1 0 ]
        [ 0 0 1 ] ;point 5
        [ 1 0 1 ] ;point 6
        [ 1 1 1 ] ;point 7 and so on...
        [ 0 1 1 ]

    ]
    ; faces - anticlockwise winding
    [
        [ 4 3 2 1] ;just six face square, instead of 12 tirangles
        [ 5 6 7 8 ]
        [ 1 5 8 4 ]
        [ 1 2 6 5]
        [ 2 3 7 6 ]
        [ 8 7 3 4  ]        
    ]
]


pyramid-model: [
	;vertices
	[
		[1 0.87 2] ;1
		[0 0 0] ;2
		[2 0 0] ;3
		[1 1.73 0] ;4
	]
	; faces	
	[
		[4 2 1]
		[4 3 2] 
		[1 3 4] 
		[ 1 2 3]
	]
]



square-pyramid-model: [
	;vertices
	[
		[1  1 2] ;1
		[0 0 0] ;2
		[2 0 0] ;3
		[2 2 0] ;4
		[0 2 0] ; 5
	]
	; faces	
	[
		[1 5 2 ]
		[1 2 3]
		[1 3 4]
		[1 4 5]
		[3 2 5]
		[3 5 4]
	]
]

wall-model: [
	;vertices
	[
		[0 0 0] ;1
		[1 0 0] ;2
		[1 0 1 ] ;3
		[0 0 1 ] ;4
		
	]
	; faces	
	[
		[3 4 1 2 ]		
		
	]
]

prysm-8-model: [
	;vertices
	[
		[-0.414 -1 0] ;1
		[0.414 -1 0] ;2
		[1 -0.414 0 ] ;3
		[1 0.414 0 ] ;4
		[0.414  1 0] ;5
		[-0.414  1 0] ;6
		[-1 0.414 0 ] ;7
		[-1 -0.414 0 ] ;8
		[-0.414 -1 1] ;9
		[0.414 -1 1] ;10
		[1 -0.414 1 ] ;11
		[1 0.414 1 ] ;12
		[0.414  1 1] ;13
		[-0.414  1 1] ;14
		[-1 0.414 1 ] ;15
		[-1 -0.414 1 ] ;16
		
	]
	; faces	
	[
		[8 7 6 5 4 3 2 1 ]
		[9 10 11 12 13 14 15 16]
		[1 2 10 9]
		[2 3 11 10]
		[3 4 12 11]
		[4 5 13 12]
		[5 6 14 13]
		[6 7 15 14]
		[7 8 16 15]
		[8 1 9 16]		
	]
]
