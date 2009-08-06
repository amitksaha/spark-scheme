;; 3d utilities.
;; Copyright (C) 2007, 2008 Vijay Mathew Pandyalakal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; If not, see <http://www.gnu.org/licenses/>.

;; Please contact Vijay Mathew Pandyalakal if you need additional 
;; information or have any questions.
;; (Electronic mail: vijay.the.schemer@gmail.com)

(library airglow-3d-util

	 (import ((prefix spark.opengl:: #%spark-opengl))
		 (exception))

	 (export texparam->int texparamval->int capability->int
		 depthfunc->int polyface->int polymode->int
		 listmode->int fog-param->int fog-coord->const
		 materialtype->int pixeltype->int pixelformat->int
		 pixeldatatype->int pixelmap->int pixelpack->int
		 pixeltransfer->int)

	 (define (pixeltype->int t)
	   (if (integer? t)
	       t
	       (begin
		 (case t
		   ((color) spark.opengl::GL-COLOR)
		   ((stencil) spark.opengl::GL-STENCIL)
		   ((depth) spark.opengl::GL-DEPTH)
		   (else (raise-exception "pixeltype->int" 
					  "Invalic pixel type." null))))))

	 (define (pixelformat->int p)
	   (if (integer? p)
	       p
	       (begin
		 (case p
		   ((color-index) spark.opengl::GL-COLOR-INDEX)
		   ((luminance) spark.opengl::GL-LUMINANCE)
		   ((luminance-alpha) spark.opengl::GL-LUMINANCE-ALPHA)
		   ((rgb) spark.opengl::GL-RGB)
		   ((rgba) spark.opengl::GL-RGBA)
		   ((red) spark.opengl::GL-RED)
		   ((green) spark.opengl::GL-GREEN)
		   ((blue) spark.opengl::GL-BLUE)
		   ((alpha) spark.opengl::GL-ALPHA)
		   ((stencil-index) spark.opengl::GL-STENCIL-INDEX)
		   ((depth-component) spark.opengl::GL-DEPTH-COMPONENT)
		   (else (raise-exception "pixelformat->int"
					  "Invalid pixel format." null))))))

	 (define (pixeldatatype->int p)
	   (if (integer? p)
	       p
	       (begin
		 (case p
		   ((byte) spark.opengl::GL-BYTE)
		   ((ubyte) spark.opengl::GL-UNSIGNED-BYTE)
		   ((bitmap) spark.opengl::GL-BITMAP)
		   ((short) spark.opengl::GL-SHORT)
		   ((ushort) spark.opengl::GL-UNSIGNED-SHORT)
		   ((int) spark.opengl::GL-INT)
		   ((uint) spark.opengl::GL-UNSIGNED-INT)
		   ((float) spark.opengl::GL-FLOAT)
		   (else (raise-exception "pixeldatatype->int"
					  "Invalid pixel datatype."
					  null))))))

	 (define (pixeltransfer->int p)
	   (if (integer? p)
	       p
	       (begin
		 (case p
		   ((map-color) spark.opengl::GL-MAP-COLOR)
		   ((map-stencil) spark.opengl::GL-MAP-STENCIL)
		   ((index-shift) spark.opengl::GL-INDEX-SHIFT)
		   ((index-offset) spark.opengl::GL-INDEX-OFFSET)
		   ((red-scale) spark.opengl::GL-RED-SCALE)
		   ((red-bias) spark.opengl::GL-RED-BIAS)
		   ((green-scale) spark.opengl::GL-GREEN-SCALE)
		   ((green-bias) spark.opengl::GL-GREEN-BIAS)
		   ((blue-scale) spark.opengl::GL-BLUE-SCALE)
		   ((blue-bias) spark.opengl::GL-BLUE-BIAS)
		   ((alpha-scale) spark.opengl::GL-ALPHA-SCALE)
		   ((alpha-bias) spark.opengl::GL-ALPHA-BIAS)
		   ((depth-scale) spark.opengl::GL-DEPTH-SCALE)
		   ((depth-bias) spark.opengl::GL-DEPTH-BIAS)
		   (else (raise-exception "pixeltransfer->int"
					  "Invalid pixel transfer mode." null))))))

	 (define (pixelmap->int p)
	   (if (integer? p)
	       p
	       (begin
		 (case p
		   ((i-to-i) spark.opengl::GL-PIXEL-MAP-I-TO-I)
		   ((s-to-s) spark.opengl::GL-PIXEL-MAP-S-TO-S)
		   ((i-to-r) spark.opengl::GL-PIXEL-MAP-I-TO-R)
		   ((i-to-g) spark.opengl::GL-PIXEL-MAP-I-TO-G)
		   ((i-to-b) spark.opengl::GL-PIXEL-MAP-I-TO-B)
		   ((i-to-a) spark.opengl::GL-PIXEL-MAP-I-TO-A)
		   ((r-to-r) spark.opengl::GL-PIXEL-MAP-R-TO-R)
		   ((g-to-g) spark.opengl::GL-PIXEL-MAP-G-TO-G)
		   ((b-to-b) spark.opengl::GL-PIXEL-MAP-B-TO-B)
		   ((a-to-a) spark.opengl::GL-PIXEL-MAP-A-TO-A)
		   (else (raise-exception "pixelmap->int"
					  "Invalid pixel map." null))))))

	 (define (pixelpack->int p)
	   (if (integer? p)
	       p
	       (begin
		 (case p
		   ((swap-bytes) spark.opengl::GL-PACK-SWAP-BYTES)
		   ((lsb-first) spark.opengl::GL-PACK-LSB-FIRST)
		   ((row-length) spark.opengl::GL-PACK-ROW-LENGTH)
		   ((skip-pixels) spark.opengl::GL-PACK-SKIP-PIXELS)
		   ((skip-rows) spark.opengl::GL-PACK-SKIP-ROWS)
		   ((swap-bytes) spark.opengl::GL-PACK-SWAP-BYTES)
		   ((alignment) spark.opengl::GL-PACK-ALIGNMENT)
		   ((swap-bytes~) spark.opengl::GL-UNPACK-SWAP-BYTES)
		   ((lsb-first~) spark.opengl::GL-UNPACK-LSB-FIRST)
		   ((row-length~) spark.opengl::GL-UNPACK-ROW-LENGTH)
		   ((skip-pixels~) spark.opengl::GL-UNPACK-SKIP-PIXELS)
		   ((skip-rows~) spark.opengl::GL-UNPACK-SKIP-ROWS)
		   ((swap-bytes~) spark.opengl::GL-UNPACK-SWAP-BYTES)
		   ((alignment~) spark.opengl::GL-UNPACK-ALIGNMENT)
		   (else (raise-exception "pixelpack->int"
					  "Invalid pixel pack option." null))))))

	 (define (texparam->int p)
	   (if (integer? p)
	       p
	       (begin
		 (case p
		   ((min-filter) spark.opengl::GL-TEXTURE-MIN-FILTER)
		   ((mag-filter) spark.opengl::GL-TEXTURE-MAG-FILTER)
		   ((wrap-s) spark.opengl::GL-TEXTURE-WRAP-S)
		   ((wrap-t) spark.opengl::GL-TEXTURE-WRAP-T)
		   ((priority) spark.opengl::GL-TEXTURE-PRIORITY)
		   (else (raise-exception "texparam->int" "Invalid param." null))))))

	 (define (texparamval->int v)
	   (if (or (integer? v)
		   (real? v))
	       v
	       (begin
		 (case v
		   ((nearest) spark.opengl::GL-NEAREST)
		   ((linear) spark.opengl::GL-LINEAR)
		   ((nearest-mipmap-nearest) spark.opengl::GL-NEAREST-MIPMAP-NEAREST)
		   ((linear-mipmap-nearest) spark.opengl::GL-LINEAR-MIPMAP-NEAREST)
		   ((nearest-mipmap-linear) spark.opengl::GL-NEAREST-MIPMAP-LINEAR)
		   ((linear-mipmap-linear) spark.opengl::GL-LINEAR-MIPMAP-LINEAR)
		   ((clamp) spark.opengl::GL-CLAMP)
		   ((repeat) spark.opengl::GL-REPEAT)
		   (else (raise-exception "texparamval->int" "Invalid param." null))))))

	 (define (capability->int cap)
	   (if (integer? cap)
	       cap
	       (begin
		 (case cap
		   ((alpha-test) spark.opengl::GL-ALPHA-TEST)
		   ((auto-normal) spark.opengl::GL-AUTO-NORMAL)
		   ((blend) spark.opengl::GL-BLEND)
		   ((color-logic-op) spark.opengl::GL-COLOR-LOGIC-OP)
		   ((color-material) spark.opengl::GL-COLOR-MATERIAL)
		   ((color-table) spark.opengl::GL-COLOR-TABLE)
		   ((convolution-1d) spark.opengl::GL-CONVOLUTION-1D)
		   ((convolution-2d) spark.opengl::GL-CONVOLUTION-2D)
		   ((cull-face) spark.opengl::GL-CULL-FACE)
		   ((depth-test) spark.opengl::GL-DEPTH-TEST)
		   ((dither) spark.opengl::GL-DITHER)
		   ((fog) spark.opengl::GL-FOG)
		   ((histogram) spark.opengl::GL-HISTOGRAM)
		   ((index-logic-op) spark.opengl::GL-INDEX-LOGIC-OP)
		   ((lighting) spark.opengl::GL-LIGHTING)
		   ((line-smooth) spark.opengl::GL-LINE-SMOOTH)
		   ((line-stipple) spark.opengl::GL-LINE-STIPPLE)
		   ((map1-color-4) spark.opengl::GL-MAP1-COLOR-4)
		   ((map1-index) spark.opengl::GL-MAP1-INDEX)
		   ((map1-normal) spark.opengl::GL-MAP1-NORMAL)
		   ((map1-texture-coord-1) spark.opengl::GL-MAP1-TEXTURE-COORD-1)
		   ((map1-texture-coord-2) spark.opengl::GL-MAP1-TEXTURE-COORD-2)
		   ((map1-texture-coord-3) spark.opengl::GL-MAP1-TEXTURE-COORD-3)
		   ((map1-texture-coord-4) spark.opengl::GL-MAP1-TEXTURE-COORD-4)
		   ((map1-vertex-3) spark.opengl::GL-MAP1-VERTEX-3)
		   ((map1-vertex-4) spark.opengl::GL-MAP1-VERTEX-4)
		   ((map2-color-4) spark.opengl::GL-MAP2-COLOR-4)
		   ((map2-index) spark.opengl::GL-MAP2-INDEX)
		   ((map2-normal) spark.opengl::GL-MAP2-NORMAL)
		   ((map2-texture-coord-1) spark.opengl::GL-MAP2-TEXTURE-COORD-1)
		   ((map2-texture-coord-2) spark.opengl::GL-MAP2-TEXTURE-COORD-2)
		   ((map2-texture-coord-3) spark.opengl::GL-MAP2-TEXTURE-COORD-3)
		   ((map2-texture-coord-4) spark.opengl::GL-MAP2-TEXTURE-COORD-4)
		   ((map2-vertex-3) spark.opengl::GL-MAP2-VERTEX-3)
		   ((map2-vertex-4) spark.opengl::GL-MAP2-VERTEX-4)
		   ((minmax) spark.opengl::GL-MINMAX)
		   ((normalize) spark.opengl::GL-NORMALIZE)
		   ((point-smooth) spark.opengl::GL-POINT-SMOOTH)
		   ((polygon-offset-fill) spark.opengl::GL-POLYGON-OFFSET-FILL)
		   ((polygon-offset-line) spark.opengl::GL-POLYGON-OFFSET-LINE)
		   ((polygon-offset-point) spark.opengl::GL-POLYGON-OFFSET-POINT)
		   ((polygon-smooth) spark.opengl::GL-POLYGON-SMOOTH)
		   ((polygon-stipple) spark.opengl::GL-POLYGON-STIPPLE)
		   ((post-color-matrix-color-table) spark.opengl::GL-POST-COLOR-MATRIX-COLOR-TABLE)
		   ((post-convolution-color-table) spark.opengl::GL-POST-CONVOLUTION-COLOR-TABLE)
		   ((rescale-normal) spark.opengl::GL-RESCALE-NORMAL)
		   ((separable-2d) spark.opengl::GL-SEPARABLE-2D)
		   ((scissor-test) spark.opengl::GL-SCISSOR-TEST)
		   ((stencil-test) spark.opengl::GL-STENCIL-TEST)
		   ((texture-1d) spark.opengl::GL-TEXTURE-1D)
		   ((texture-2d) spark.opengl::GL-TEXTURE-2D)
		   ((texture-3d) spark.opengl::GL-TEXTURE-3D)
		   ((texture-gen-q) spark.opengl::GL-TEXTURE-GEN-Q)
		   ((texture-gen-r) spark.opengl::GL-TEXTURE-GEN-R)
		   ((texture-gen-s) spark.opengl::GL-TEXTURE-GEN-S)
		   ((texture-gen-t) spark.opengl::GL-TEXTURE-GEN-T)
		   (else (raise-exception "capability->int" "Invalid capability." null))))))

	 (define (depthfunc->int f)
	   (if (integer? f)
	       f
	       (begin
		 (case f
		   ((never) spark.opengl::GL-NEVER)
		   ((less) spark.opengl::GL-LESS)
		   ((equal) spark.opengl::GL-EQUAL)
		   ((lequal) spark.opengl::GL-LEQUAL)
		   ((greater) spark.opengl::GL-GREATER)
		   ((not-equal) spark.opengl::GL-NOTEQUAL)
		   ((gequal) spark.opengl::GL-GEQUAL)
		   ((always) spark.opengl::GL-ALWAYS)
		   (else (raise-exception "depthfunc->int" "Invalid depth function." null))))))

	 (define (polyface->int face)
	   (if (integer? face)
	       face
	       (begin
		 (case face
		   ((front) spark.opengl::GL-FRONT)
		   ((back) spark.opengl::GL-BACK)
		   ((front-and-back) spark.opengl::GL-FRONT-AND-BACK)
		   (else (raise-exception "polyface->int" "Invalid polyface." null))))))

	 (define (polymode->int m)
	   (if (integer? m)
	       m
	       (begin
		 (case m
		   ((point) spark.opengl::GL-POINT)
		   ((line) spark.opengl::GL-LINE)
		   ((fill) spark.opengl::GL-FILL)
		   (else (raise-exception "polymode->int" "Invalid polymode." null))))))

	 (define (materialtype->int m)
	   (if (integer? m)
	       m
	       (begin
		 (case m
		   ((emission) spark.opengl::GL-EMISSION)
		   ((ambient) spark.opengl::GL-AMBIENT)
		   ((specular) spark.opengl::GL-SPECULAR)
		   ((diffuse) spark.opengl::GL-DIFFUSE)
		   ((shininess) spark.opengl::GL-SHININESS)
		   (else (raise-exception "materialtype->int"
					  "Invalid material type." null))))))

	 (define (listmode->int m)
	   (if (integer? m)
	       m
	       (begin
		 (case m
		   ((compile) spark.opengl::GL-COMPILE)
		   ((compile-and-execute) spark.opengl::GL-COMPILE-AND-EXECUTE)
		   (else (raise-exception "listmode->int" "Invalid listmode." null))))))

	 (define (fog-param->int p)
	   (if (integer? p)
	       p
	       (begin
		 (case p
		   ((fog-mode) spark.opengl::GL-FOG-MODE)
		   ((fog-density) spark.opengl::GL-FOG-DENSITY)
		   ((fog-start) spark.opengl::GL-FOG-START)
		   ((fog-end) spark.opengl::GL-FOG-END)
		   ((fog-index) spark.opengl::GL-FOG-INDEX)
		   ((fog-color) spark.opengl::GL-FOG-COLOR)
		   ((fog-coord-src) spark.opengl::GL-FOG-COORD-SRC)
		   (else (raise-exception "fog-param->int" "Invalid fog param." null))))))

	 (define (fog-coord->const c)
	   (if (or (integer? c)
		   (real? c)
		   (list? c))
	       c
	       (begin
		 (case c
		   ((fog-coord) spark.opengl::GL-FOG-COORD)
		   ((fragment-depth) spark.opengl::GL-FRAGMENT-DEPTH)
		   (else (raise-exception "fog-coord->const" "Invalid fog-coord." null)))))))


