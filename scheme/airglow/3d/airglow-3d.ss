;; Common entry point for all airglow-3d modules.
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

(library airglow-3d

	(import (airglow-3d-rendering) (airglow-3d-transform)
		(airglow-3d-geometry) (airglow-3d-shaders)
		(airglow-3d-extras) (airglow-3d-camera)
		(airglow-3d-texturing) (airglow-3d-lighting))

	(export 
	 (all-from airglow-3d-rendering)
	 (all-from airglow-3d-transform)
	 (all-from airglow-3d-geometry)
	 (all-from airglow-3d-shaders)
	 (all-from airglow-3d-extras)
	 (all-from airglow-3d-camera)
	 (all-from airglow-3d-lighting)
	 (all-from airglow-3d-texturing)))
	 
