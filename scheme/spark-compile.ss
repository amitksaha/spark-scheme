;; Compiles all spark modules.
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

(require (lib "compile.ss"))

(print (compile-file "spark-global.ss")) (newline)
(print (compile-file "spark-lib-manager.ss")) (newline)
(print (compile-file "ext/spark-lang-ext.ss")) (newline)
(print (compile-file "ext/spark-util-ext.ss")) (newline)
(print (compile-file "ext/spark-string-ext.ss")) (newline)
(print (compile-file "ext/spark-numeric-ext.ss")) (newline)
(print (compile-file "ext/spark-list-ext.ss")) (newline)
;;(print (compile-file "ext/spark-hash-map.ss")) (newline)
(print (compile-file "ext/spark-stack.ss")) (newline)
(print (compile-file "ext/spark-async.ss")) (newline)
(print (compile-file "ext/spark-process.ss")) (newline)
(print (compile-file "spark-global-provide.ss")) (newline)

(load-relative "ext/spark-class.ss")
(load-relative "ext/spark-ffi.ss")
(load-relative "ext/spark-pregexp.ss")
(load-relative "ext/spark-match.ss")
(load-relative "ext/spark-compile.ss")

(load-relative "net/address.ss")
(load-relative "net/socket.ss")
(load-relative "net/curl.ss")
(load-relative "net/net.ss")

(load-relative "fcgi/fcgi.ss")

(load-relative "reactor/handler.ss")
(load-relative "reactor/select-reactor.ss")
(load-relative "reactor/socket-acceptor.ss")
(load-relative "reactor/socket-connector.ss")
(load-relative "reactor/reactor.ss")

(load-relative "sunit/sunit.ss")

(load-relative "sql/sqlite/db.ss")
(load-relative "sql/odbc/db.ss")

(load-relative "xml/xml-parser.ss")

(load-relative "airglow/util.ss")
(load-relative "airglow/image.ss")
(load-relative "airglow/airglow-base.ss")
(load-relative "airglow/widget.ss")
(load-relative "airglow/group.ss")
(load-relative "airglow/window.ss")
(load-relative "airglow/button.ss")
(load-relative "airglow/border.ss")
(load-relative "airglow/browser.ss")
(load-relative "airglow/pack.ss")
(load-relative "airglow/input-field.ss")
(load-relative "airglow/color-chooser.ss")
(load-relative "airglow/menu.ss")
(load-relative "airglow/chart.ss")
(load-relative "airglow/scroll.ss")
(load-relative "airglow/progress.ss")
(load-relative "airglow/tabs.ss")
(load-relative "airglow/wizard.ss")
(load-relative "airglow/text-buffer.ss")
(load-relative "airglow/text-editor.ss")
(load-relative "airglow/input-choice.ss")
(load-relative "airglow/file-chooser.ss")
(load-relative "airglow/help-view.ss")
(load-relative "airglow/help-dialog.ss")
(load-relative "airglow/event.ss")
(load-relative "airglow/dnd.ss")
(load-relative "airglow/graphics.ss")
(load-relative "airglow/valuator.ss")
(load-relative "airglow/clock.ss")
(load-relative "airglow/ask.ss")
(load-relative "airglow/airglow.ss")

(load-relative "airglow/3d/util.ss")
(load-relative "airglow/3d/rendering.ss")
(load-relative "airglow/3d/transform.ss")
(load-relative "airglow/3d/geometry.ss")
(load-relative "airglow/3d/shaders.ss")
(load-relative "airglow/3d/extras.ss")
(load-relative "airglow/3d/camera.ss")
(load-relative "airglow/3d/texturing.ss")
(load-relative "airglow/3d/lighting.ss")
(load-relative "airglow/3d/airglow-3d.ss")

(load-relative "aura/sgml.ss")
(load-relative "aura/aura.ss")

(load-relative "http/url-encode.ss")
(load-relative "http/globals.ss")
(load-relative "http/session.ss")
(load-relative "http/request-parser.ss")
(load-relative "http/resource-loader.ss")
(load-relative "http/mime-types.ss")
(load-relative "http/response.ss")
(load-relative "http/web-server.ss")
(load-relative "http/http.ss")

(print (compile-file "ext/spark-class.ss")) (newline)
(print (compile-file "ext/spark-ffi.ss")) (newline)
(print (compile-file "ext/spark-pregexp.ss")) (newline)
(print (compile-file "ext/spark-match.ss")) (newline)
(print (compile-file "ext/spark-compile.ss")) (newline)

(print (compile-file "common/exception.ss")) (newline)
(print (compile-file "common/asserts.ss")) (newline)
(print (compile-file "common/util.ss")) (newline)

(print (compile-file "net/address.ss")) (newline)
(print (compile-file "net/socket.ss")) (newline)
(print (compile-file "net/curl.ss")) (newline)
(print (compile-file "net/net.ss")) (newline)

(print (compile-file "fcgi/fcgi.ss")) (newline)

(print (compile-file "reactor/handler.ss")) (newline)
(print (compile-file "reactor/select-reactor.ss")) (newline)
(print (compile-file "reactor/socket-acceptor.ss")) (newline)
(print (compile-file "reactor/socket-connector.ss")) (newline)
(print (compile-file "reactor/reactor.ss")) (newline)

(print (compile-file "sunit/sunit.ss")) (newline)

(print (compile-file "sql/sqlite/db.ss")) (newline)
(print (compile-file "sql/odbc/db.ss")) (newline)

(print (compile-file "xml/xml-parser.ss")) (newline)

(print (compile-file "airglow/util.ss")) (newline)
(print (compile-file "airglow/image.ss")) (newline)
(print (compile-file "airglow/airglow-base.ss")) (newline)
(print (compile-file "airglow/widget.ss")) (newline)
(print (compile-file "airglow/group.ss")) (newline)
(print (compile-file "airglow/window.ss")) (newline)
(print (compile-file "airglow/button.ss")) (newline)
(print (compile-file "airglow/border.ss")) (newline)
(print (compile-file "airglow/browser.ss")) (newline)
(print (compile-file "airglow/pack.ss")) (newline)
(print (compile-file "airglow/input-field.ss")) (newline)
(print (compile-file "airglow/color-chooser.ss")) (newline)
(print (compile-file "airglow/menu.ss")) (newline)
(print (compile-file "airglow/chart.ss")) (newline)
(print (compile-file "airglow/scroll.ss")) (newline)
(print (compile-file "airglow/progress.ss")) (newline)
(print (compile-file "airglow/tabs.ss")) (newline)
(print (compile-file "airglow/wizard.ss")) (newline)
(print (compile-file "airglow/text-buffer.ss")) (newline)
(print (compile-file "airglow/text-editor.ss")) (newline)
(print (compile-file "airglow/input-choice.ss")) (newline)
(print (compile-file "airglow/file-chooser.ss")) (newline)
(print (compile-file "airglow/help-view.ss")) (newline)
(print (compile-file "airglow/help-dialog.ss")) (newline)
(print (compile-file "airglow/event.ss")) (newline)
(print (compile-file "airglow/dnd.ss")) (newline)
(print (compile-file "airglow/graphics.ss")) (newline)
(print (compile-file "airglow/valuator.ss")) (newline)
(print (compile-file "airglow/clock.ss")) (newline)
(print (compile-file "airglow/ask.ss")) (newline)
(print (compile-file "airglow/airglow.ss")) (newline)

;; 3d
(print (compile-file "airglow/3d/util.ss")) (newline)
(print (compile-file "airglow/3d/rendering.ss")) (newline)
(print (compile-file "airglow/3d/transform.ss")) (newline)
(print (compile-file "airglow/3d/geometry.ss")) (newline)
(print (compile-file "airglow/3d/shaders.ss")) (newline)
(print (compile-file "airglow/3d/extras.ss")) (newline)
(print (compile-file "airglow/3d/camera.ss")) (newline)
(print (compile-file "airglow/3d/texturing.ss")) (newline)
(print (compile-file "airglow/3d/lighting.ss")) (newline)
(print (compile-file "airglow/3d/airglow-3d.ss")) (newline)

;; aura
(print (compile-file "aura/sgml.ss")) (newline)
(print (compile-file "aura/aura.ss")) (newline)

;; http
(print (compile-file "http/url-encode.ss")) (newline)
(print (compile-file "http/globals.ss")) (newline)
(print (compile-file "http/session.ss")) (newline)
(print (compile-file "http/request-parser.ss")) (newline)
(print (compile-file "http/resource-loader.ss")) (newline)
(print (compile-file "http/mime-types.ss")) (newline)
(print (compile-file "http/response.ss")) (newline)
(print (compile-file "http/web-server.ss")) (newline)
(print (compile-file "http/http.ss")) (newline)

