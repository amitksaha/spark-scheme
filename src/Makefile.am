AM_CPPFLAGS = -I../include -I../libs/mzs/src/mzscheme/include/ -I../libs/fltk -I../libs/sqlite

bin_PROGRAMS = spark

spark_SOURCES = spark.cpp spark_fltk_draw.cpp spark_fltk_pack.cpp spark_fltk_valuator.cpp spark_curl.cpp spark_fltk_event.cpp spark_fltk_positioner.cpp spark_fltk_widget.cpp spark_expat.cpp spark_fltk_file_chooser.cpp  spark_fltk_progress.cpp spark_fltk_window.cpp spark_fltk.cpp spark_fltk_group.cpp spark_fltk_scroll.cpp spark_fltk_wizard.cpp spark_fltk_ask.cpp spark_fltk_help_dialog.cpp spark_fltk_spinner.cpp spark_impl.cpp spark_fltk_browser.cpp spark_fltk_help_view.cpp spark_fltk_tabs.cpp spark_select.cpp spark_fltk_button.cpp spark_fltk_image.cpp spark_fltk_text_buffer.cpp spark_socket.cpp spark_fltk_chart.cpp spark_fltk_input.cpp spark_fltk_text_display.cpp spark_sqlite.cpp spark_fltk_clock_output.cpp spark_fltk_input_choice.cpp  spark_fltk_tile.cpp spark_sysinfo.cpp spark_fltk_color_chooser.cpp spark_fltk_menu.cpp spark_opengl.cpp spark_opengl_util.cpp spark_fcgi.cpp spark_odbc.cpp

LDADD = dl expat z Xext X11 m curl ssl pthread GL GLU odbc iconv

spark_LDADD = ../libs/mzs/src/mzscheme/libmzscheme.a ../libs/mzs/src/mzscheme/libmzgc.a ../libs/fltk/lib/libfltk.a ../libs/fltk/lib/libfltk_images.a ../libs/fltk/lib/libfltk_jpeg.a ../libs/fltk/lib/libfltk_png.a ../libs/fltk/lib/libfltk_gl.a ../libs/sqlite/sqlite3.o

install-exec-local:
	mkdir /usr/lib/spark
	cp -R ../libs/mzs/collects /usr/lib/spark
	cp -R ../scheme /usr/lib/spark
	chmod 777 -R /usr/lib/spark	
	cd ../scheme;spark -s ./spark-compile.ss
	rm -rf /usr/lib/spark/scheme
	cp -R ../scheme /usr/lib/spark
	chmod 777 -R /usr/lib/spark	

uninstall-local:
	rm -rf /usr/lib/spark
