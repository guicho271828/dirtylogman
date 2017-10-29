
dirty: roswell/dirty.ros Makefile
	ros dump --remove-docstrings --delete-debug-info --delete-macro-definitions --delete-compiler-macro-definitions --delete-compiler-information-sbcl executable $<
	mv $(basename $<) $@
	du -h dirty
