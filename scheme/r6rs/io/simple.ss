(library (yuni scheme r6rs io simple)
  (export call-with-input-file call-with-output-file
          close-input-port close-output-port current-input-port current-output-port
          display eof-object? newline open-input-file open-output-file peek-char
          read read-char with-input-from-file with-output-to-file write write-char
          ;; AND SO ON
	  ;add
	  output-port? input-port? port?
          )
  (import 
    (yuni scheme r6rs)))
