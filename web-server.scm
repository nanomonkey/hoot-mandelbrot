(use-modules (ice-9 format)
             (ice-9 ftw)
             (ice-9 hash-table)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 binary-ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (sxml simple)
             (web server)
             (web request)
             (web response)
             (web uri))

(define %mime-types
  (alist->hash-table
   '(("js" . application/javascript)
     ("html" . text/html)
     ("css" . text/css)
     ("wasm" . application/wasm)
     ("png" . image/png)
     ("wav" . audio/wav))))

(define (file-extension file)
  "Return the extension of FILE or #f if there is none."
  (let ((dot (string-rindex file #\.)))
    (and dot (substring file (+ 1 dot) (string-length file)))))

(define (mime-type file-name)
  "Guess the MIME type for FILE-NAME based upon its file extension."
  (or (hash-ref %mime-types (file-extension file-name))
      'text/plain))

(define (stat:directory? stat)
  "Return #t if STAT is a directory."
  (eq? (stat:type stat) 'directory))

(define (directory? file-name)
  "Return #t if FILE-NAME is a directory."
  (stat:directory? (stat file-name)))

(define (directory-contents dir)
  "Return a list of the files contained within DIR."
  (define name+directory?
    (match-lambda
     ((name stat)
      (list name (stat:directory? stat)))))

  (define (same-dir? other stat)
    (string=? dir other))

  (match (file-system-tree dir same-dir?)
    ;; We are not interested in the parent directory, only the
    ;; children.
    ((_ _ children ...)
     (map name+directory? children))))

(define (work-dir+path->file-name work-dir path)
  "Convert the URI PATH to an absolute file name relative to the
directory WORK-DIR."
  (string-append work-dir path))

(define (request-path-components request)
  "Split the URI path of REQUEST into a list of component strings.  For
example: \"/foo/bar\" yields '(\"foo\" \"bar\")."
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (request-file-name request)
  "Return the relative file name corresponding to the REQUEST URI."
  (let ((components (request-path-components request)))
    (if (null? components)
        "/"
        (string-join components "/" 'prefix))))

(define (resolve-file-name file-name)
  "If FILE-NAME is a directory with an 'index.html' file,
return that file name.  If FILE-NAME does not exist, return #f.
Otherwise, return FILE-NAME as-is."
  (let ((index-file-name (string-append file-name "/index.html")))
    (cond
     ((file-exists? index-file-name) index-file-name)
     ((file-exists? file-name) file-name)
     (else #f))))

(define (render-file file-name)
  "Return a 200 OK HTTP response that renders the contents of
FILE-NAME."
  (values `((content-type . (,(mime-type file-name))))
          (call-with-input-file file-name get-bytevector-all)))

(define (render-directory path dir)
  "Render the contents of DIR represented by the URI PATH."
  (define (concat+uri-encode . file-names)
    "Concatenate FILE-NAMES, preserving the correct file separators."
    (string-join (map uri-encode
                      (remove string-null?
                              (append-map (cut string-split <> #\/) file-names)))
                 "/" 'prefix))

  (define render-child
    (match-lambda
     ((file-name directory?)
      `(li
        (a (@ (href ,(concat+uri-encode path file-name)))
           ,(if directory?
                (string-append file-name "/")
                file-name))))))

  (define file-name<
    (match-lambda*
     (((name-a _) (name-b _))
      (string< name-a name-b))))

  (let* ((children (sort (directory-contents dir) file-name<))
         (title (string-append "Directory listing for " path))
         (view `(html
                (head
                 (title ,title))
                (body
                 (h1 ,title)
                 (ul ,@(map render-child children))))))
    (values '((content-type . (text/html)))
            (lambda (port)
              (display "<!DOCTYPE html>" port)
              (sxml->xml view port)))))

(define (not-found path)
  "Return a 404 not found HTTP response for PATH."
  (values (build-response #:code 404)
          (string-append "Resource not found: " path)))

(define (serve-file work-dir path)
  "Return an HTTP response for the file represented by PATH."
  (match (resolve-file-name
          (work-dir+path->file-name work-dir path))
    (#f (not-found path))
    ((? directory? dir)
     (render-directory path dir))
    (file-name
     (render-file file-name))))

(define (make-handler work-dir)
  (lambda (request body)
    "Serve the file asked for in REQUEST."
    (format #t "~a ~a~%"
            (request-method request)
            (uri-path (request-uri request)))
    (serve-file work-dir (request-file-name request))))

(define* (serve work-dir #:key (open-params '()))
  "Run a simple HTTP server that serves files in WORK-DIR."
  (run-server (make-handler work-dir) 'http open-params))

(let ((port 8088))
  (format #t "Serving on http://localhost:~a\n" port)
  (serve (getcwd) #:open-params `(#:port ,port #:addr ,INADDR_ANY)))
