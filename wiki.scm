(import (chicken io))
(import (chicken file))
(import (chicken file posix))
(import filepath)
(import (chicken process-context))

(define wiki-root-directory (get-environment-variable "WIKI_PATH"))

(define (wiki/get-page-path page-name)
    (filepath:add-extension
      (filepath:join-path (list wiki-root-directory page-name)) "md"))

(define (wiki/get-page-content page-name)
    (call-with-input-file 
     (wiki/get-page-path page-name) (lambda (port) (read-string #f port))))

(define (wiki/create-page page-name)
    (let ((f (file-open 
              (wiki/get-page-path page-name) (+ open/creat open/wronly))))
         (file-write f (string-append "#" page-name "#"))
         (file-close f)))

(define (wiki/get-or-create-path page-name)
    (let ((page-path (wiki/get-page-path page-name)))
        (if (file-exists? page-path) page-path (wiki/create-page page-name))))

(define (wiki/list-pages-names wiki-directory)
    (map filepath:drop-extension (directory wiki-directory)))

(define (wiki/list-pages-paths wiki-directory)
    (map
      (lambda (x) (filepath:join-path (list wiki-root-directory x)))
      (directory wiki-directory)))

(define (wiki/usage)
    (print "wiki is simple wiki utility")
    (newline)
    (print "Arguments:")
    (print "\tget PAGE_NAME                - get wiki page")
    (print "\tget-or-create PAGE_NAME      - get or create wiki page")
    (print "\tget-path PAGE_NAME           - get wiki page path")
    (print "\tget-or-create-path PAGE_NAME - get or create wiki page")
    (print "\tlist                         - list all pages names")
    (print "\tlist-paths                   - list all pages names")
    (print "\thelp                         - show this page"))

(define wiki/run 
    (case-lambda
     (() (wiki/usage))
     ((command-name arg)
      (cond ((equal? command-name "get")
             (print (wiki/get-page-content arg)))
            ((equal? command-name "get-or-create")
             (cdr (list (wiki/get-or-create-path arg)
                        (print (wiki/get-page-content arg)))))
            ((equal? command-name "get-path")
             (print (wiki/get-page-path arg)))
            ((equal? command-name "get-or-create-path")
             (print (wiki/get-or-create arg)))
            (else
             (print "Command not found!"))))
     ((command-name) 
      (cond ((equal? command-name "list")
             (map print (wiki/list-pages-names wiki-root-directory)))
            ((equal? command-name "list-paths")
             (map print (wiki/list-pages-paths wiki-root-directory)))
            ((equal? command-name "help")
             (wiki/usage))
            (else (print "Command not found"))))))

(if (not wiki-root-directory)
    (print "WIKI_PATH env var is not set!")
    (apply wiki/run (cdr (argv))))
