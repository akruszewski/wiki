#! /usr/local/bin/racket
#lang racket/base

(require racket/cmdline)
(require racket/file)
(require racket/match)

(define wiki-root-directory (getenv "WIKI_PATH"))

(define (wiki/get-page-path page-name)
    (path-add-extension
      (build-path wiki-root-directory page-name) #".md"))

(define (wiki/get-page-content page-path)
    (file->string page-path))

(define (wiki/create-page page-name)
    (display-to-file
      (string-append "#" page-name "#")
      (wiki/get-page-path page-name)))

(define (wiki/get-or-create-path page-name)
    (let ((page-path (wiki/get-page-path page-name)))
        (if (file-exists? page-path)
            page-path
            (cdr (cons (wiki/create-page page-name) page-path)))))

(define (wiki/list-pages-names wiki-directory)
    (map
      (lambda (x) (path->string (path-replace-extension x #"")))
      (directory-list wiki-directory)))

(define (wiki/list-pages-paths wiki-directory)
    (map
      (lambda (x) (path->string (path->complete-path x wiki-root-directory)))
      (directory-list wiki-directory)))

(define/match (wiki/run cmd . page-name)
    [("get" page-name)
     (printf "~a\n" (wiki/get-page-content
                      (wiki/get-or-create-path (car page-name))))]
    [("get-path" page-name)
     (printf "~a\n" (path->string (wiki/get-or-create-path (car page-name))))]
    [("list" '())
     (void (map
            (lambda (page) (printf "~a\n" page))
            (wiki/list-pages-names wiki-root-directory)))]
    [("list-paths" '())
     (void (map
            (lambda (page) (printf "~a\n" page))
            (wiki/list-pages-paths wiki-root-directory)))])

(apply wiki/run (vector->list (current-command-line-arguments)))
