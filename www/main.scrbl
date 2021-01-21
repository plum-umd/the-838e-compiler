#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
	  scribble/decode
	  scribble/html-properties
	  "defns.rkt"
	  "utils.rkt")

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
		     (decode-flow strs)))


@(define accessible
   (style #f (list (js-addition "js/accessibility.js")
		   (attributes '((lang . "en"))))))

@title[#:style accessible @courseno]{: Advanced Compilers}

@image[#:style float-right]{img/Dore-munchausen-illustration.jpg}

@emph{@string-titlecase[semester], @year}

@emph{Lectures: Tuesday & Thursday, 2:00pm Eastern - 3:15pm Eastern, Online}

@emph{Professor: @prof}

CMSC 838E is a graduate-level advanced compilers course.  Its major
goal is to design and build a complete modern programming language
implementation, including advanced research-level features.



@;{
@tabular[#:style 'boxed
	 #:row-properties '(bottom-border ())
	 (list* (list @bold{Staff} 'cont 'cont)
		(list @bold{Name} @elem{@bold{E-mail}} @elem{@bold{Hours}})
		(list prof prof-email "TBD EST")
		staff)]
}

@bold{Communications:} @link["https://discord.gg/VPuPxSxaMJ"]{Discord}

@bold{Assumptions:} As a graduate-level course, the major assumption
is that you are self-directed and motivated to pursue your own
educational goals and that you can collaborate with others.  Coming in
to this course, you should know how to program in a functional
programming language like OCaml and have some familiarity with
programming in C and Assembly. See the @seclink["Texts"]{Texts} page
for references to brush up on this material.  The course is built upon
the foundation of
@link["https://www.cs.umd.edu/classes/spring2021/cmsc430/"]{CMSC 430},
but it's not assumed you've taken the course; only that you can work
through the material quickly.

@bold{Disclaimer:} All information on this web page is tentative and subject to
change. Any substantive change will be accompanied with an announcement to the
class.

@include-section{syllabus.scrbl}
@include-section{texts.scrbl}
@;include-section{schedule.scrbl}
@include-section{notes.scrbl}
@;include-section{assignments.scrbl}
@;include-section{midterms.scrbl}
@include-section{project.scrbl}
@include-section{racket.scrbl}
