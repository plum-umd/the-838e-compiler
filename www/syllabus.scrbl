#lang scribble/manual
@(require scribble/core
          "defns.rkt")

@;provide[exam-table]

@title[#:style 'unnumbered]{Syllabus}

@local-table-of-contents[]

@section{Prerequisites and Description}

@bold{Prerequisite:} a grade of C or better in CMSC330; and permission
of instructor; or CMSC graduate student.

@bold{Credits:} 3.

@courseno is a graduate-level advanced compilers course.  Its major
goal is to design and build a complete modern programming language
implementation, including advanced research-level features.
Throughout the course, students will collaboratively design and
implement a high-level, memory safe programming languages with modern
features drawn from the programming language literature, incorporated
into a compiler that target the x86 CPU architecture

The course assumes familiarity with a functional programming such as
OCaml from CMSC 330, and, to a lesser extent, imperative programming
in C and Assembly as covered in CMSC 216.  Since the course builds
significantly on
@link["https://www.cs.umd.edu/class/spring2021/cmsc430"]{CMSC 430}, it
is recommended undergraduate students have already completed 430 and
that graduate students have taken a compilers course before, however
in either case, highly motivated students should be able to keep up
with the material without this background.

@section{Course Workflow}

This course will be run as a collaborative research ``workshop.''
There will not be traditional lectures beyond the first few covering
the background on functional programming and compilation.  Once this
background is covered, we will work collectively to accomplish goals
we establish for our compiler.  Students will work both in small teams
and collectively with the whole class.  Class time will be spent
covering research papers relevant to our goals, discussing technical
issues, and reviewing our collective work.

Outside of the scheduled class time, we will communicate primarily via
Discord and Github.

The discord server is there for you to organize as a class, ask
questions of each other, and to get help from eachother and the
instructor. 

@section{Topics}

This course will be intentionally open-ended; we will collectively
decide what to explore, but some potential topics include:

@itemlist[
  @item{Overview of compilation}
  @item{Operational semantics}
  @item{Interpreters}
  @item{Intermediate representations and bytecode}
  @item{Code generation}
  @item{Run-time systems}
  @item{Garbage collection}
  @item{Type systems, type soundness, type inference}
  @item{Register allocation and optimization}
  @item{Language design}
  @item{Memory safety}
  @item{Module systems}
  @item{Macro systems}
  @item{Foreignn interfaces}
  @item{Alternative backends}
  @item{Contracts}
  @item{Gradual types}]

@section{Grading}

This is a graduate course.  If you're in for the grade, you're not
doing it right.

Grades will be assigned based on your effort to engage with the
material and collaboration with classmates.

@section[#:tag "syllabus-videos"]{Class meetings}

We will use the scheduled class times to meet synchronously via Zoom.

@section[#:tag "syllabus-project"]{Project}

There will be small projects undertaken throughout the semester and a
final more substantial project to be delivered by the end of the
course.  All projects will be collaborative, although some will
involve working individually or small teams.  For the final projects,
students will have freedom to design their own project and set their
own goals.

@section{Computing Resources}

Students are expected to use their own systems or resources provided
by the department or university to develop the deliverables for this
course.

We will use git and Github for revision control, continuous
integration, and deployment.  Please make sure everything you build
works on these systems.

@section{Outside-of-class communication}

Asynchronous communication will be done via Discord.  Please check in
to the Discord server at least once per weekday at a bare minimum.


@section{Students with Disabilities}

Students with disabilities who have been certified by Disability
Support Services as needing any type of special accommodations should
see the instructor as soon as possible during the schedule adjustment
period (the first two weeks of class). Please provide DSS's letter of
accommodation to the instructor at that time.

All arrangements for exam accommodations as a result of disability
@bold{must} be made and arranged with the instructor @bold{at least}
three business days prior to the exam date; later requests (including
retroactive ones) will be refused.

@section{University of Maryland Policies for Students}

Please read the university's guide on
@link["https://www.ugst.umd.edu/courserelatedpolicies.html"]{Course
Related Policies}, which provides you with resources and information
relevant to your participation in a UMD course.

Graduate students should also be familiar with the Graduate School's
@link["https://academiccatalog.umd.edu/graduate/policies/academic-record/"]{Academic
Policies}.

@section{Academic Integrity}

The Campus Senate has adopted a policy asking students to include the
following statement on each examination or assignment in every course:
"I pledge on my honor that I have not given or received any
unauthorized assistance on this examination (or assignment)."
Consequently, you will be requested to include this pledge on each
exam and assignment. Please also carefully read the Office of Information
Technology's @link["http://www.nethics.umd.edu/aup/"]{policy}
regarding acceptable use of computer accounts.

Projects are to be completed @bold{collaboratively}, therefore
cooperation with others is @bold{not} a violation of the University's
Code of Academic Integrity.  However, your work must be your work.
Plaigarism or the uncredited use of others' work is not acceptable.
@bold{Any evidence} of this, or of unacceptable use of computer
accounts, use of unauthorized materials, or other possible violations
of the Honor Code, @bold{will be submitted} to the Student Honor
Council, which could result in an XF for the course, suspension, or
expulsion.

If you have any question about a particular situation or source then
consult with the instructor in advance. 

@bold{It is the responsibility, under the honor policy, of anyone who
suspects an incident of academic dishonesty has occurred to report it
to their instructor, or directly to the Honor Council.}


@section{Course Evaluations}

If you have a suggestion for improving this class, don't hesitate to
tell the instructor. At the end of the semester, please don't forget
to provide your feedback using the campus-wide
@link["https://www.courseevalum.umd.edu/"]{CourseEvalUM} system. Your
comments will help make this class better.

@section{Right to Change Information}

Although every effort has been made to be complete and accurate,
unforeseen circumstances arising during the semester could require the
adjustment of any material given here. Consequently, given due notice
to students, the instructors reserve the right to change any
information on this syllabus or in other course materials.  Such
changes will be announced and prominently displayed at the top of the
syllabus.

@section{Course Materials}

Portions of the course materials are based on material developed by
Ranjit Jhala and Joe Gibbs Politz.  The CMSC 430 materials were
developed by David Van Horn and José Manuel Calderón Trilla.
