\NeedsTeXFormat{LaTeX2e} % 
\ProvidesClass{aghthesis} 

% DECLARING OPTIONS

\DeclareOption{polish}
{
\newcommand*{\LangAgh}{Akademia G\'{o}rniczo-Hutnicza\\[2mm] im. Stanis\l{}awa Staszica w Krakowie}
\newcommand*{\LangFaculty}{Wydzia{\l} Elektrotechniki, Automatyki, Informatyki i Elektroniki\\[2mm] Katedra Informatyki}
\newcommand*{\LangSupervisor}{Opiekun:}
\newcommand*{\LangStatementHead}{O\'SWIADCZENIE AUTORA PRACY}
\newcommand*{\LangStatementBody}{O\'swiadczam, \'swiadomy(-a) odpowiedzialno\'sci karnej za po\'swiadczenie nieprawdy, \.ze niniejszy projekt wykona{\l}em(-am) osobi\'scie i samodzielnie w zakresie opisanym w dalszej cz\k{e}\'sci dokumentu  i \.ze nie korzysta{\l}em(-am) ze \'zr\'ode{\l} innych ni\.z wymienione w dalszej cz\k{e}\'sci dokumentu.}
\newcommand*{\LangSignature}{PODPIS}
\newcommand*{\LangThesisType}{Projekt in\.zynierski}
}

\DeclareOption{english}
{
\newcommand*{\LangAgh}{AGH University of Science and Technology}
\newcommand*{\LangFaculty}{Faculty of Electrical Engineering, Automatics,\\Computer Science and Electronics\\[1mm] Department of Computer Science}
\newcommand*{\LangSupervisor}{Supervisor:}
\newcommand*{\LangStatementHead}{NON-PLAGIARISM STATEMENT}
\newcommand*{\LangStatementBody}{I hereby declare that I have written this thesis completely by myself, that I have used no other sources or resources than the ones mentioned,
and that I am aware of the rules of the Penal Code referring to criminal liability for providing false evidence.}
\newcommand*{\LangSignature}{SIGNATURE}
\newcommand*{\LangThesisType}{Final project}
}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}} 


% PROCESSING OPTIONS
\ProcessOptions
\LoadClass[a4paper,twoside]{article}


\RequirePackage{setspace}
\RequirePackage{times}
\RequirePackage[T1]{fontenc}
\RequirePackage{graphicx}
\RequirePackage{geometry}
\RequirePackage[usenames]{color}
\RequirePackage{fancyhdr}
\RequirePackage{titlesec}
\RequirePackage{babel}



\addto\captionspolish{%
  \def\bibname{Materia{\l}y {\'z}r{\'o}d{\l}owe}%
  \def\refname{Materia{\l}y {\'z}r{\'o}d{\l}owe}%
  \def\listtablename{Spis tabel}%
  \def\tablename{Tabela}%  
}


\geometry{bindingoffset=10mm, lmargin=20mm, rmargin=20mm, tmargin=25mm, bmargin=25mm, foot=10mm, head=10mm}


\renewcommand{\baselinestretch}{1.1}

\def\title#1        {\gdef\@title{#1}}
\def\faculty#1      {\gdef\@faculty{#1}}
\def\thesistype#1   {\gdef\@thesistype{#1}}
\def\supervisor#1   {\gdef\@supervisor{#1}}

\bibliographystyle{abbrv}

%------------------------------------------------------------------------------


\renewcommand{\maketitle}{%


\newpage \titlepage 

\begin{center}

{\Large{\bfseries{\textsc \LangAgh}}}

\vspace*{1mm}\rule{.7\textwidth}{1mm}\vspace*{1mm}

{\large \LangFaculty}\vspace*{1cm}

\includegraphics[width=70pt,viewport=250 350 350 500]{agh.pdf}

\vspace*{1cm}
{\huge\scshape \LangThesisType}\\
\vspace*{2cm}

\doublespacing
{\huge{\bfseries\scshape \@title}}\\
\vspace*{1.5cm}

{\LARGE\scshape \@author}\\
%\vspace*{5cm}
\vfill

\singlespacing

\large{\scshape \LangSupervisor}\\
{\@supervisor}
\vfill

\rule{.7\textwidth}{1mm}\\
\vspace*{1mm}
{\large Krak\'{o}w \@date}

\end{center}


\newpage
\thispagestyle{empty}

%\vspace*{15cm} 
\mbox{}
\vfill

\begin{flushright}
\begin{minipage}[!h]{13cm}
\mbox{\large{\scshape \textbf{\LangStatementHead}}}\\[3mm] 
{\scshape \LangStatementBody}\\
\end{minipage}

\vspace{2cm}

\makebox[6cm][s]{\dotfill}\par
\makebox[6cm][c]{\small \LangSignature}

\end{flushright}

%------------------------------------------------------------------------------

\clearpage
\setcounter{page}{3}

}

%\ProcessOptions

\pagestyle{fancy}
\renewcommand{\sectionmark}[1]{\markright{\thesection.\ #1}}
\renewcommand{\headrulewidth}{0.5pt}
\fancyhf{}
\fancyhead{}

\fancyhead[LE]{\bfseries \thepage}
\fancyhead[RO]{\bfseries \thepage}

\frenchspacing
\sloppy 
\widowpenalty     9999 
\clubpenalty      9999 
\flushbottom




\titleformat{\section}[block]
{\bfseries\Large}{\filright \Large\thesection. }{0ex}{}
\titleformat{\subsection}[block]
{\bfseries\large}{\filright \large\thesubsection. }{0ex}{}
\titleformat{\subsubsection}[block]
{\bfseries\normalsize}{\filright \normalsize\thesubsubsection. }{0ex}{}
