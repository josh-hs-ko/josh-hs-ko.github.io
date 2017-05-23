module Publications where


data PublicationType = Published | Warning | Unpublished

data HyperlinkYear = IncludeYear | ExcludeYear

data Publication = Publication
  { title    :: String
  , authors  :: [String]
  , venue    :: Maybe (String, Maybe (String, HyperlinkYear))
  , year     :: Int
  , types    :: [(PublicationType, String)]
  , links    :: [(String, Maybe String, String)]
  , info     :: [(String, String, Maybe String)]
  }


publicationList :: [Publication]
publicationList =
  [ Publication
      { title    = "Palgol: A high-level DSL for vertex-centric graph processing with remote data access"
      , authors  = [ "Yongzhe Zhang"
                   , "Hsiang-Shang Ko"
                   , "Zhenjiang Hu"
                   ]
      , venue    = Just ("arXiv", Nothing)
      , year     = 2017
      , types    = [ (Unpublished, "Technical report")
                   ]
      , links    = [ ("PDF", Just "arXiv", "https://arxiv.org/pdf/1703.09542")
                   , ("Repository", Just "Bitbucket", "https://bitbucket.org/zyz915/palgol")
                   ]
      , info     = [ ("arXiv", "1703.09542", Nothing)
                   , ("Abstract", "Pregel is a popular parallel computing model for dealing with large-scale graphs. However, it can be tricky to implement graph algorithms correctly and efficiently in Pregel’s vertex-centric model, as programmers need to carefully restructure an algorithm in terms of supersteps and message passing, which are low-level and detached from the algorithm descriptions. Some domain-specific languages (DSLs) have been proposed to provide more intuitive ways to implement graph algorithms, but none of them can flexibly describe remote access (reading or writing attributes of other vertices through references), causing a still wide range of algorithms hard to implement.</p><p>To address this problem, we design and implement Palgol, a more declarative and powerful DSL which supports remote access. In particular, programmers can use a more declarative syntax called <em>global field access</em> to directly read data on remote vertices. By structuring supersteps in a high-level vertex-centric computation model and analyzing the logic patterns of global field access, we provide a novel algorithm for compiling Palgol programs to efficient Pregel code. We demonstrate the power of Palgol by using it to implement a bunch of practical Pregel algorithms and compare them with hand-written code. The evaluation result shows that the efficiency of Palgol is comparable with that of hand-written code.", Nothing)
                   ]
      }
  , Publication
      { title    = "An axiomatic basis for bidirectional programming"
      , authors  = [ "Hsiang-Shang Ko"
                   , "Zhenjiang Hu"
                   ]
      , venue    = Nothing
      , year     = 2017
      , types    = [ (Unpublished, "Draft manuscript")
                   ]
      , links    = [ ("PDF", Just "March 2017", "manuscripts/BiGUL-logic.pdf")
                   , ("Agda code", Just "Bitbucket", "https://bitbucket.org/prl_tokyo/bigul/src/logic/Agda/")
                   ]
      , info     = [ ("Abstract", "Among the frameworks of bidirectional transformations proposed for addressing various synchronisation problems, Foster et al.’s asymmetric lenses have influenced the design of a generation of bidirectional programming languages. Most of these languages are highly declarative, and only allow the programmer to specify a consistency relation with limited control over the behaviour of the automatically derived consistency restorer. However, synchronisation problems are diverse and require vastly different consistency restoration strategies, and to cope with the diversity, the programmer must be empowered to fully control and reason about the consistency restoration behaviour of their bidirectional programs. The putback-based approach to bidirectional programming was proposed to address this issue once and for all, and this paper takes the approach one step further by proposing a Hoare-style logic for Ko et al.’s putback-based language BiGUL. With this Hoare-style logic, the BiGUL programmer can precisely characterise the bidirectional behaviour of their programs by reasoning solely in the putback direction. The theory underlying the Hoare-style logic has been formalised and checked in Agda, but this paper presents the Hoare-style logic in a semi-formal way to make it easily understood and usable by the working BiGUL programmer.", Nothing)
                   ]
      }
  , Publication
      { title    = "BenchmarX reloaded: A practical benchmark framework for bidirectional transformations"
      , authors  = [ "Anthony Anjorin"
                   , "Zinovy Diskin"
                   , "Frédéric Jouault"
                   , "Hsiang-Shang Ko"
                   , "Erhan Leblebici"
                   , "Bernhard Westfechtel"
                   ]
      , venue    = Just ("International Workshop on Bidirectional Transformations (BX)", Just ("http://bx-community.wikidot.com/bx2017:home", IncludeYear))
      , year     = 2017
      , types    = []
      , links    = [ ("PDF", Just "CEUR-WS.org", "http://ceur-ws.org/Vol-1827/paper6.pdf")
                   , ("Repository", Just "GitHub", "https://github.com/eMoflon/benchmarx")
                   ]
      , info     = [ ("Pages", "15–30", Nothing)
                   , ("URL", "http://ceur-ws.org/Vol-1827/paper6.pdf", Nothing)
                   , ("Abstract", "Bidirectional transformation (bx) approaches provide a systematic way of specifying, restoring, and maintaining the consistency of related models. The current diversity of bx approaches is certainly beneficial, but it also poses challenges, especially when it comes to comparing the different approaches and corresponding bx tools that implement them. Although a benchmark for bx (referred to as a <em>benchmarx</em>) has been identified in the community as an important and currently still missing contribution, only a rather abstract description and characterisation of what a benchmarx should be has been published to date. In this paper, therefore, we focus on providing a practical and pragmatic framework, on which future concrete benchmarx can be built. To demonstrate its feasibility, we present a first non-trivial benchmarx based on a well-known example, and use it to compare and evaluate three bx tools, chosen to cover the broad spectrum of bx approaches.", Nothing)
                   ]
      }
  , Publication
      { title    = "Principle and practice of bidirectional programming in BiGUL"
      , authors  = [ "Zhenjiang Hu"
                   , "Hsiang-Shang Ko"
                   ]
      , venue    = Just ("Oxford Summer School on Bidirectional Transformations", Just ("https://www.cs.ox.ac.uk/projects/tlcbx/ssbx/", ExcludeYear))
      , year     = 2017
      , types    = [ (Unpublished, "Draft manuscript")
                   , (Warning, "Publication planned")
                   ]
      , links    = [ ("PDF", Just "19 April 2017", "https://bitbucket.org/prl_tokyo/bigul/raw/master/SSBX16/tutorial.pdf")
                   , ("Repository", Just "Bitbucket", "https://bitbucket.org/prl_tokyo/bigul")
                   ]
      , info     = [ ("Abstract", "Putback-based bidirectional programming allows the programmer to write only one putback transformation, from which the unique corresponding forward transformation is derived for free. A key distinguishing feature of putback-based bidirectional programming is full control over the bidirectional behavior, which is important for specifying intended bidirectional transformations without any ambiguity. In this tutorial, we will introduce BiGUL, a simple yet powerful putback-based bidirectional programming language, explaining the underlying principles and showing how various kinds of bidirectional applications can be developed in BiGUL.", Nothing)
                   ]
      }
  , Publication
      { title    = "Programming with ornaments"
      , authors  = [ "Hsiang-Shang Ko"
                   , "Jeremy Gibbons"
                   ]
      , venue    = Just ("Journal of Functional Programming", Nothing)
      , year     = 2017
      , types    = []
      , links    = [ ("PDF", Just "preprint", "manuscripts/OrnJFP.pdf")
                   , ("Agda code", Just "JFP", "https://www.cambridge.org/core/journals/journal-of-functional-programming/article/programming-with-ornaments/97C63D7C90556ACD2EC1482E63026A74#fndtn-supplementary-materials")
                   ]
      , info     = [ ("Volume", "27", Nothing)
                   , ("Number", "e2", Nothing)
                   , ("DOI", "10.1017/S0956796816000307", Nothing)
                   , ("Abstract", "Dependently typed programming advocates the use of various indexed versions of the same shape of data, but the formal relationship amongst these structurally similar datatypes usually needs to be established manually and tediously. Ornaments have been proposed as a formal mechanism to manage the relationships between such datatype variants. In this paper, we conduct a case study under an ornament framework; the case study concerns programming binomial heaps and their operations — including insertion and minimum extraction — by viewing them as lifted versions of binary numbers and numeric operations. We show how current dependently typed programming technology can lead to a clean treatment of the binomial heap constraints when implementing heap operations. We also identify some gaps between the current technology and an ideal dependently typed programming language that we would wish to have for our development.", Nothing)
                   ]
      }
  , Publication
      { title    = "Parsing and reflective printing, bidirectionally"
      , authors  = [ "Zirun Zhu"
                   , "Yongzhe Zhang"
                   , "Hsiang-Shang Ko"
                   , "Pedro Martins"
                   , "João Saraiva"
                   , "Zhenjiang Hu"
                   ]
      , venue    = Just ("International Conference on Software Language Engineering (SLE)", Just ("http://www.sleconf.org/2016/", IncludeYear))
      , year     = 2016
      , types    = []
      , links    = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/authorize?N21371")
                   , ("Demo site", Nothing, "http://biyacc.yozora.moe")
                   ]
      , info     = [ ("Pages", "2–14", Nothing)
                   , ("DOI", "10.1145/2997364.2997369", Nothing)
                   , ("Abstract", "Language designers usually need to implement parsers and printers. Despite being two intimately related programs, in practice they are often designed separately, and then need to be revised and kept consistent as the language evolves. It will be more convenient if the parser and printer can be unified and developed in one single program, with their consistency guaranteed automatically.</p><p>Furthermore, in certain scenarios (like showing compiler optimisation results to the programmer), it is desirable to have a more powerful <em>reflective</em> printer that, when an abstract syntax tree corresponding to a piece of program text is modified, can reflect the modification to the program text while preserving layouts, comments, and syntactic sugar.</p><p>To address these needs, we propose a domain-specific language BiYacc, whose programs denote both a parser and a reflective printer for an unambiguous context-free grammar. BiYacc is based on the theory of <em>bidirectional transformations</em>, which helps to guarantee by construction that the pairs of parsers and reflective printers generated by BiYacc are consistent. We show that BiYacc is capable of facilitating many tasks such as Pombrio and Krishnamurthi’s “resugaring”, language evolution, and refactoring.", Nothing)
                   ]
      }
  , Publication
      { title    = "The under-appreciated put: Implementing delta-alignment in BiGUL"
      , authors  = [ "Jorge Mendes"
                   , "Hsiang-Shang Ko"
                   , "Zhenjiang Hu"
                   ]
      , venue    = Just ("GRACE Center, National Institute of Informatics", Nothing)
      , year     = 2016
      , types    = [ (Unpublished, "Technical report")
                   ]
      , links    = [ ("PDF", Just "revised version", "manuscripts/BiGUL-delta-alignment.pdf")
                   , ("PDF", Just "GRACE Center", "http://grace-center.jp/wp-content/uploads/2016/04/GRACE-TR-2016-03.pdf")
                   ]
      , info     = [ ("Number", "GRACE-TR 2016-03", Nothing)
                   , ("URL", "http://grace-center.jp/wp-content/uploads/2016/04/GRACE-TR-2016-03.pdf", Nothing)
                   , ("Note", "The revised version uses a concrete running example and contains more explanations.", Nothing)
                   , ("Abstract", "There are two approaches to bidirectional programming. One is the get-based method where one writes <em>get</em> and <em>put</em> is automatically derived, and the other is the put-based method where one writes <em>put</em> and <em>get</em> is automatically derived. In this paper, we argue that the put-based method deserves more attention, because a good language for programming <em>put</em> can not only give full control over the behavior of bidirectional transformations, but also enable us to efficiently develop various domain-specific bidirectional languages and use them seamlessly in one framework, which would be non-trivial with the get-based method. We demonstrate how the matching/delta/generic lenses can be implemented in BiGUL, a putback-based bidirectional language.", Nothing)
                   ]
      }
  , Publication
      { title    = "Brul: A putback-based bidirectional transformation library for updatable views"
      , authors  = [ "Tao Zan"
                   , "Li Liu"
                   , "Hsiang-Shang Ko"
                   , "Zhenjiang Hu"
                   ]
      , venue    = Just ("International Workshop on Bidirectional Transformations (BX)", Just ("http://bx-community.wikidot.com/bx2016:home", IncludeYear))
      , year     = 2016
      , types    = []
      , links    = [ ("PDF", Just "CEUR-WS.org", "http://ceur-ws.org/Vol-1571/paper_3.pdf")
                   ]
      , info     = [ ("Pages", "77–89", Nothing)
                   , ("URL", "http://ceur-ws.org/Vol-1571/paper_3.pdf", Nothing)
                   , ("Abstract", "In work on relational databases, the view-update problem is about how to translate update operations on the view table to corresponding update operations on the source table properly. It is a problem that the translation policies are not unique in many situations. Relational lenses try to solve this problem by providing a list of combinators that let the user write <em>get</em> functions (queries) with specified updated policies for <em>put</em> functions (updates); however this can only provide limited control of update policies which still may not satisfy the user’s real needs. In this paper, we implement a library Brul that provides putback-based basic combinators for the user to write the <em>put</em> function with flexible update policies easily; from the <em>put</em> function, a unique <em>get</em> function can be derived automatically. Brul is implemented in terms of BiGUL, a core bidirectional programming language which has been formalized in Agda and implemented as a Haskell library.", Nothing)
                   ]
      }
  , Publication
      { title    = "BiGUL: A formally verified core language for putback-based bidirectional programming"
      , authors  = [ "Hsiang-Shang Ko"
                   , "Tao Zan"
                   , "Zhenjiang Hu"
                   ]
      , venue    = Just ("Workshop on Partial Evaluation and Program Manipulation (PEPM)", Just ("http://conf.researchr.org/home/pepm-2016", IncludeYear))
      , year     = 2016
      , types    = []
      , links    = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/authorize?N21372")
                   , ("Repository", Just "Bitbucket", "https://bitbucket.org/prl_tokyo/bigul")
                   ]
      , info     = [ ("Pages", "61–72", Nothing)
                   , ("DOI", "10.1145/2847538.2847544", Nothing)
                   , ("Note", "The version of BiGUL described in this paper is outdated. See the <a href=\"#publication-a1e91c98\">“axiomatic basis” paper</a> for a semi-formal axiomatic introduction to the current BiGUL, or the <a href=\"#publication-5d3c0d1a\">Oxford tutorial</a> for a gentler treatment.", Nothing)
                   , ("Abstract", "Putback-based bidirectional programming allows the programmer to write only one putback transformation, from which the unique corresponding forward transformation is derived for free. The logic of a putback transformation is more sophisticated than that of a forward transformation and does not always give rise to well-behaved bidirectional programs; this calls for more robust language design to support development of well-behaved putback transformations. In this paper, we design and implement a concise core language BiGUL for putback-based bidirectional programming to serve as a foundation for higher-level putback-based languages. BiGUL is completely formally verified in the dependently typed programming language Agda to guarantee that any putback transformation written in BiGUL is well-behaved.", Nothing)
                   ]
      }
  , Publication
      { title    = "BiYacc: Roll your parser and reflective printer into one"
      , authors  = [ "Zirun Zhu"
                   , "Hsiang-Shang Ko"
                   , "Pedro Martins"
                   , "João Saraiva"
                   , "Zhenjiang Hu"
                   ]
      , venue    = Just ("International Workshop on Bidirectional Transformations (BX)", Just ("http://bx-community.wikidot.com/bx2015:home", IncludeYear))
      , year     = 2015
      , types    = [ (Published, "Tool paper")
                   ]
      , links    = [ ("PDF", Just "CEUR-WS.org", "http://ceur-ws.org/Vol-1396/p43-zhu.pdf")
                   , ("Demo site", Nothing, "http://biyacc.yozora.moe")
                   ]
      , info     = [ ("Pages", "43–50", Nothing)
                   , ("URL", "http://ceur-ws.org/Vol-1396/p43-zhu.pdf", Nothing)
                   , ("See also", "the more complete <a href=\"#publication-4342d3dd\">SLE’16 paper</a>, which, in particular, describes the internals of BiYacc in much more detail.", Nothing)
                   , ("Abstract", "Language designers usually need to implement parsers and printers. Despite being two related programs, in practice they are designed and implemented separately. This approach has an obvious disadvantage: as a language evolves, both its parser and printer need to be separately revised and kept synchronised. Such tasks are routine but complicated and error-prone. To facilitate these tasks, we propose a language called BiYacc, whose programs denote both a parser and a printer. In essence, BiYacc is a domain-specific language for writing <em>putback-based</em> bidirectional transformations — the printer is a putback transformation, and the parser is the corresponding get transformation. The pairs of parsers and printers generated by BiYacc are thus always guaranteed to satisfy the usual round-trip properties. The highlight that distinguishes this <em>reflective</em> printer from others is that the printer — being a putback transformation — accepts not only an abstract syntax tree but also a string, and produces an updated string consistent with the given abstract syntax tree. We can thus make use of the additional input string, with mechanisms such as simultaneous pattern matching on the view and the source, to provide users with full control over the printing-strategies.", Nothing)
                   ]
      }
  , Publication
      { title    = "Analysis and synthesis of inductive families"
      , authors  = [ "Hsiang-Shang Ko"
                   ]
      , venue    = Just ("University of Oxford", Nothing)
      , year     = 2014
      , types    = [ (Published, "DPhil dissertation")
                   ]
      , links    = [ ("PDF", Just "GitHub", "https://github.com/josh-hs-ko/dissertation/raw/master/dissertation.pdf")
                   , ("PDF", Just "Oxford", "https://ora.ox.ac.uk/objects/ora:9019/datastreams/THESIS01")
                   , ("Agda code", Just "GitHub", "https://github.com/josh-hs-ko/Thesis")
                   ]
      , info     = [ ("URL", "https://ora.ox.ac.uk/objects/ora:9019", Nothing)
                   , ("Repository", "https://github.com/josh-hs-ko/dissertation", Nothing)
                   , ("Note", "The GitHub PDF uses https DOI links and fixes identifier hyperlinks and a few typos in the Oxford version.", Nothing)
                   , ("Abstract", "Based on a natural unification of logic and computation, Martin-Löf’s <em>intuitionistic type theory</em> can be regarded simultaneously as a computationally meaningful higher-order logic system and an expressively typed functional programming language, in which proofs and programs are treated as the same entities. Two modes of programming can then be distinguished: in <em>externalism</em>, we construct a program separately from its correctness proof with respect to a given specification, whereas in <em>internalism</em>, we encode the specification in a sophisticated type such that any program inhabiting the type also encodes a correctness proof, and we can use type information as a guidance on program construction. Internalism is particularly effective in the presence of <em>inductive families</em>, whose design can have a strong influence on program structure. Techniques and mechanisms for facilitating internalist programming are still lacking, however.</p><p>This dissertation proposes that internalist programming can be facilitated by exploiting an interconnection between internalism and externalism, expressed as isomorphisms between inductive families into which data structure invariants are encoded and their simpler variants paired with predicates expressing those invariants. The interconnection has two directions: one <em>analysing</em> inductive families into simpler variants and predicates, and the other <em>synthesising</em> inductive families from simpler variants and specific predicates. They respectively give rise to two applications, one achieving a modular structure of internalist libraries, and the other bridging internalist programming with relational specifications and program derivation. The datatype-generic mechanisms supporting the applications are based on McBride’s <em>ornaments</em>. Theoretically, the key ornamental constructs — <em>parallel composition of ornaments</em> and <em>relational algebraic ornamentation</em> — are further characterised in terms of lightweight category theory. Most of the results are completely formalised in the Agda programming language.", Nothing)
                   ]
      }
  , Publication
      { title    = "Relational algebraic ornaments"
      , authors  = [ "Hsiang-Shang Ko"
                   , "Jeremy Gibbons"
                   ]
      , venue    = Just ("Workshop on Dependently Typed Programming (DTP)", Just ("http://www.seas.upenn.edu/~sweirich/dtp13/", IncludeYear))
      , year     = 2013
      , types    = []
      , links    = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/authorize?N21373")
                   , ("Agda code", Just "ACM", "http://dl.acm.org/citation.cfm?id=2502413&preflayout=flat#formats")
                   ]
      , info     = [ ("Pages", "37–48", Nothing)
                   , ("DOI", "10.1145/2502409.2502413", Nothing)
                   , ("See also", "my <a href=\"#publication-9c9a95f3\">DPhil dissertation</a>, which subsumes and streamlines the content of this paper, and includes more examples.", Nothing)
                   , ("Abstract", "Dependently typed programming is hard, because ideally dependently typed programs should share structure with their correctness proofs, but there are very few guidelines on how one can arrive at such integrated programs. McBride’s algebraic ornamentation provides a methodological advancement, by which the programmer can derive a datatype from a specification involving a fold, such that a program that constructs elements of that datatype would be correct by construction. It is thus an effective method that leads the programmer from a specification to a dependently typed program. We enhance the applicability of this method by generalising algebraic ornamentation to a relational setting and bringing in relational algebraic methods, resulting in a hybrid approach that makes essential use of both dependently typed programming and relational program derivation. A dependently typed solution to the minimum coin change problem is presented as a demonstration of this hybrid approach. We also give a theoretically interesting “completeness theorem” of relational algebraic ornaments, which sheds some light on the expressive power of ornaments and inductive families.", Nothing)
                   ]
      }
  , Publication
      { title    = "Modularising inductive families"
      , authors  = [ "Hsiang-Shang Ko"
                   , "Jeremy Gibbons"
                   ]
      , venue    = Just ("Progress in Informatics", Nothing)
      , year     = 2013
      , types    = []
      , links    = [ ("PDF", Just "NII", "http://www.nii.ac.jp/pi/n10/10_65.pdf")
                   , ("Agda code", Just "GitHub", "https://github.com/josh-hs-ko/Thesis")
                   ]
      , info     = [ ("Number", "10", Nothing)
                   , ("Pages", "65–88", Nothing)
                   , ("DOI", "10.2201/NiiPi.2013.10.5", Nothing)
                   , ("See also", "my <a href=\"#publication-9c9a95f3\">DPhil dissertation</a>, which subsumes and streamlines the content of this paper, and includes more examples.", Nothing)
                   , ("Abstract", "Dependently typed programmers are encouraged to use inductive families to integrate constraints with data construction. Different constraints are used in different contexts, leading to different versions of datatypes for the same data structure. For example, sequences might be constrained by length or by an ordering on elements, giving rise to different datatypes “vectors” and “sorted lists” for the same underlying data structure of sequences. Modular implementation of common operations for these structurally similar datatypes has been a longstanding problem. We propose a datatype-generic solution, in which we axiomatise a family of isomorphisms between datatypes and their more refined versions as datatype refinements, and show that McBride’s ornaments can be translated into such refinements. With the ornament-induced refinements, relevant properties of the operations can be separately proven for each constraint, and after the programmer selects several constraints to impose on a basic datatype and synthesises a new datatype incorporating those constraints, the operations can be routinely upgraded to work with the synthesised datatype.", Nothing)
                   ]
      }
  , Publication
      { title    = "Modularising inductive families"
      , authors  = [ "Hsiang-Shang Ko"
                   , "Jeremy Gibbons"
                   ]
      , venue    = Just ("Workshop on Generic Programming (WGP)", Just ("http://www.wgp-sigplan.org/farmer/doku.php?id=2011", IncludeYear))
      , year     = 2011
      , types    = [ (Warning, "Superseded")
                   ]
      , links    = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/authorize?N21374")
                   ]
      , info     = [ ("Pages", "13–24", Nothing)
                   , ("DOI", "10.1145/2036918.2036921", Nothing)
                   , ("Note", "This paper has been superseded by an <a href=\"#publication-685cb02c\">extended version in Progress in Informatics</a>.", Nothing)
                   , ("Abstract", "Dependently typed programmers are encouraged to use inductive families to integrate constraints with data construction. Different constraints are used in different contexts, leading to different versions of datatypes for the same data structure. Modular implementation of common operations for these structurally similar datatypes has been a longstanding problem. We propose a datatype-generic solution based on McBride’s datatype ornaments, exploiting an isomorphism whose interpretation borrows ideas from realisability. Relevant properties of the operations are separately proven for each constraint, and after the programmer selects several constraints to impose on a basic datatype and synthesises an inductive family incorporating those constraints, the operations can be routinely upgraded to work with the synthesised inductive family.", Nothing)
                   ]
      }
  , Publication
      { title    = "Algebra of Programming in Agda: Dependent types for relational program derivation"
      , authors  = [ "Shin-Cheng Mu"
                   , "Hsiang-Shang Ko"
                   , "Patrik Jansson"
                   ]
      , venue    = Just ("Journal of Functional Programming", Nothing)
      , year     = 2009
      , types    = []
      , links    = [ ("PDF", Just "preprint", "manuscripts/AoPA-JFP.pdf")
                   , ("Repository", Just "GitHub", "https://github.com/scmu/aopa")
                   ]
      , info     = [ ("Volume", "19", Nothing)
                   , ("Issue", "5", Nothing)
                   , ("Pages", "545–579", Nothing)
                   , ("DOI", "10.1017/S0956796809007345", Nothing)
                   , ("Abstract", "Relational program derivation is the technique of stepwise refining a relational specification to a program by algebraic rules. The program thus obtained is correct by construction. Meanwhile, dependent type theory is rich enough to express various correctness properties to be verified by the type checker. We have developed a library, AoPA, to encode relational derivations in the dependently typed programming language Agda. A program is coupled with an algebraic derivation whose correctness is guaranteed by the type system. Two non-trivial examples are presented: an optimisation problem, and a derivation of quicksort where well-founded recursion is used to model terminating hylomorphisms in a language with inductive types.", Nothing)
                   ]
      }
  , Publication
      { title    = "Algebra of Programming using dependent types"
      , authors  = [ "Shin-Cheng Mu"
                   , "Hsiang-Shang Ko"
                   , "Patrik Jansson"
                   ]
      , venue    = Just ("International Conference on Mathematics of Program Construction (MPC)", Just ("http://mpc08.lri.fr", IncludeYear))
      , year     = 2008
      , types    = [ (Warning, "Superseded")
                   ]
      , links    = [ ("PDF", Just "preprint", "manuscripts/AoPA-MPC.pdf")
                   , ("Repository", Just "GitHub", "https://github.com/scmu/aopa")
                   ]
      , info     = [ ("LNCS", "5133", Nothing)
                   , ("Pages", "268–283", Nothing)
                   , ("DOI", "10.1007/978-3-540-70594-9_15", Nothing)
                   , ("Note", "This paper has been superseded by an <a href=\"#publication-41007bea\">extended version in the Journal of Functional Programming</a>.", Nothing)
                   , ("Abstract", "Dependent type theory is rich enough to express that a program satisfies an input/output relational specification, but it could be hard to construct the proof term. On the other hand, squiggolists know very well how to show that one relation is included in another by algebraic reasoning. We demonstrate how to encode functional and relational derivations in a dependently typed programming language. A program is coupled with an algebraic derivation from a specification, whose correctness is guaranteed by the type system.", Nothing)
                   ]
      }
  ]
