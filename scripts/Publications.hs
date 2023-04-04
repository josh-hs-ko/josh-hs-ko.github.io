module Publications where


data PublicationType = Published | Warning | Unpublished

publicationType :: a -> a -> a -> PublicationType -> a
publicationType x y z Published   = x
publicationType x y z Warning     = y
publicationType x y z Unpublished = z

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
      { title   = "Datatype-generic programming meets elaborator reflection"
      , authors = [ "Hsiang-Shang Ko"
                  , "Liang-Ting Chen"
                  , "Tzu-Chi Lin"
                  ]
      , venue   = Just ("International Conference on Functional Programming (ICFP)", Just ("https://icfp22.sigplan.org", IncludeYear))
      , year    = 2022
      , types   = []
      , links   = [ ("PDF", Nothing, "https://dl.acm.org/doi/pdf/10.1145/3547629")
                  , ("Artefact", Just "Zenodo", "https://doi.org/10.5281/zenodo.6603498")
                  , ("Agda code", Just "HTML", "/Agda/ICFP22/Everything.html")
                  , ("Repository", Just "GitHub", "https://github.com/Zekt/Type-Embellishment")
                  , ("Slides", Just "PDF", "/slides/ICFP22.pdf")
                  , ("Video", Just "YouTube", "https://youtu.be/daJdoJGpsns")
                  ]
      , info    = [ ("PACMPL", "6 (ICFP)", Nothing)
                  , ("Article", "98", Nothing)
                  , ("Pages", "1–29", Nothing)
                  , ("DOI", "10.1145/3547629", Nothing)
                  , ("Agda version", "2.6.3", Nothing)
                  , ("Abstract", "Datatype-generic programming is natural and useful in dependently typed languages such as Agda. However, datatype-generic libraries in Agda are not reused as much as they should be, because traditionally they work only on datatypes decoded from a library’s own version of datatype descriptions; this means that different generic libraries cannot be used together, and they do not work on native datatypes, which are preferred by the practical Agda programmer for better language support and access to other libraries. Based on elaborator reflection, we present a framework in Agda featuring a set of general metaprograms for instantiating datatype-generic programs as, and for, a useful range of native datatypes and functions —including universe-polymorphic ones— in programmer-friendly and customisable forms. We expect that datatype-generic libraries built with our framework will be more attractive to the practical Agda programmer. As the elaborator reflection features used by our framework become more widespread, our design can be ported to other languages too.", Nothing)
                  ]
      }
  , Publication
      { title   = "Syntax-generic operations, reflectively reified"
      , authors = [ "Tzu-Chi Lin"
                  , "Hsiang-Shang Ko"
                  ]
      , venue   = Just ("Workshop on Type-Driven Development (TyDe)", Just ("https://icfp22.sigplan.org/home/tyde-2022", IncludeYear))
      , year    = 2022
      , types   = [ (Published, "Extended abstract")
                  ]
      , links   = [ ("PDF", Nothing, "https://github.com/Zekt/Generic-Scoped-Syntax/raw/main/tex/NGS.pdf")
                  , ("Repository", Just "GitHub", "https://github.com/Zekt/Generic-Scoped-Syntax")
                  ]
      , info    = [ ("Related blog post", "0022", Nothing)
                  , ("Abstract", "Libraries of generic operations on syntax trees with binders are emerging, and one of these is Allais et al.’s [2021] datatype-generic library in Agda, which provides syntax-generic constructions but not in a conventional form preferred by programmers. We port a core part of Allais et al.’s library to our new datatype-generic framework, which uses Agda's elaborator reflection to reify generic constructions to programs close to what programmers would write by hand. We hope that this work will make syntax-generic libraries such as Allais et al.’s more attractive, and stimulate discussion on the development of generic libraries.", Nothing)
                  ]
      }
  , Publication
      { title   = "Realising intensional S4 and GL modalities"
      , authors = [ "Liang-Ting Chen"
                  , "Hsiang-Shang Ko"
                  ]
      , venue   = Just ("Conference on Computer Science Logic (CSL)", Just ("https://csl2022.uni-goettingen.de", IncludeYear))
      , year    = 2022
      , types   = []
      , links   = [ ("PDF", Nothing, "https://drops.dagstuhl.de/opus/volltexte/2022/15734/pdf/LIPIcs-CSL-2022-14.pdf")
                  , ("Source code", Just "Zenodo", "https://doi.org/10.5281/zenodo.5602771")
                  ]
      , info    = [ ("LIPIcs", "216", Nothing)
                  , ("Article", "14", Nothing)
                  , ("Pages", "1–17", Nothing)
                  , ("DOI", "10.4230/LIPIcs.CSL.2022.14", Nothing)
                  , ("Related blog post", "0013", Nothing)
                  , ("Abstract", "There have been investigations into type-theoretic foundations for metaprogramming, notably Davies and Pfenning’s (2001) treatment in <b>S4</b> modal logic, where code evaluating to values of type <i>A</i> is given the modal type <code>Code</code> <i>A</i> (<code>□</code><i>A</i> in the original paper). Recently Kavvos (2017) extended PCF with <code>Code</code> <i>A</i> and intensional recursion, understood as the deductive form of the <b>GL</b> (Gödel-Löb) axiom in provability logic, but the resulting type system is logically inconsistent. Inspired by staged computation, we observe that a term of type <code>Code</code> <i>A</i> is, in general, code to be evaluated in a next stage, whereas <b>S4</b> modal type theory is a special case where code can be evaluated in the current stage, and the two types of code should be discriminated. Consequently, we use two separate modalities <code>⊠</code> and <code>□</code> to model <b>S4</b> and <b>GL</b> respectively in a unified categorical framework while retaining logical consistency. Following Kavvos’ (2017) novel approach to the semantics of intensionality, we interpret the two modalities in the 𝒫-category of assemblies and trackable maps. For the <b>GL</b> modality <code>□</code> in particular, we use guarded type theory to articulate what it means by a “next” stage and to model intensional recursion by guarded recursion together with Kleene’s second recursion theorem. Besides validating the S4 and GL axioms, our model better captures the essence of intensionality by refuting congruence (so that two extensionally equal terms may not be intensionally equal) and internal quoting (both <i>A</i> → <code>□</code><i>A</i> and <i>A</i> → <code>⊠</code><i>A</i>). Our results are developed in (guarded) homotopy type theory and formalised in Agda.", Nothing)
                  ]
      }
  , Publication
      { title   = "Weakly durable high-performance transactions"
      , authors = [ "Yun-Sheng Chang"
                  , "Yu-Fang Chen"
                  , "Hsiang-Shang Ko"
                  ]
      , venue   = Nothing
      , year    = 2021
      , types   = [ (Unpublished, "Manuscript")
                  ]
      , links   = [ ("PDF", Nothing, "https://arxiv.org/pdf/2110.01465")
                  ]
      , info    = [ ("arXiv", "2110.01465", Nothing)
                  , ("DOI", "10.48550/arXiv.2110.01465", Nothing)
                  , ("Related blog post", "0019", Nothing)
                  , ("Abstract", "Existing disk-based database systems largely fall into two categories—they either provide very high performance but few guarantees, or expose the transaction abstraction satisfying the full ACID guarantees at the cost of lower performance. In this paper, we present an alternative that achieves the best of both worlds, namely good performance and transactional properties. Our key observation is that, because of the frequent use of synchronization primitives, systems with strong durability can hardly utilize the extremely high parallelism granted by modern storage devices. Thus, we explore the notion of weakly durable transactions, and discuss how to safely relax durability without compromising other transactional properties. We present AciKV, a transactional system whose design is centered around weak durability. AciKV exposes to users the normal transactional interface, but what sets it apart from others is a new “persist” primitive that decouples durability from commit. AciKV is a middle ground between systems that perform fast atomic operations, and ones that support transactions; this middle ground is useful as it provides similar performance to the former, while prevents isolation and consistency anomalies like the latter. Our evaluation using the YCSB benchmark shows that AciKV, under workloads that involve write requests, exhibits more than two orders of magnitude higher throughput than existing strongly durable systems.", Nothing)
                  ]
      }
  , Publication
      { title   = "C-lenses explained: Bx foundations for the rest of us"
      , authors = [ "Anthony Anjorin"
                  , "Hsiang-Shang Ko"
                  , "Erhan Leblebici"
                  ]
      , venue   = Just ("International Workshop on Bidirectional Transformations (Bx)", Just ("http://bx-community.wikidot.com/bx2021:home", IncludeYear))
      , year    = 2021
      , types   = []
      , links   = [ ("PDF", Nothing, "http://ceur-ws.org/Vol-2999/bxpaper1.pdf")
                  ]
      , info    = [ ("Pages", "1–17", Nothing)
                  , ("URL", "http://ceur-ws.org/Vol-2999/bxpaper1.pdf", Nothing)
                  , ("Abstract", "Bidirectional transformations (bx) are mechanisms for maintaining the consistency of multiple artefacts. Some of the challenges bx research aims to address include answering fundamental questions such as how best to precisely characterise consistency, “good” consistency maintainers, and the required input and assumptions to guarantee this good behaviour.</p><p>While substantial progress has been made towards unifying the different (variants of) formal frameworks for bx, many of these formal results are not yet easily accessible to more practically-oriented bx researchers, often due to a missing background in (advanced) category theory. This is an unfortunate situation, as many bx tools are developed and maintained by practically-oriented bx researchers, who would but currently cannot fully profit from formal results and insights. In particular, we are not aware of any practical implementation of C-lenses, not even preliminary discussions about its relevance to practical bx. We believe this is because of the inaccessibility of the categorical language, and write this paper to decipher C-lenses for bx researchers without any substantial background in category theory. Our goal is to spark broader interest in C-lenses and in discussions on their usefulness in practice and potential for improving existing, and inspiring new bx tools.</p><p>We start by reviewing the well-known state-based bx framework, already accessible to a broad audience due to its simplicity. We then explain how this framework can be elegantly generalised to the richer, delta-based setting of C-lenses using multiple examples and illustrative diagrams.", Nothing)
                  ]
      }
  , Publication
      { title   = "Programming metamorphic algorithms: An experiment in type-driven algorithm design"
      , authors = [ "Hsiang-Shang Ko"
                  ]
      , venue   = Just ("The Art, Science, and Engineering of Programming", Nothing)
      , year    = 2021
      , types   = []
      , links   = [ ("PDF", Nothing, "https://arxiv.org/pdf/2010.16302v3")
                  , ("Agda code", Just "Zenodo", "https://doi.org/10.5281/zenodo.4030101")
                  , ("Agda code", Just "HTML", "/Agda/Programming21/MetamorphismsInAgda.html")
                  ]
      , info    = [ ("Volume",  "5", Nothing)
                  , ("Issue" ,  "2", Nothing)
                  , ("Article", "7", Nothing)
                  , ("Pages", "1–34", Nothing)
                  , ("DOI", "10.22152/programming-journal.org/2021/5/7", Nothing)
                  , ("Agda version", "2.6.1 with Standard Library version 1.3", Nothing)
                  , ("Related blog post", "0003", Nothing)
                  , ("Abstract", "In <em>dependently typed programming</em>, proofs of basic, structural properties can be embedded implicitly into programs and do not need to be written explicitly. Besides saving the effort of writing separate proofs, a most distinguishing and fascinating aspect of dependently typed programming is that it makes the idea of <em>interactive type-driven development</em> much more powerful, where expressive type information becomes useful hints that help the programmer to complete a program. There have not been many attempts at exploiting the full potential of the idea, though. As a departure from the usual properties dealt with in dependently typed programming, and as a demonstration that the idea of interactive type-driven development has more potential to be discovered, we conduct an experiment in ‘type-driven algorithm design’: we develop algorithms from their specifications encoded in sophisticated types, to see how useful the hints provided by a type-aware interactive development environment can be. The algorithmic problem we choose is <em>metamorphisms</em>, whose definitional behaviour is consuming a data structure to compute an intermediate value and then producing a codata structure from that value, but there are other ways to compute metamorphisms. We develop Gibbons’s streaming algorithm and Nakano’s jigsaw model in the interactive development environment provided by the dependently typed language Agda, turning intuitive ideas about these algorithms into formal conditions and programs that are correct by construction.", Nothing)
                  ]
      }
  , Publication
      { title   = "Determinizing crash behavior with a verified snapshot-consistent flash translation layer"
      , authors = [ "Yun-Sheng Chang"
                  , "Yao Hsiao"
                  , "Tzu-Chi Lin"
                  , "Che-Wei Tsao"
                  , "Chun-Feng Wu"
                  , "Yuan-Hao Chang"
                  , "Hsiang-Shang Ko"
                  , "Yu-Fang Chen"
                  ]
      , venue   = Just ("Symposium on Operating Systems Design and Implementation (OSDI)", Just ("https://www.usenix.org/conference/osdi20", IncludeYear))
      , year    = 2020
      , types   = []
      , links   = [ ("PDF", Nothing, "https://www.usenix.org/system/files/osdi20-chang.pdf")
                  , ("Artefacts", Just "GitHub", "https://github.com/yunshengtw/scftl")
                  ]
      , info    = [ ("Pages", "81–97", Nothing)
                  , ("URL", "https://www.usenix.org/conference/osdi20/presentation/chang", Nothing)
                  , ("Related blog posts", "0004, 0007", Nothing)
                  , ("Abstract", "This paper introduces the design of a snapshot-consistent flash translation layer (SCFTL) for flash disks, which has a stronger guarantee about the possible behavior after a crash than conventional designs. More specifically, the flush operation of SCFTL also has the functionality of making a “disk snapshot.” When a crash occurs, the flash disk is guaranteed to recover to the state right before the last flush. The major benefit of SCFTL is that it allows a more efficient design of upper layers in the storage stack. For example, the file system built on SCFTL does not require the use of a journal for crash recovery. Instead, it only needs to perform a flush operation of SCFTL at the end of each atomic transaction. We use a combination of a proof assistant, a symbolic executor, and an SMT solver, to formally verify the correctness of our SCFTL implementation. We modify the xv6 file system to support group commit and utilize SCFTL’s stronger crash guarantee. Our evaluation using file system benchmarks shows that the modified xv6 on SCFTL is 3 to 30 times faster than xv6 with logging on conventional FTLs, and is in the worst case only two times slower than the state-of-the-art setting: the ext4 file system on the Physical Block Device (pblk) FTL.", Nothing)
                  ]
      }
  , Publication
      { title   = "Unifying parsing and reflective printing for fully disambiguated grammars"
      , authors = [ "Zirun Zhu"
                  , "Hsiang-Shang Ko"
                  , "Yongzhe Zhang"
                  , "Pedro Martins"
                  , "João Saraiva"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Just ("New Generation Computing", Nothing)
      , year    = 2020
      , types   = []
      , links   = [ ("PDF", Just "accepted version", "/manuscripts/NGCO20.pdf")
                  , ("Definitive version", Just "Springer Nature SharedIt", "https://rdcu.be/b3SAc")
               -- , ("Demo site", Nothing, "https://biyacc.k331.one")
                  ]
      , info    = [ ("Volume", "38", Nothing)
                  , ("Pages", "423–476", Nothing)
                  , ("DOI", "10.1007/s00354-019-00082-y", Nothing)
                  , ("Abstract", "Language designers usually need to implement parsers and printers. Despite being two closely related programs, in practice they are often designed separately, and then need to be revised and kept consistent as the language evolves. It will be more convenient if the parser and printer can be unified and developed in a single program, with their consistency guaranteed automatically. Furthermore, in certain scenarios (like showing compiler optimisation results to the programmer), it is desirable to have a more powerful <em>reflective</em> printer that, when an abstract syntax tree corresponding to a piece of program text is modified, can propagate the modification to the program text while preserving layouts, comments, and syntactic sugar.</p><p>To address these needs, we propose a domain-specific language BiYacc, whose programs denote both a parser and a reflective printer for a fully disambiguated context-free grammar. BiYacc is based on the theory of bidirectional transformations, which helps to guarantee by construction that the generated pairs of parsers and reflective printers are consistent. Handling grammatical ambiguity is particularly challenging: we propose an approach based on generalised parsing and disambiguation filters, which produce all the parse results and (try to) select the only correct one in the parsing direction; the filters are carefully bidirectionalised so that they also work in the printing direction and do not break the consistency between the parsers and reflective printers. We show that BiYacc is capable of facilitating many tasks such as Pombrio and Krishnamurthi’s ‘resugaring’, simple refactoring, and language evolution.", Nothing)
                  ]
      }
  , Publication
      { title   = "Benchmarking bidirectional transformations: Theory, implementation, application, and assessment"
      , authors = [ "Anthony Anjorin"
                  , "Thomas Buchmann"
                  , "Bernhard Westfechtel"
                  , "Zinovy Diskin"
                  , "Hsiang-Shang Ko"
                  , "Romina Eramo"
                  , "Georg Hinkel"
                  , "Leila Samimi-Dehkordi"
                  , "Albert Zündorf"
                  ]
      , venue   = Just ("Software and Systems Modeling", Nothing)
      , year    = 2020
      , types   = []
      , links   = [ ("PDF", Just "accepted version", "/manuscripts/SoSyM20.pdf")
                  , ("Definitive version", Just "Springer Nature SharedIt", "https://rdcu.be/bQZ4q")
                  , ("Repository", Just "GitHub", "https://github.com/eMoflon/benchmarx")
                  ]
      , info    = [ ("Volume", "19", Nothing)
                  , ("Pages", "647–691", Nothing)
                  , ("DOI", "10.1007/s10270-019-00752-x", Nothing)
                  , ("Abstract", "Bidirectional transformations (bx) are relevant for a wide range of application domains. While bx problems may be solved with unidirectional languages and tools, maintaining separate implementations of forward and backward synchronizers with mutually consistent behavior can be difficult, laborious, and error-prone. To address the challenges involved in handling bx problems, dedicated languages and tools for bx have been developed. Due to their heterogeneity, however, the numerous and diverse approaches to bx are difficult to compare, with the consequence that fundamental differences and similarities are not yet well understood. This motivates the need for suitable benchmarks that facilitate the comparison of bx approaches. This paper provides a comprehensive treatment of benchmarking bx, covering theory, implementation, application, and assessment. At the level of theory, we introduce a conceptual framework that defines and classifies architectures of bx tools. At the level of implementation, we describe <em>Benchmarx</em>, an infrastructure for benchmarking bx tools which is based on the conceptual framework. At the level of application, we report on a wide variety of solutions to the well-known Families-to-Persons benchmark, which were developed and compared with the help of Benchmarx. At the level of assessment, we reflect on the usefulness of the Benchmarx approach to benchmarking bx, based on the experiences gained from the Families-to-Persons benchmark.", Nothing)
                  ]
      }
  , Publication
      { title   = "Retentive lenses"
      , authors = [ "Zirun Zhu"
                  , "Zhixuan Yang"
                  , "Hsiang-Shang Ko"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Nothing
      , year    = 2020
      , types   = [ (Unpublished, "Manuscript")
                  ]
      , links   = [ ("PDF", Nothing, "https://arxiv.org/pdf/2001.02031")
                  ]
      , info    = [ ("arXiv", "2001.02031", Nothing)
                  , ("DOI", "10.48550/arXiv.2001.02031", Nothing)
                  , ("Abstract", "Based on Foster et al.’s lenses, various bidirectional programming languages and systems have been developed for helping the user to write correct data synchronisers. The two well-behavedness laws of lenses, namely Correctness and Hippocraticness, are usually adopted as the guarantee of these systems. While lenses are designed to retain information in the source when the view is modified, well-behavedness says very little about the retaining of information: Hippocraticness only requires that the source be unchanged if the view is not modified, and nothing about information retention is guaranteed when the view is changed. To address the problem, we propose an extension of the original lenses, called <em>retentive lenses</em>, which satisfy a new Retentiveness law guaranteeing that if parts of the view are unchanged, then the corresponding parts of the source are retained as well. As a concrete example of retentive lenses, we present a domain-specific language for writing tree transformations; we prove that the pair of <em>get</em> and <em>put</em> functions generated from a program in our DSL forms a retentive lens. We demonstrate the practical use of retentive lenses and the DSL by presenting case studies on code refactoring, Pombrio and Krishnamurthi’s resugaring, and XML synchronisation.", Nothing)
                  ]
      }
  , Publication
      { title   = "Towards bidirectional synchronization between communicating processes and session types"
      , authors = [ "Liye Guo"
                  , "Hsiang-Shang Ko"
                  , "Keigo Imai"
                  , "Nobuko Yoshida"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Just ("Workshop on Software Foundations for Data Interoperability (SFDI)", Just ("http://biscuits.work/second-workshop/", IncludeYear))
      , year    = 2019
      , types   = [ (Published, "Short paper")
                  ]
      , links   = [ ("PDF", Just "accepted version", "/manuscripts/SFDI19.pdf") ]
      , info    = [ ("DOI", "10.1109/BIGCOMP.2019.8679265", Nothing)
                  , ("Abstract", "Session types are a type discipline for eliminating communication errors in concurrent computing. These types can be thought of as a representation of communication protocols implemented by communicating processes. One application scenario that can be naturally supported by session types is semantics-preserving transformation of processes in response to protocol changes due to optimization, evolution, refactoring, etc. Such transformation can be seen as a particular kind of synchronization problem that has long been studied by the bidirectional transformations (BX) community. This short paper offers a preliminary analysis of the process–type synchronization problem in terms of BX, describing the prospects and challenges.", Nothing)
                  ]
      }
  , Publication
      { title   = "Towards a visual editor for lens combinators"
      , authors = [ "Anthony Anjorin"
                  , "Hsiang-Shang Ko"
                  ]
      , venue   = Just ("International Workshop on Bidirectional Transformations (Bx)", Just ("https://2018.programming-conference.org/track/bx-2018-papers", IncludeYear))
      , year    = 2018
      , types   = [ (Published, "Extended abstract")
                  ]
      , links   = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/doi/10.1145/3191697.3191719?cid=81488664395")
                  , ("Slides", Just "PDF", "/slides/BX18.pdf")
                  ]
      , info    = [ ("Pages", "33–35", Nothing)
                  , ("DOI", "10.1145/3191697.3191719", Nothing)
                  , ("Abstract", "Languages for programming state-based asymmetric lenses are usually based on lens combinators, whose style, having a functional programming origin, is alien to most programmers and awkward to use even for experienced functional programmers. We propose a <em>visual syntax</em> mimicking circuit diagrams for the combinator-based language BiGUL, provide a relational interpretation that allows the diagrams to be understood bidirectionally, and sketch how an editor for the visual syntax can help to construct, understand, and debug lens combinator programs in an intuitive and friendly way.", Nothing)
                  ]
      }
  , Publication
      { title   = "Principles and practice of bidirectional programming in BiGUL"
      , authors = [ "Zhenjiang Hu"
                  , "Hsiang-Shang Ko"
                  ]
      , venue   = Just ("International Summer School on Bidirectional Transformations (Oxford, UK, 25–29 July 2016)", Just ("https://www.cs.ox.ac.uk/projects/tlcbx/ssbx/", ExcludeYear))
      , year    = 2018
      , types   = []
      , links   = [ ("PDF", Just "accepted version", "/manuscripts/SSBX16.pdf")
                  , ("Haskell code", Just "zip", "https://bitbucket.org/prl_tokyo/bigul/src/master/SSBX16/SSBX16-BiGUL-code.zip")
                  ]
      , info    = [ ("LNCS", "9715", Nothing)
                  , ("Chapter", "4", Nothing)
                  , ("Pages", "100–150", Nothing)
                  , ("DOI", "10.1007/978-3-319-79108-1_4", Nothing)
                  , ("Abstract", "Putback-based bidirectional programming allows the programmer to write only one putback transformation, from which the unique corresponding forward transformation is derived for free. A key distinguishing feature of putback-based bidirectional programming is full control over the bidirectional behavior, which is important for specifying intended bidirectional transformations without any ambiguity. In this chapter, we will introduce BiGUL, a simple yet powerful putback-based bidirectional programming language, explaining the underlying principles and showing how various kinds of bidirectional application can be developed in BiGUL.", Nothing)
                  ]
      }
  , Publication
      { title   = "An axiomatic basis for bidirectional programming"
      , authors = [ "Hsiang-Shang Ko"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Just ("Symposium on Principles of Programming Languages (POPL)", Just ("https://popl18.sigplan.org", IncludeYear))
      , year    = 2018
      , types   = []
      , links   = [ ("PDF", Nothing, "https://dl.acm.org/doi/pdf/10.1145/3158129")
                  , ("Agda code", Just "zip", "https://dl.acm.org/action/downloadSupplement?doi=10.1145%2F3158129&file=popl18-p3-aux.zip")
                  , ("Agda code", Just "HTML", "/Agda/POPL18/Everything.html")
                  , ("Slides", Just "PDF", "/slides/POPL18.pdf")
                  , ("Video", Just "YouTube", "https://youtu.be/-dNgQmRTYKg")
                  ]
      , info    = [ ("PACMPL", "2 (POPL)", Nothing)
                  , ("Article", "41", Nothing)
                  , ("Pages", "1–29", Nothing)
                  , ("DOI", "10.1145/3158129", Nothing)
                  , ("Agda version", "2.5.2 with Standard Library version 0.13", Nothing)
                  , ("Related slides", "‘<a href=\"/slides/BX18'.pdf\">Towards a general-purpose bidirectional language</a>’ at the <a href=\"https://2018.programmingconference.org/track/bx-2018-papers\">International Workshop on Bidirectional Transformations (Bx) 2018</a>", Nothing)
                  , ("Abstract", "Among the frameworks of bidirectional transformations proposed for addressing various synchronisation (consistency maintenance) problems, Foster et al.’s [2007] asymmetric lenses have influenced the design of a generation of bidirectional programming languages. Most of these languages are based on a declarative programming model, and only allow the programmer to describe a consistency specification with ad hoc and/or awkward control over the consistency restoration behaviour. However, synchronisation problems are diverse and require vastly different consistency restoration strategies, and to cope with the diversity, the programmer must have the ability to fully control and reason about the consistency restoration behaviour. The putback-based approach to bidirectional programming aims to provide exactly this ability, and this paper strengthens the putback-based position by proposing the first fully fledged reasoning framework for a bidirectional language — a Hoare-style logic for Ko et al.’s [2016] putback-based language BiGUL. The Hoare-style logic lets the BiGUL programmer precisely characterise the bidirectional behaviour of their programs by reasoning solely in the putback direction, thereby offering a unidirectional programming abstraction that is reasonably straightforward to work with and yet provides full control not achieved by previous approaches. The theory has been formalised and checked in Agda, but this paper presents the Hoare-style logic in a semi-formal way to make it easily understood and usable by the working BiGUL programmer.", Nothing)
                  ]
      }
  , Publication
      { title   = "Palgol: A high-level DSL for vertex-centric graph processing with remote data access"
      , authors = [ "Yongzhe Zhang"
                  , "Hsiang-Shang Ko"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Just ("Asian Symposium on Programming Languages and Systems (APLAS)", Just ("https://www-aplas.github.io", IncludeYear))
      , year    = 2017
      , types   = []
      , links   = [ ("PDF", Just "accepted version", "/manuscripts/APLAS17.pdf")
                  , ("Repository", Just "Bitbucket", "https://bitbucket.org/zyz915/palgol")
                  ]
      , info    = [ ("LNCS", "10695", Nothing)
                  , ("Pages", "301–320", Nothing)
                  , ("DOI", "10.1007/978-3-319-71237-6_15", Nothing)
                  , ("Technical report", "arXiv:1703.09542", Nothing)
                  , ("Abstract", "Pregel is a popular distributed computing model for dealing with large-scale graphs. However, it can be tricky to implement graph algorithms correctly and efficiently in Pregel’s vertex-centric model, especially when the algorithm has multiple computation stages, complicated data dependencies, or even communication over dynamic internal data structures. Some domain-specific languages (DSLs) have been proposed to provide more intuitive ways to implement graph algorithms, but due to the lack of support for <em>remote access</em> — reading or writing attributes of other vertices through references — they cannot handle the above mentioned dynamic communication, causing a class of Pregel algorithms with fast convergence impossible to implement.</p><p>To address this problem, we design and implement Palgol, a more declarative and powerful DSL which supports remote access. In particular, programmers can use a more declarative syntax called <em>chain access</em> to naturally specify dynamic communication as if directly reading data on arbitrary remote vertices. By analyzing the logic patterns of chain access, we provide a novel algorithm for compiling Palgol programs to efficient Pregel code. We demonstrate the power of Palgol by using it to implement several practical Pregel algorithms, and the evaluation result shows that the efficiency of Palgol is comparable with that of hand-written code.", Nothing)
                  ]
      }
  , Publication
      { title   = "BenchmarX reloaded: A practical benchmark framework for bidirectional transformations"
      , authors = [ "Anthony Anjorin"
                  , "Zinovy Diskin"
                  , "Frédéric Jouault"
                  , "Hsiang-Shang Ko"
                  , "Erhan Leblebici"
                  , "Bernhard Westfechtel"
                  ]
      , venue   = Just ("International Workshop on Bidirectional Transformations (Bx)", Just ("http://bx-community.wikidot.com/bx2017:home", IncludeYear))
      , year    = 2017
      , types   = [ (Warning, "Superseded")
                  ]
      , links   = [ ("PDF", Nothing, "http://ceur-ws.org/Vol-1827/paper6.pdf")
                  , ("Repository", Just "GitHub", "https://github.com/eMoflon/benchmarx")
                  ]
      , info    = [ ("Pages", "15–30", Nothing)
                  , ("URL", "http://ceur-ws.org/Vol-1827/paper6.pdf", Nothing)
                  , ("Note", "This paper has been superseded by an <a href=\"#publication-cb9d73cc\">extended version in Software and Systems Modelling</a>.", Nothing)
                  , ("Abstract", "Bidirectional transformation (bx) approaches provide a systematic way of specifying, restoring, and maintaining the consistency of related models. The current diversity of bx approaches is certainly beneficial, but it also poses challenges, especially when it comes to comparing the different approaches and corresponding bx tools that implement them. Although a benchmark for bx (referred to as a <em>benchmarx</em>) has been identified in the community as an important and currently still missing contribution, only a rather abstract description and characterisation of what a benchmarx should be has been published to date. In this paper, therefore, we focus on providing a practical and pragmatic framework, on which future concrete benchmarx can be built. To demonstrate its feasibility, we present a first non-trivial benchmarx based on a well-known example, and use it to compare and evaluate three bx tools, chosen to cover the broad spectrum of bx approaches.", Nothing)
                  ]
      }
  , Publication
      { title   = "Programming with ornaments"
      , authors = [ "Hsiang-Shang Ko"
                  , "Jeremy Gibbons"
                  ]
      , venue   = Just ("Journal of Functional Programming", Nothing)
      , year    = 2017
      , types   = []
      , links   = [ ("PDF", Nothing, "https://www.cambridge.org/core/services/aop-cambridge-core/content/view/97C63D7C90556ACD2EC1482E63026A74/S0956796816000307a.pdf/programming-with-ornaments.pdf")
                  , ("Agda code", Just "zip", "https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S0956796816000307/resource/name/S0956796816000307sup001.zip")
                  , ("Agda code", Just "HTML", "/Agda/JFP17/Everything.html")
                  ]
      , info    = [ ("Volume", "27", Nothing)
                  , ("Article", "e2", Nothing)
                  , ("Pages", "1–43", Nothing)
                  , ("DOI", "10.1017/S0956796816000307", Nothing)
                  , ("Agda version", "2.5.1.1 with Standard Library version 0.12", Nothing)
                  , ("Abstract", "Dependently typed programming advocates the use of various indexed versions of the same shape of data, but the formal relationship amongst these structurally similar datatypes usually needs to be established manually and tediously. Ornaments have been proposed as a formal mechanism to manage the relationships between such datatype variants. In this paper, we conduct a case study under an ornament framework; the case study concerns programming binomial heaps and their operations — including insertion and minimum extraction — by viewing them as lifted versions of binary numbers and numeric operations. We show how current dependently typed programming technology can lead to a clean treatment of the binomial heap constraints when implementing heap operations. We also identify some gaps between the current technology and an ideal dependently typed programming language that we would wish to have for our development.", Nothing)
                  ]
      }
  , Publication
      { title   = "Parsing and reflective printing, bidirectionally"
      , authors = [ "Zirun Zhu"
                  , "Yongzhe Zhang"
                  , "Hsiang-Shang Ko"
                  , "Pedro Martins"
                  , "João Saraiva"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Just ("International Conference on Software Language Engineering (SLE)", Just ("http://www.sleconf.org/2016/", IncludeYear))
      , year    = 2016
      , types   = [ (Warning, "Superseded")
                  ]
      , links   = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/doi/10.1145/2997364.2997369?cid=81488664395")
                --   , ("Demo site", Nothing, "https://biyacc.k331.one")
                  ]
      , info    = [ ("Pages", "2–14", Nothing)
                  , ("DOI", "10.1145/2997364.2997369", Nothing)
                  , ("Note", "This paper has been superseded by an <a href=\"#publication-5b02d923\">extended version in New Generation Computing</a>.", Nothing)
                  , ("Abstract", "Language designers usually need to implement parsers and printers. Despite being two intimately related programs, in practice they are often designed separately, and then need to be revised and kept consistent as the language evolves. It will be more convenient if the parser and printer can be unified and developed in one single program, with their consistency guaranteed automatically.</p><p>Furthermore, in certain scenarios (like showing compiler optimisation results to the programmer), it is desirable to have a more powerful <em>reflective</em> printer that, when an abstract syntax tree corresponding to a piece of program text is modified, can reflect the modification to the program text while preserving layouts, comments, and syntactic sugar.</p><p>To address these needs, we propose a domain-specific language BiYacc, whose programs denote both a parser and a reflective printer for an unambiguous context-free grammar. BiYacc is based on the theory of <em>bidirectional transformations</em>, which helps to guarantee by construction that the pairs of parsers and reflective printers generated by BiYacc are consistent. We show that BiYacc is capable of facilitating many tasks such as Pombrio and Krishnamurthi’s ‘resugaring’, language evolution, and refactoring.", Nothing)
                  ]
      }
--, Publication
--    { title   = "The under-appreciated put: Implementing delta-alignment in BiGUL"
--    , authors = [ "Jorge Mendes"
--                , "Hsiang-Shang Ko"
--                , "Zhenjiang Hu"
--                ]
--    , venue   = Just ("GRACE Center, National Institute of Informatics", Nothing)
--    , year    = 2016
--    , types   = [ (Unpublished, "Technical report")
--                ]
--    , links   = [ ("PDF", Just "revised version", "/manuscripts/BiGUL-delta-alignment.pdf")
--                ]
--    , info    = [ ("Number", "GRACE-TR 2016-03", Nothing)
--                , ("URL", "http://grace-center.jp/wp-content/uploads/2016/04/GRACE-TR-2016-03.pdf", Nothing)
--                , ("Note", "The revised version uses a concrete running example and contains more explanations.", Nothing)
--                , ("Abstract", "There are two approaches to bidirectional programming. One is the get-based method where one writes <em>get</em> and <em>put</em> is automatically derived, and the other is the put-based method where one writes <em>put</em> and <em>get</em> is automatically derived. In this paper, we argue that the put-based method deserves more attention, because a good language for programming <em>put</em> can not only give full control over the behavior of bidirectional transformations, but also enable us to efficiently develop various domain-specific bidirectional languages and use them seamlessly in one framework, which would be non-trivial with the get-based method. We demonstrate how the matching/delta/generic lenses can be implemented in BiGUL, a putback-based bidirectional language.", Nothing)
--                ]
--    }
--, Publication
--    { title   = "Brul: A putback-based bidirectional transformation library for updatable views"
--    , authors = [ "Tao Zan"
--                , "Li Liu"
--                , "Hsiang-Shang Ko"
--                , "Zhenjiang Hu"
--                ]
--    , venue   = Just ("International Workshop on Bidirectional Transformations (Bx)", Just ("http://bx-community.wikidot.com/bx2016:home", IncludeYear))
--    , year    = 2016
--    , types   = []
--    , links   = [ ("PDF", Nothing, "http://ceur-ws.org/Vol-1571/paper_3.pdf")
--                ]
--    , info    = [ ("Pages", "77–89", Nothing)
--                , ("URL", "http://ceur-ws.org/Vol-1571/paper_3.pdf", Nothing)
--                , ("Abstract", "In work on relational databases, the view-update problem is about how to translate update operations on the view table to corresponding update operations on the source table properly. It is a problem that the translation policies are not unique in many situations. Relational lenses try to solve this problem by providing a list of combinators that let the user write <em>get</em> functions (queries) with specified updated policies for <em>put</em> functions (updates); however this can only provide limited control of update policies which still may not satisfy the user’s real needs. In this paper, we implement a library Brul that provides putback-based basic combinators for the user to write the <em>put</em> function with flexible update policies easily; from the <em>put</em> function, a unique <em>get</em> function can be derived automatically. Brul is implemented in terms of BiGUL, a core bidirectional programming language which has been formalized in Agda and implemented as a Haskell library.", Nothing)
--                ]
--    }
  , Publication
      { title   = "BiGUL: A formally verified core language for putback-based bidirectional programming"
      , authors = [ "Hsiang-Shang Ko"
                  , "Tao Zan"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Just ("Workshop on Partial Evaluation and Program Manipulation (PEPM)", Just ("http://conf.researchr.org/home/pepm-2016", IncludeYear))
      , year    = 2016
      , types   = []
      , links   = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/doi/10.1145/2847538.2847544?cid=81488664395")
                  , ("Agda code", Just "zip", "https://dl.acm.org/action/downloadSupplement?doi=10.1145%2F2847538.2847544&file=p61-ko-s.zip")
                  , ("Agda code", Just "HTML", "/Agda/PEPM16/BiGUL.Everything.html")
                  , ("Repository", Just "Bitbucket", "https://bitbucket.org/prl_tokyo/bigul")
                  , ("Slides", Just "PDF", "/slides/PEPM16.pdf")
                  ]
      , info    = [ ("Pages", "61–72", Nothing)
                  , ("DOI", "10.1145/2847538.2847544", Nothing)
                  , ("Agda version", "2.4.2.4 with Standard Library version 0.11", Nothing)
                  , ("Note", "The version of BiGUL described in this paper is outdated. See the <a href=\"#publication-e124d6a0\">‘axiomatic basis’ paper</a> for a semi-formal axiomatic introduction to the current BiGUL.", Nothing)
                  , ("Abstract", "Putback-based bidirectional programming allows the programmer to write only one putback transformation, from which the unique corresponding forward transformation is derived for free. The logic of a putback transformation is more sophisticated than that of a forward transformation and does not always give rise to well-behaved bidirectional programs; this calls for more robust language design to support development of well-behaved putback transformations. In this paper, we design and implement a concise core language BiGUL for putback-based bidirectional programming to serve as a foundation for higher-level putback-based languages. BiGUL is completely formally verified in the dependently typed programming language Agda to guarantee that any putback transformation written in BiGUL is well-behaved.", Nothing)
                  ]
      }
  , Publication
      { title   = "BiYacc: Roll your parser and reflective printer into one"
      , authors = [ "Zirun Zhu"
                  , "Hsiang-Shang Ko"
                  , "Pedro Martins"
                  , "João Saraiva"
                  , "Zhenjiang Hu"
                  ]
      , venue   = Just ("International Workshop on Bidirectional Transformations (Bx)", Just ("http://bx-community.wikidot.com/bx2015:home", IncludeYear))
      , year    = 2015
      , types   = [ (Published, "Tool paper")
                  ]
      , links   = [ ("PDF", Nothing, "http://ceur-ws.org/Vol-1396/p43-zhu.pdf")
                --   , ("Demo site", Nothing, "https://biyacc.k331.one")
                  ]
      , info    = [ ("Pages", "43–50", Nothing)
                  , ("URL", "http://ceur-ws.org/Vol-1396/p43-zhu.pdf", Nothing)
                  , ("See also", "the more complete <a href=\"#publication-4342d3dd\">SLE’16 paper</a>, which, in particular, describes the internals of BiYacc in much more detail.", Nothing)
                  , ("Abstract", "Language designers usually need to implement parsers and printers. Despite being two related programs, in practice they are designed and implemented separately. This approach has an obvious disadvantage: as a language evolves, both its parser and printer need to be separately revised and kept synchronised. Such tasks are routine but complicated and error-prone. To facilitate these tasks, we propose a language called BiYacc, whose programs denote both a parser and a printer. In essence, BiYacc is a domain-specific language for writing <em>putback-based</em> bidirectional transformations — the printer is a putback transformation, and the parser is the corresponding get transformation. The pairs of parsers and printers generated by BiYacc are thus always guaranteed to satisfy the usual round-trip properties. The highlight that distinguishes this <em>reflective</em> printer from others is that the printer — being a putback transformation — accepts not only an abstract syntax tree but also a string, and produces an updated string consistent with the given abstract syntax tree. We can thus make use of the additional input string, with mechanisms such as simultaneous pattern matching on the view and the source, to provide users with full control over the printing-strategies.", Nothing)
                  ]
      }
  , Publication
      { title   = "Analysis and synthesis of inductive families"
      , authors = [ "Hsiang-Shang Ko"
                  ]
      , venue   = Just ("University of Oxford", Nothing)
      , year    = 2014
      , types   = [ (Published, "DPhil dissertation")
                  ]
      , links   = [ ("PDF", Just "revised version", "https://github.com/josh-hs-ko/dissertation/raw/master/dissertation.pdf")
                  , ("Agda code", Just "GitHub", "https://github.com/josh-hs-ko/Thesis")
                  , ("Agda code", Just "HTML", "/Agda/Thesis/Everything.html")
                  ]
      , info    = [ ("URL", "https://ora.ox.ac.uk/objects/ora:9019", Nothing)
                  , ("Repository", "https://github.com/josh-hs-ko/dissertation", Nothing)
                  , ("Agda version", "2.5.2 with Standard Library version 0.13", Nothing)
                  , ("Note", "The revised version uses https DOI links and fixes identifier hyperlinks and a few typos in the Bodleian version.", Nothing)
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
      , links    = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/doi/10.1145/2502409.2502413?cid=81488664395")
                   , ("Agda code", Just "zip", "https://dl.acm.org/action/downloadSupplement?doi=10.1145%2F2502409.2502413&file=dtp04.zip")
                   , ("Slides", Just "PDF", "/slides/DTP13.pdf")
                   ]
      , info     = [ ("Pages", "37–48", Nothing)
                   , ("DOI", "10.1145/2502409.2502413", Nothing)
                   , ("Agda version", "2.3.3 with Standard Library version 0.6", Nothing)
                   , ("See also", "my <a href=\"#publication-9c9a95f3\">DPhil dissertation</a>, which subsumes and streamlines the content of this paper, and includes more examples.", Nothing)
                   , ("Abstract", "Dependently typed programming is hard, because ideally dependently typed programs should share structure with their correctness proofs, but there are very few guidelines on how one can arrive at such integrated programs. McBride’s algebraic ornamentation provides a methodological advancement, by which the programmer can derive a datatype from a specification involving a fold, such that a program that constructs elements of that datatype would be correct by construction. It is thus an effective method that leads the programmer from a specification to a dependently typed program. We enhance the applicability of this method by generalising algebraic ornamentation to a relational setting and bringing in relational algebraic methods, resulting in a hybrid approach that makes essential use of both dependently typed programming and relational program derivation. A dependently typed solution to the minimum coin change problem is presented as a demonstration of this hybrid approach. We also give a theoretically interesting ‘completeness theorem’ of relational algebraic ornaments, which sheds some light on the expressive power of ornaments and inductive families.", Nothing)
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
      , links    = [ ("PDF", Nothing, "http://www.nii.ac.jp/pi/n10/10_65.pdf")
                   , ("Agda code", Just "zip", "/Agda/PI13/PI13.zip")
                   , ("Agda code", Just "HTML", "/Agda/PI13/Everything.html")
                   ]
      , info     = [ ("Number", "10", Nothing)
                   , ("Pages", "65–88", Nothing)
                   , ("DOI", "10.2201/NiiPi.2013.10.5", Nothing)
                   , ("See also", "my <a href=\"#publication-9c9a95f3\">DPhil dissertation</a>, which subsumes and streamlines the content of this paper, and includes more examples.", Nothing)
                   , ("Abstract", "Dependently typed programmers are encouraged to use inductive families to integrate constraints with data construction. Different constraints are used in different contexts, leading to different versions of datatypes for the same data structure. For example, sequences might be constrained by length or by an ordering on elements, giving rise to different datatypes ‘vectors’ and ‘sorted lists’ for the same underlying data structure of sequences. Modular implementation of common operations for these structurally similar datatypes has been a longstanding problem. We propose a datatype-generic solution, in which we axiomatise a family of isomorphisms between datatypes and their more refined versions as datatype refinements, and show that McBride’s ornaments can be translated into such refinements. With the ornament-induced refinements, relevant properties of the operations can be separately proven for each constraint, and after the programmer selects several constraints to impose on a basic datatype and synthesises a new datatype incorporating those constraints, the operations can be routinely upgraded to work with the synthesised datatype.", Nothing)
                   ]
      }
  , Publication
      { title    = "Modularising inductive families"
      , authors  = [ "Hsiang-Shang Ko"
                   , "Jeremy Gibbons"
                   ]
      , venue    = Just ("Workshop on Generic Programming (WGP)", Just ("https://dl.acm.org/citation.cfm?id=2036918", IncludeYear))
      , year     = 2011
      , types    = [ (Warning, "Superseded")
                   ]
      , links    = [ ("PDF", Just "ACM Author-Izer", "https://dl.acm.org/doi/10.1145/2036918.2036921?cid=81488664395")
                   , ("Agda code", Just "HTML", "/Agda/WGP11/OAOAOO.html")
                   , ("Slides", Just "PDF", "/slides/WGP11.pdf")
                   ]
      , info     = [ ("Pages", "13–24", Nothing)
                   , ("DOI", "10.1145/2036918.2036921", Nothing)
                   , ("Related slides", "Same topic at the <a href=\"/slides/DTP11.pdf\">Workshop on Dependently Typed Programming (DTP) 2011</a>, and ‘numerical representations à la ornamentation’ at <a href=\"/slides/FitA12.pdf\">Fun in the Afternoon 2012</a>", Nothing)
                   , ("Note", "This paper has been superseded by an <a href=\"#publication-685cb02c\">extended version in <em>Progress in Informatics</em></a>.", Nothing)
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
      , links    = [ ("PDF", Nothing, "https://www.cambridge.org/core/services/aop-cambridge-core/content/view/ACA0C08F29621A892FB0C0B745254D15/S0956796809007345a.pdf/algebra-of-programming-in-agda-dependent-types-for-relational-program-derivation.pdf")
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
      , links    = [ ("PDF", Just "accepted version", "/manuscripts/MPC08.pdf")
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
