window.MathJax = {
  tex: {
    inlineMath: [ ['$', '$'], ['\\(', '\\)'] ],
    displayMath: [ ['$$', '$$'], ['\\[', '\\]'] ],
    processEscapes: true,
    autoload: {
      color: [],
      colorV2: ['color']
    },
    packages: {'[+]': ['noerrors']},
    tags: 'ams',
    tagSide: 'left',
    macros: {
      awa: ['\\rlap{#2}\\phantom{#1}', 2],
      and: '\\mathrel{\\thinspace\\wedge\\thinspace}',
      braces: ['\\{ \\thinspace #1 \\thinspace \\}', 1],
      Coloneqq: '\\mathrel{\\vcenter{::}}=',
      defeq: '\\mathrel{\\vcenter{:}}=',
      Nat: '\\mathbb{N}',
      or: '\\mathrel{\\thinspace\\vee\\thinspace}',
      Order: '\\mathcal{O}',
      reason: ['\\quad \\{ \\thinspace \\text{#1} \\thinspace \\}', 1],
      sem: ['[\\![#1]\\!]', 1],
      ket: ['|#1\\rangle', 1],
      bra: ['\\langle#1|', 1]
    }
  },
  chtml: {
    displayAlign: 'left',
    displayIndent: '1.5em'
  },
  options: {
    ignoreHtmlClass: 'tex2jax_ignore',
    processHtmlClass: 'tex2jax_process'
  },
  loader: {
    load: ['[tex]/noerrors']
  }
};
