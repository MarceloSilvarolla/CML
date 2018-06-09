# CML

Linguagem para Machine Learning baseada em C.

## Como rodar o interpretador
Na linha de comando, faça o seguinte:
$ sml
- CM.make "CML.cm";
- CML.interpret "arquivoAInterpretar.cml";

## Construído com
* [MLLex](https://www.smlnj.org/doc/ML-Lex) - gerador de analisadores léxicos
* [MLYacc](http://www.smlnj.org/doc/ML-Yacc/) - gerador de analisadores sintáticos
* [SML/NJ Compilation Manager](https://www.smlnj.org/doc/CM/index.html) - gerenciador de compilação para uso de MLLex e MLYacc


## Referências utilizadas

* [User's guide to MLLex and MLYacc](www.cs.tufts.edu/comp/181/ug.pdf) - guia de MLLex, MLYacc e SML/NJ Compilation Manager
* [ANSI C Yacc grammar](http://www.quut.com/c/ANSI-C-grammar-y.html) - gramática em Yacc e Lex para ANSI C
* [ANSI C Lex specification](http://www.quut.com/c/ANSI-C-grammar-l-2011.html) - especificação Lex para ANSI C
