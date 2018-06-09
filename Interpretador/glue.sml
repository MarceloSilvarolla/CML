structure CMLLrVals = CMLLrValsFun(
               structure Token = LrParser.Token);
structure CMLLex =    CMLLexFun(
               structure Tokens = CMLLrVals.Tokens);
structure CMLParser = JoinWithArg(
               structure ParserData = CMLLrVals.ParserData
               structure Lex=CMLLex
               structure LrParser=LrParser);

