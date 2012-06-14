module Main where
import System.Environment
import Test.HUnit
import Monad
import qualified Scheme

tests = test [ "parseDec"	~: "(readExpr 1234)"		~: "Number: 1234"	~=? (Scheme.readExpr "1234"),
	       "parseDec"	~: "(readExpr #d443)"		~: "Number: 443"	~=? (Scheme.readExpr "#d443"),
	       "parseOct"	~: "(readExpr #o10)"		~: "Number: 8"		~=? (Scheme.readExpr "#o10"),
	       "parseHex"	~: "(readExpr #xFf)"		~: "Number: 255"	~=? (Scheme.readExpr "#xFf"),
	       "parseBin"	~: "(readExpr #b10001101)"	~: "Number: 141"	~=? (Scheme.readExpr "#b10001101"),
	       "parseTrue"	~: "(readExpr #t)"		~: "Bool: True"		~=? (Scheme.readExpr "#t"),
	       "parseFalse"	~: "(readExpr #f)"		~: "Bool: False"	~=? (Scheme.readExpr "#f"),
	       "parseAtom"	~: "(readExpr asdf)"		~: "Atom: asdf"		~=? (Scheme.readExpr "asdf"),
	       "parseAtom"	~: "(readExpr $sym)"		~: "Atom: $sym"		~=? (Scheme.readExpr "$sym"),
	       "parseAtom"	~: "(readExpr sym123)"		~: "Atom: sym123"	~=? (Scheme.readExpr "sym123"),
	       "parseString"	~: "(readExpr \"asdf\")"	~: "String: asdf"	~=? (Scheme.readExpr "\"asdf\""),
	       "parseString"	~: "(readExpr \"as\\\"hsd\")"	~: "String: as\\\"hsd"	~=? (Scheme.readExpr "\"as\\\\hsd\"") ]

main :: IO ()
main = do c <- runTestTT tests
	  return ()
