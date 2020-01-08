module Lib (
    CoreProgram,
    Program,
    Name,
    Expr(..),
    parse,
    parseExpr,
    parseProgr,
) where

import Core
import Parse (parse)
import ParseExpr (parseExpr)
import ParseProgr (parseProgr)
