module TinyDefinitions where

  -- TODO: Add Nodes for Addition, Subtraction, Multiplication, Division, Remainder
  data ParseTree = AndNode ParseTree ParseTree |
                   OrNode ParseTree ParseTree  |
                   AdditionNode ParseTree ParseTree |
                   SubstractionNode ParseTree ParseTree |
                   MultiplicationNode ParseTree ParseTree |
                   DividionNode ParseTree ParseTree |
                   RemainderNode ParseTree |
                   NotNode ParseTree           |
                   ValueNode ValueType         |
                   IdNode String               |
                   LetNode String ParseTree ParseTree |
                   LambdaNode String ParseTree |
                   CallNode String ParseTree |
                   EmptyNode
                    deriving (Show)
  
  -- closure structure

  data ClosureStructure = Closure String ParseTree EnvType 
                          deriving (Show)

  -- TODO: Add IntegerType and PairType below
  data ValueType = BoolType Bool | 
                   IntegerType Integer |
                   PairType () |
                   ClosureType ClosureStructure
                    deriving (Show)

  type EnvType = [(String,ValueType)]
