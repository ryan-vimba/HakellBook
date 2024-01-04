module RPN where

data MemoryOperation = ReadAt | WriteAt
data Operation = Sum | Len | Text | Concat | Memory MemoryOperation Int
data Value = IntValue Int | StrValue String | Empty
data ProgramStep = OperationStep Operation | OperandStep Value
data Result = Operands [Value] | Computed Value | Error String
data SingletonResult = Single Value | SingletonError String
data Runtime = Runtime [ProgramStep] [Value] Result        -- remaining steps to execute, memory, current result (so far)  
data StringResult = StringResult String | StringResultError String
data MemoryWriteResult = NewMemoryContents [Value] | FailedMemoryWrite String

-- Symbolic Program Step shortcuts
(.>) idx = OperationStep (Memory ReadAt idx)
(>.) idx = OperationStep (Memory WriteAt idx)
(#+) = OperationStep Sum
(|$) = OperationStep Len
(>$) = OperationStep Text   -- converts anything to a string
(+$) = OperationStep Concat
(#) n = OperandStep (IntValue n)
($$) s = OperandStep (StrValue s) 

-- Reallocate "memory" to a specific size (unless if the list is already large enough)
realloc :: [Value] -> Int -> [Value]
realloc l n = if n > (length l) then replicate n Empty else replicate (length l) Empty

-- Allocate "memory" based of the steps contained within the "Program"
allocFor :: [ProgramStep] -> [Value]
allocFor steps = alloc steps []
                 where 
                    alloc :: [ProgramStep] -> [Value] -> [Value]
                    alloc rem mem = case rem of
                                        (OperationStep (Memory _ idx) : t)  -> (alloc t (realloc mem (idx + 1)))  
                                        (_ : t)                             -> (alloc t mem)  
                                        []                                  -> mem

-- Accumulate values during program runtime
accumulate :: Runtime -> Runtime
accumulate r = case r of
                   (Runtime (OperationStep (Memory ReadAt idx) : pst) mem prev) -> accumulateOpReadFromMemory pst mem idx prev
                   (Runtime (OperationStep _ : _) _ _) -> r     -- When we hit a non-memory-read operation: we stop accumulating
                   (Runtime [] mem prev) -> Runtime [] mem prev     
                   (Runtime (OperandStep operand : pst) mem (Computed _)) -> accumulate (Runtime pst mem (Operands (operand : [])))
                   (Runtime (OperandStep operand : pst) mem (Operands acc)) -> accumulate (Runtime pst mem (Operands (acc ++ (operand : []))))
               where accumulateOpReadFromMemory :: [ProgramStep] -> [Value] -> Int -> Result -> Runtime
                     accumulateOpReadFromMemory pst mem idx prev = if idx >= 0 || idx < (length mem) 
                                                               then
                                                                    case prev of
                                                                       (Computed _) -> accumulate (Runtime pst mem (Operands ((mem !! idx) : [])))
                                                                       (Operands acc) -> accumulate (Runtime pst mem (Operands (acc ++ ((mem !! idx) : []))))
                                                                       (Error err) -> Runtime pst mem (Error err)
                                                               else
                                                                    Runtime pst mem (Error "Memory read index is out-of-range.")                        

-- "Write" a value to a "memory" location
write :: [Value] -> Int -> Value -> MemoryWriteResult
write [] idx val = FailedMemoryWrite "Cannot write to non-existant memory."
write mem idx val
  | idx < 0 = FailedMemoryWrite "Cannot write to memory at a negative index."
  | idx >= (length mem) = FailedMemoryWrite "Cannot write past the end of allocated memory."
  | otherwise = NewMemoryContents (take idx mem ++ [val] ++ drop (idx + 1) mem)    

-- Peform a single operation given memory and previous result (which should contain operands). will return result and resulting memory contents.
performOperation :: Operation -> [Value] -> Result -> (Result, [Value])
performOperation op mem prev = case op of
                                   Sum -> case prev of
                                                (Computed (IntValue i)) -> (Computed (IntValue i), mem)
                                                (Computed (StrValue _)) -> (Computed (IntValue 0), mem)  -- If we see a string, it is ignored and total is 0
                                                (Operands values) -> (sumAll 0 values, mem)
                                                (Error err) -> (Error ("Operation 'Sum' being performed on previous error: " ++ err), mem)
                                   Text -> case (lastOperandOf prev) of
                                                (Single (IntValue i)) -> (Computed (StrValue (show i)), mem)
                                                (Single (StrValue s)) -> (Computed (StrValue s), mem)
                                                (SingletonError err) -> (Computed (StrValue err), mem)
                                   Len -> case  (lastOperandOf prev) of
                                                (Single (IntValue _)) -> (Error "Operation 'Len' being performed on an integer. String expected.", mem)
                                                (Single (StrValue s)) -> (Computed (IntValue (length s)), mem)
                                                (SingletonError err) -> (Error ("Operation 'Len' being performed on a previous error:" ++ err), mem)             
                                   Concat -> case prev of
                                                (Computed (IntValue i)) -> (Error "Operation 'Concat' being performed on an integer. String expected.", mem)
                                                (Computed (StrValue s)) -> (Computed (StrValue s), mem)
                                                (Operands values) -> case (concatAll "" values) of
                                                                       (StringResult s) -> (Computed (StrValue s), mem)
                                                                       (StringResultError err) -> (Error err, mem)
                                                (Error err) -> (Error ("Operation 'Concat' being performed on a previous error:" ++ err), mem)
                                   Memory WriteAt idx -> case (lastOperandOf prev) of
                                                          (Single val) -> case (write mem idx val) of
                                                                              (NewMemoryContents newMem) -> (Computed val, newMem)
                                                                              (FailedMemoryWrite err) -> (Error err, mem)            
                                                          (SingletonError err) -> (Error ("Operation 'WriteAt' being performed on a previous error:" ++ err), mem)  
                               where sumAll :: Int -> [Value] -> Result
                                     sumAll total values = case values of
                                                               [] -> Computed (IntValue total)
                                                               ((IntValue i) : t) -> sumAll (total + i) t
                                                               ((StrValue s) : t) -> sumAll total t    -- strings are ignored and we continue totaling integer values
                                                               (Empty : t) -> sumAll total t   -- an "empty" is also ignored and totaling continues
                                     concatAll :: String -> [Value] -> StringResult
                                     concatAll str values = case values of
                                                                [] -> StringResult str
                                                                ((IntValue _) : _) -> StringResultError "Operation 'Concat' expects string parameters. Found an integer."
                                                                ((StrValue s) : t) -> concatAll (str ++ s) t
                                                                (Empty : t) -> concatAll str t 
                                     lastOperandOf :: Result -> SingletonResult
                                     lastOperandOf r = case r of
                                                         (Operands values) -> case (reverse values) of
                                                                                  [] -> SingletonError "Expected a single computed operand. Found Zero."
                                                                                  (h:_) -> (Single h)
                                                         (Computed value) -> (Single value)
                                                         (Error err) -> SingletonError ("Attempting to extract operand from previous error:" ++ err)                         
                                                                                           

execute :: [ProgramStep] -> String                                                  -- for simplicity we capture final result as a String
execute program = let initialized = Runtime program (allocFor program) (Computed Empty)
                  in computeValue initialized
                  where computeValue :: Runtime -> String
                        computeValue runtime = case runtime of
                                                   (Runtime [] _ (Computed (IntValue i))) -> "Integer Result: " ++ (show i)
                                                   (Runtime [] _ (Computed (StrValue s))) -> "String Result: " ++ s
                                                   (Runtime [] _ (Computed Empty)) -> "<Empty Result>"
                                                   (Runtime [] _ (Error s)) -> "Error Result: " ++ s
                                                   _ -> computeValue (nextStep runtime) 
                        nextStep :: Runtime -> Runtime
                        nextStep runtime = case runtime of
                                               (Runtime [] mem result) -> Runtime [] mem result    -- wierd bug here
                                               (Runtime (OperandStep _ : _) _ _) -> accumulate runtime
                                               (Runtime (OperationStep (Memory ReadAt _) : _) _ _) -> accumulate runtime 
                                               (Runtime (OperationStep operation : pst) mem acc) -> let (opResult, newMem) = performOperation operation mem acc
                                                                                                    in case opResult of (Error err) -> Runtime [] [] (Error err)
                                                                                                                        _ -> Runtime pst newMem opResult
