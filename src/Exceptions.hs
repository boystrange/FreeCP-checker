-- MIT License
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- |Representation of FairCheck-specific syntax and typing errors.
module Exceptions where

import Atoms
import Type
import Measure
import Render ()
import Control.Exception (Exception)
import qualified Data.List as List

-- |The type of FairCheck exceptions.
data MyException
  = ErrorSyntax String
  | ErrorMultipleTypeDefinitions TypeName
  | ErrorMultipleProcessDefinitions ProcessName
  | ErrorMultipleNameDeclarations ChannelName
  | ErrorUnknownIdentifier String String
  | ErrorActionUnbounded ProcessName
  | ErrorCastUnbounded ProcessName ChannelName
  | ErrorSessionUnbounded ProcessName ChannelName
  | ErrorTypeUnbounded ChannelName
  | ErrorTypeNonContractive TypeM
  | ErrorProcessNonContractive
  | ErrorTypeMismatch ChannelName String TypeM
  | ErrorArityMismatch ProcessName Int Int
  | ErrorLabelMismatch ChannelName [Label] [Label]
  | ErrorInvalidType String
  | ErrorLinearity [ChannelName]
  | ErrorRuntime String
  | ErrorGeneric
  | ErrorDebug String
  | ErrorNotImplemented String

instance Exception MyException

instance Show MyException where
  show (ErrorSyntax msg) = msg
  show (ErrorMultipleTypeDefinitions tname) = "multiple type definitions: " ++ showWithPos tname
  show (ErrorMultipleProcessDefinitions pname) = "multiple process definitions: " ++ showWithPos pname
  show (ErrorUnknownIdentifier kind name) = "unknown " ++ kind ++ ": " ++ name
  show (ErrorMultipleNameDeclarations u) = "multiple declarations: " ++ showWithPos u
  show (ErrorTypeMismatch name e t) = "type error: " ++ showWithPos name ++ ": expected " ++ e ++ ", actual " ++ show t
  show (ErrorArityMismatch pname expected actual) =
    "arity mismatch for " ++ showWithPos pname ++ ": expected " ++
    show expected ++ ", actual " ++ show actual
  show (ErrorInvalidType msg) = "invalid type: " ++ msg
  show (ErrorActionUnbounded pname) = "action-unbounded process: " ++ showWithPos pname
  show (ErrorSessionUnbounded pname name) = "session-unbounded process: " ++ showWithPos pname ++ " creates " ++ showWithPos name
  show (ErrorCastUnbounded pname name) = "cast-unbounded process: " ++ showWithPos pname ++ " casts " ++ showWithPos name
  show (ErrorLinearity pnames) = "linearity violation: " ++ List.intercalate ", " (map showWithPos pnames)
  show (ErrorLabelMismatch name elabels alabels) = "labels mismatch: " ++ showWithPos name ++ ": expected " ++ show elabels ++ ", actual " ++ show alabels
  show (ErrorTypeUnbounded name) = "unbounded type: " ++ showWithPos name
  show (ErrorTypeNonContractive t) = "non-contractive type: " ++ show t
  show ErrorProcessNonContractive = "non-contractive process definitions"
  show (ErrorRuntime msg) = "runtime error: " ++ msg
  show ErrorGeneric = "generic error"
  show (ErrorDebug msg) = "debug " ++ msg
  show (ErrorNotImplemented msg) = "not implemented: " ++ msg
