module Types.CLI.PrintTrace
( PrintTrace(PrintTrace)
)
where
import Test.Tasty.Options
import Data.Typeable (Typeable)

newtype PrintTrace = PrintTrace Bool
    deriving (Eq, Ord, Typeable)

instance IsOption PrintTrace where
  defaultValue = PrintTrace False
  parseValue = fmap PrintTrace . safeReadBool
  optionName = return "print-trace"
  optionHelp = return "Print tracing information (to debug failing tests)"
  optionCLParser = mkFlagCLParser mempty (PrintTrace True)
