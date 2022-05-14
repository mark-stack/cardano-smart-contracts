-- Single text datum/redeemer - User Redeems All Funds
-- For development services, please contact mark@cardanocrowd.com

-- 1. Imports
import  Plutus.Contract
import  Control.Monad  hiding (fmap)
import  Data.ByteString.Char8 qualified as C
import  Data.Map  as Map
import  Data.Maybe (catMaybes)
import  Data.Text qualified as T
import  Data.Text (Text)
import  Data.Void  (Void)
import  PlutusTx   (Data (..))
import  qualified  PlutusTx
import  qualified  PlutusTx.Builtins   as  Builtins
import  PlutusTx.Prelude  hiding (Semigroup(..), unless)
import  Ledger   hiding (singleton)
import  Ledger.Constraints  as Constraints
import  Ledger.Ada   as Ada
import  Ledger (Ada, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (ScriptContext, scriptContextTxInfo),valuePaidTo)
import  qualified Ledger.Scripts  as Scripts
import  Ledger.Typed.Scripts qualified as TypedScripts
--import  Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import  Playground.Contract
import  Playground.TH    (mkKnownCurrencies, mkSchemaDefinitions)
import  Playground.Types   (KnownCurrency (..))
import  Prelude   (IO, Semigroup (..), String)
import  Prelude qualified as Haskell
import  Text.Printf  (printf)
import  GHC.Generics (Generic)
import  Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)
import  Data.Aeson (FromJSON, ToJSON)
import  Schema (ToSchema)

-- 2. Declare variables

-- 3. Data type declarations
newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''ClearString

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { unique_identifier :: Haskell.String
    , amount  :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- 4. Validators & other functions
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

clearString :: Haskell.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

contractValidator :: Validator
contractValidator = TypedScripts.validatorScript contractInstance

contractAddress :: Address
contractAddress = Ledger.scriptAddress contractValidator

findIdentifierWordValue :: Map TxOutRef ChainIndexTxOut -> Maybe HashedString
findIdentifierWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map identifierWordValue

identifierWordValue :: ChainIndexTxOut -> Maybe HashedString
identifierWordValue o = do
  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
  PlutusTx.fromBuiltinData d

-- 5. Validator type declarations
data MyContract
instance TypedScripts.ValidatorTypes MyContract where
    type instance RedeemerType MyContract = ClearString
    type instance DatumType MyContract = HashedString

-- 6. Compile the validator
contractInstance :: TypedScripts.TypedValidator MyContract
contractInstance = TypedScripts.mkTypedValidator @MyContract
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = TypedScripts.wrapValidator @HashedString @ClearString

-- 7. Schema endpoints
type ContractSchema =
        Endpoint "Lock" LockParams
        .\/ Endpoint "Withdrawal" GuessParams

-- 8. Endpoints logic
lock :: AsContractError e => Promise () ContractSchema e ()
lock = endpoint @"Lock" @LockParams $ \(LockParams string amount) -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show amount <> " to the script"
    let tx  = Constraints.mustPayToTheScript (hashString string) amount
    void (submitTxConstraints contractInstance tx)

withdrawal :: AsContractError e => Promise () ContractSchema e ()
withdrawal = endpoint @"Withdrawal" @GuessParams $ \(GuessParams input1) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"

-- Reference https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract.html#t:Contract

    -- GET UTXOs:
    -- Some options for retrieving UXTOs
    -- myUtxos <- utxosAt contractAddress
    -- myUtxos <- utxoRefsAt contractAddress

    -- 'fundsAtAddressGeq' = Watch an address for changes, and return the outputs at that address when the total value at the address has reached or surpassed the given value.
    myUtxos <- fundsAtAddressGeq contractAddress (Ada.lovelaceValueOf 1)
  
    -- Redeemer
    let redeemer = clearString input1

    -- build transaction
        tx = collectFromScript myUtxos redeemer 

    -- Log a message saying if the Identifier word was correctly guessed
    let hashedIdentifierWord = findIdentifierWordValue myUtxos
        isCorrectIdentifierWord = fmap (`isGoodGuess` redeemer) hashedIdentifierWord == Just True
    if isCorrectIdentifierWord
        then logWarn @Haskell.String "Correct Identifier word! Submitting the transaction"
        else logWarn @Haskell.String "Incorrect Identifier word, but still submiting the transaction"

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the guess is
    -- wrong.
    logInfo @Haskell.String "Submitting transaction to guess the Identifier word"
    void (submitTxConstraintsSpending contractInstance myUtxos tx)

-- 9. Endpoints
endpoints :: AsContractError e => Contract () ContractSchema e ()
endpoints = do
    logInfo @Haskell.String "Waiting for Lock or Withdrawal selection..."
    selectList [lock, withdrawal]

-- 10. Schema Definitions
mkSchemaDefinitions ''ContractSchema

-- 11. mkKnownCurrencies
mkKnownCurrencies []