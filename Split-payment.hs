-- Split payment between 2 wallet addresses
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
import  Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash, knownWallet)
import  Data.Aeson (FromJSON, ToJSON)
import  Schema (ToSchema)

-- 2. Declare variables

-- 3. Data type declarations
data MySplitData =
    MySplitData
        { myRecipient1PubKeyHash :: PaymentPubKeyHash -- First recipient of the funds
        , myRecipient2PubKeyHash :: PaymentPubKeyHash -- Second recipient of the funds
        , myAmount     :: Ada -- ^ How much Ada we want to lock
        }
    deriving stock (Haskell.Show, Generic)

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
PlutusTx.unstableMakeIsData ''MySplitData
PlutusTx.makeLift ''MySplitData

data LockArgs =
        LockArgs
            { myRecipient1Wallet :: Integer
            , myRecipient2Wallet :: Integer
            , myTotalAda :: Ada
            }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

mySplitData :: LockArgs -> MySplitData
mySplitData LockArgs{myRecipient1Wallet, myRecipient2Wallet, myTotalAda} =
    MySplitData
        { myRecipient1PubKeyHash = mockWalletPaymentPubKeyHash (knownWallet myRecipient1Wallet)
        , myRecipient2PubKeyHash = mockWalletPaymentPubKeyHash (knownWallet myRecipient2Wallet)
        , myAmount = myTotalAda
        }

-- 4. Validators & other functions
validateSplit :: MySplitData -> () -> ScriptContext -> Bool
validateSplit MySplitData{myRecipient1PubKeyHash, myRecipient2PubKeyHash, myAmount} _ ScriptContext{scriptContextTxInfo} =
    let half = Ada.divide myAmount 2 in
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash myRecipient1PubKeyHash)) >= half &&
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash myRecipient2PubKeyHash)) >= (myAmount - half)

-- 5. Validator type declarations
data Split
instance TypedScripts.ValidatorTypes Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = MySplitData

-- 6. Compile the validator
splitValidator :: TypedScripts.TypedValidator Split
splitValidator = TypedScripts.mkTypedValidator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = TypedScripts.wrapValidator @MySplitData @()

-- 7. Schema endpoints
type SplitSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs

-- 8. Endpoints logic
lock :: Promise () SplitSchema T.Text ()
lock = endpoint @"lock" (lockFunds . mySplitData)

lockFunds :: MySplitData -> Contract () SplitSchema T.Text ()
lockFunds datum@MySplitData{myAmount} = do

    logInfo $ "XXX myAmount:" <> Haskell.show myAmount

    --Build transaction (requires datum and amount)
    let tx = Constraints.mustPayToTheScript datum (Ada.toValue myAmount)
    
    --Send transaction (requires validator instance and transaction)
    void $ submitTxConstraints myInstance tx

unlock :: Promise () SplitSchema T.Text ()
unlock = endpoint @"unlock" (unlockFunds . mySplitData)

unlockFunds :: MySplitData -> Contract () SplitSchema T.Text ()
unlockFunds MySplitData{myRecipient1PubKeyHash, myRecipient2PubKeyHash, myAmount} = do
    logInfo $ "XXX myRecipient1PubKeyHash:" <> Haskell.show myRecipient1PubKeyHash
    logInfo $ "XXX myRecipient2PubKeyHash:" <> Haskell.show myRecipient2PubKeyHash
    logInfo $ "XXX myAmount:" <> Haskell.show myAmount

    let contractAddress = TypedScripts.validatorAddress myInstance
    logInfo $ "XXX contractAddress:" <> Haskell.show contractAddress   

    utxos <- utxosAt contractAddress
    
    let half = Ada.divide myAmount 2
        tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey myRecipient1PubKeyHash (Ada.toValue half)
            <> Constraints.mustPayToPubKey myRecipient2PubKeyHash (Ada.toValue $ myAmount - half)
    
    logInfo $ "XXX half:" <> Haskell.show half  
    void $ submitTxConstraintsSpending myInstance utxos tx

-- 9. Endpoints
endpoints :: Contract () SplitSchema T.Text ()
endpoints = do
    logInfo @Haskell.String "Waiting for A or B selection..."
    selectList [lock, unlock]

-- 10. Schema Definitions
mkSchemaDefinitions ''SplitSchema

-- 11. mkKnownCurrencies
mkKnownCurrencies []
