module Application.Helper.Money
    ( Money
    , moneyFromDouble
    , moneyFromCents
    , moneyToCents
    , giveMoney
    , takeMoney
    , negateMoney
    , formatMoney
    , walletAmount
    , setWalletAmount
    , modifyWalletAmount
    , transactionAmount
    , setTransactionAmount
    , holdingCost
    , setHoldingCost
    , modifyHoldingCost
    ) where

import Generated.Types
import IHP.Prelude
import Web.Types.Money

-- Wallet accessors
walletAmount :: Wallet -> Money
walletAmount wallet = moneyFromCents (get #amountCents wallet)

setWalletAmount :: Money -> Wallet -> Wallet
setWalletAmount money wallet =
    wallet |> set #amountCents (moneyToCents money)

modifyWalletAmount :: Money -> Wallet -> Wallet
modifyWalletAmount delta wallet =
    setWalletAmount (giveMoney (walletAmount wallet) delta) wallet

-- Transaction accessors
transactionAmount :: Transaction -> Money
transactionAmount txn = moneyFromCents (get #amountCents txn)

setTransactionAmount :: Money -> Transaction -> Transaction
setTransactionAmount money txn =
    txn |> set #amountCents (moneyToCents money)

-- Holding accessors
holdingCost :: Holding -> Money
holdingCost holding = moneyFromCents (get #amountCents holding)

setHoldingCost :: Money -> Holding -> Holding
setHoldingCost money holding =
    holding |> set #amountCents (moneyToCents money)

modifyHoldingCost :: Money -> Holding -> Holding
modifyHoldingCost delta holding =
    setHoldingCost (giveMoney (holdingCost holding) delta) holding
