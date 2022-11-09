module Shoronpo where

import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Record.Format as RF
import Type.Prelude (Proxy(..))

formatSymbol
  :: forall string flist row out proxyOrRec
   . RF.Parse string flist
  => FormatSymbolParsed flist row "" out
  => Proxy string
  -> proxyOrRec row
  -> Proxy out
formatSymbol _ _ = Proxy

class FormatSymbolParsed (flist :: RF.FList) (row :: Row Type) (acc :: Symbol) (out :: Symbol)
  | flist -> row acc out

instance nilFormatSymbolParsed :: FormatSymbolParsed RF.FNil row out out

instance consVarFormatSymbolParsed ::
  ( Symbol.Append acc sym acc'
  , Row.Cons var (Proxy sym) row' row
  , FormatSymbolParsed tail row acc' out
  ) => FormatSymbolParsed (RF.FCons (RF.Var var) tail) row acc out

instance consLitFormatSymbolParsed ::
  ( Symbol.Append acc lit acc'
  , FormatSymbolParsed tail row acc' out
  ) => FormatSymbolParsed (RF.FCons (RF.Lit lit) tail) row acc out

intercalateRowLabels
  :: forall row x out proxyOrRecord
   . IntercalateRowLabels row x out
  => proxyOrRecord row
  -> Proxy x
  -> Proxy out
intercalateRowLabels _ _ = Proxy

intercalateRecordLabels
  :: forall row x out
   . IntercalateRowLabels row x out
  => Proxy { | row }
  -> Proxy x
  -> Proxy out
intercalateRecordLabels _ _ = Proxy

class IntercalateRowLabels (row :: Row Type) (x :: Symbol) (out :: Symbol)

instance intercalateRowLabelsInstance ::
  ( RL.RowToList row rl
  , IntercalateRowLabelsImpl rl x "" out
  ) => IntercalateRowLabels row x out

class IntercalateRowLabelsImpl (rl :: RL.RowList Type) (x :: Symbol) (acc :: Symbol) (out :: Symbol)
  | rl -> x out

instance nilIntercalateRowLabelsImpl :: IntercalateRowLabelsImpl RL.Nil x out out

instance consNilIntercalateRowLabelsImpl ::
  ( Symbol.Append acc name acc'
  ) => IntercalateRowLabelsImpl (RL.Cons name ty RL.Nil) x acc acc'

else instance consIntercalateRowLabelsImpl ::
  ( Symbol.Append name x s
  , Symbol.Append acc s acc'
  , IntercalateRowLabelsImpl tail x acc' out
  ) => IntercalateRowLabelsImpl (RL.Cons name ty tail) x acc out

intercalateRowValues
  :: forall row x out proxyOrRecord
   . IntercalateRowValues row x out
  => proxyOrRecord row
  -> Proxy x
  -> Proxy out
intercalateRowValues _ _ = Proxy

intercalateRecordValues
  :: forall row x out
   . IntercalateRowValues row x out
  => Proxy { | row }
  -> Proxy x
  -> Proxy out
intercalateRecordValues _ _ = Proxy

class IntercalateRowValues (row :: Row Type) (x :: Symbol) (out :: Symbol)

instance intercalateRowValuesInstance ::
  ( RL.RowToList row rl
  , IntercalateRowValuesImpl rl x "" out
  ) => IntercalateRowValues row x out

class IntercalateRowValuesImpl (rl :: RL.RowList Type) (x :: Symbol) (acc :: Symbol) (out :: Symbol)
  | rl -> x out

instance nilIntercalateRowValuesImpl :: IntercalateRowValuesImpl RL.Nil x out out

instance consNilIntercalateRowValuesImpl ::
  ( Symbol.Append acc value acc'
  ) => IntercalateRowValuesImpl (RL.Cons name (Proxy value) RL.Nil) x acc acc'

else instance consIntercalateRowValuesImpl ::
  ( Symbol.Append value x s
  , Symbol.Append acc s acc'
  , IntercalateRowValuesImpl tail x acc' out
  ) => IntercalateRowValuesImpl (RL.Cons name (Proxy value) tail) x acc out
