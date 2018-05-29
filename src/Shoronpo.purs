module Shoronpo where

import Prim.Row as Row
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Record.Format as RF
import Type.Prelude (Proxy, RProxy, SProxy(..))

formatSymbol
  :: forall string flist row out proxyOrRec
   . RF.Parse string flist
  => FormatSymbolParsed flist row "" out
  => SProxy string
  -> proxyOrRec row
  -> SProxy out
formatSymbol _ _ = SProxy

class FormatSymbolParsed (flist :: RF.FList) (row :: # Type) (acc :: Symbol) (out :: Symbol)
  | flist -> row acc out

instance nilFormatSymbolParsed :: FormatSymbolParsed RF.FNil row out out

instance consVarFormatSymbolParsed ::
  ( Symbol.Append acc sym acc'
  , Row.Cons var (SProxy sym) row' row
  , FormatSymbolParsed tail row acc' out
  ) => FormatSymbolParsed (RF.FCons (RF.Var var) tail) row acc out

instance consLitFormatSymbolParsed ::
  ( Symbol.Append acc lit acc'
  , FormatSymbolParsed tail row acc' out
  ) => FormatSymbolParsed (RF.FCons (RF.Lit lit) tail) row acc out

intercalateRowLabels
  :: forall row x out
   . IntercalateRowLabels row x out
  => RProxy row
  -> SProxy x
  -> SProxy out
intercalateRowLabels _ _ = SProxy

intercalateRecordLabels
  :: forall row x out
   . IntercalateRowLabels row x out
  => Proxy { | row }
  -> SProxy x
  -> SProxy out
intercalateRecordLabels _ _ = SProxy

class IntercalateRowLabels (row :: # Type) (x :: Symbol) (out :: Symbol)

instance intercalateRowLabelsInstance ::
  ( RL.RowToList row rl
  , IntercalateRowLabelsImpl rl x "" out
  ) => IntercalateRowLabels row x out

class IntercalateRowLabelsImpl (rl :: RL.RowList) (x :: Symbol) (acc :: Symbol) (out :: Symbol)
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
