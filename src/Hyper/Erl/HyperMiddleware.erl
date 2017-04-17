-module(hyper_erl_hyperMiddleware@foreign).
-export([fromFn/2]).

fromFn(F,Def) -> case F of
  {fn, Fn} -> Fn;
  _ -> Def
end.
