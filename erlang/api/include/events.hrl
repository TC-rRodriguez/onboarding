%%%-------------------------------------------------------------------
%%% @author alejandrorodriguez
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2024 2:50 AM
%%%-------------------------------------------------------------------
-author("alejandrorodriguez").

-record(event_data, {
  first_name    :: string(),
  last_name     :: string(),
  gender        :: binary(),
  mrn           :: string(),
  organization  :: string()
}).

-record(event, {
  event_type  :: binary(),
  data        :: #event_data{}
}).

