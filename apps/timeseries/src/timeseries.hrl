%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------

-ifndef(TIMESERIES_PRIVATE_HRL).
-define(TIMESERIES_PRIVATE_HRL, true).

-include_lib("kernel/include/logger.hrl").

-define(APPLICATION, timeseries).

-define(WS_OPTIONS, #{idle_timeout => 3600000}).
-endif. %-ifndef(TIMESERIES_PRIVATE_HRL).
