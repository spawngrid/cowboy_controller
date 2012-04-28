-module(cowboy_controller_routes).
-export([with/2]).
-export([default/0, error/0]).

default() ->
    error().

error() ->
    [{[<<"error">>,'...'], cowboy_http_static, 
      [{directory, 
        filename:join([code:lib_dir(cowboy_controller, priv), 
                       "static"])},
       {mimetypes, {fun(Path, _) -> mimetypes:filename(Path) end, []}},
       {etag, {attributes, [filepath, filesize, inode, mtime]}}]}].


with(ExtraOpts, Routes) ->
    [ {Path, Handler, lists:concat([ExtraOpts, Opts])} 
      || {Path, Handler, Opts} <- Routes ].
