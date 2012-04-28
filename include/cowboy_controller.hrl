-define(CONTROLLER,
        init({Proto, http}, _Req, _Opts) when Proto == tcp orelse Proto == ssl ->
           {upgrade, protocol, cowboy_controller}).
