-module(cowboy_controller).
-export([routes/0, upgrade/4]).

-include_lib("cowboy/include/http.hrl").

-record(state, {
         req :: #http_req{},
         mode = production :: development | production,
         app :: atom(),
         priv :: string(),
         action :: atom(),
         handler :: atom(),
         handler_state :: any()
       }).

routes() ->
   [{[<<"error">>,'...'], cowboy_http_static, 
                          [{directory, filename:join([code:lib_dir(cowboy_controller, priv), "static"])},
                           {mimetypes, {fun(Path, _) -> mimetypes:filename(Path) end, []}},
                           {etag, {attributes, [filepath, filesize, inode, mtime]}}]}].

-spec upgrade(pid(), module(), any(), #http_req{})-> {ok, #http_req{}} | close.
upgrade(_ListenerPid, Handler, Opts, Req) ->
    App = proplists:get_value(app, Opts, application:get_application()),
    Mode = proplists:get_value(mode, Opts, production),
    Priv = proplists:get_value(priv, Opts, code:lib_dir(App, priv)),
    CtrlPriv = code:lib_dir(cowboy_controller, priv),
    Action = proplists:get_value(action, Opts),
    try
        case erlang:function_exported(Handler, ctrl_init, 2) of
            true ->
                case Handler:ctrl_init(Req, Opts) of
                    {ok, Req2, HandlerState} ->
                        State0 = #state{ handler_state = HandlerState }
                end;
            false ->
                Req2 = Req,
                State0 = #state{}
        end,
        State = State0#state{ mode = Mode, 
                              app = App, 
                              priv = Priv,
                              req = Req2, 
                              handler = Handler,
                              action = Action
                            },
        call_handler(Handler, State)
    catch Class:Reason ->
        PLReq = lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))),
        Stacktrace = erlang:get_stacktrace(),
        error_logger:error_msg(
            "** Handler ~p terminating in ctrl_init/2~n"
            "   for the reason ~p:~p~n** Options were ~p~n"
            "** Request was ~p~n** Stacktrace: ~p~n~n",
            [Handler, Class, Reason, Opts, PLReq, Stacktrace]),
        State1 = #state{ req = Req, app = App, priv = Priv, mode = Mode },
        case Mode of
            development ->
                do_render({render_template({views_filename("_exception_error.html", CtrlPriv),
                                            [{controller, Handler},{action, Action},
                                             {error_class, io_lib:format("~p",[Class])},
                                             {reason, error_logger_lager_h:format_reason({Reason,Stacktrace})},
                                             {stacktrace, Stacktrace},
                                             {stacktrace_printed, io_lib:format("~p",[Stacktrace])},
                                             {request, Req}]}, State1), [{code, 500}]}, State1#state.req);
            production ->
                do_render({render_template({views_filename("_public_error.html", CtrlPriv), []}, State1), [{code, 500}]}, State1#state.req)
        end,
        close
    end.


%% Private

call_handler(Handler, #state{ req = Req, action = Action, handler_state = HandlerState } = State) ->
    handle_return(Handler:Action(Req, HandlerState), State).

handle_return({ok, Req, HandlerState}, #state{} = State) ->
    {ok, Req, State#state{ handler_state = HandlerState }};

handle_return({render, Render, Req, HandlerState}, #state{} = State) ->
    Req2 = do_render(Render, State#state{ req = Req }),
    {ok, Req2, State#state{ req = Req2, handler_state = HandlerState }}.

do_render({template, Template}, State) ->
   do_render({{template,Template},[{code, 200}]}, State);
do_render({{template, Template}, Opts}, #state{ req = Req } = State) ->
   do_render({render_template(Template, State), Opts}, Req);
do_render(Bin, Req) when is_binary(Bin) ->
   do_render({Bin, [{code, 200}]}, Req);
do_render({Bin, Opts}, Req) when is_binary(Bin) ->
   cowboy_http_req:reply(proplists:get_value(code, Opts, 200), proplists:get_value(headers, Opts, []), Bin, Req).

render_template({Template, Variables}, #state{ mode = Mode, priv = Priv, 
                                               app = App, action = Action,
                                               handler = Handler } = State) when is_list(Template) ->
   Hash = erlang:phash2({App, Template}),
   TemplateModule = list_to_atom("cowboy_controller_template_" ++ integer_to_list(Hash)),
   TemplateLoaded = code:is_loaded(TemplateModule),
   if 
      ((Mode == development) orelse not TemplateLoaded) ->
       CompileResult = erlydtl:compile(views_filename(Template, Priv), TemplateModule, 
                                       [{custom_filters_modules, [cowboy_controller_filters]},
                                        {custom_tags_modules, [cowboy_controller_tags]},
                                        {custom_tags_dir, helpers_filename(Priv)}]),
           code:load_file(TemplateModule);
       true ->
           CompileResult = ok
   end,
   CtrlPriv = code:lib_dir(cowboy_controller, priv),   
   case {Mode, CompileResult} of
        {_, ok} ->
           {ok, Rendered} = TemplateModule:render([{erlang_application, App},{action, Action},{controller, Handler}|Variables]),
           iolist_to_binary(Rendered);
       {development, {error, {_File, [{Line, erlydtl_scanner, Reason}]}}} ->
           render_template({views_filename("_template_error.html", CtrlPriv), 
                            [{template, Template}, {variables, Variables},
                             {error_line, Line}, {error, Reason}]}, State);
       {production, _} ->
           render_template({views_filename("_public_error.html", CtrlPriv), []}, State)
   end;
render_template(Template, State) ->
   render_template({Template, []}, State).

views_filename(Path, Priv) ->
    filename:join([Priv,"views",Path]).

helpers_filename(Priv) ->
    filename:join([Priv,"helpers"]).    
