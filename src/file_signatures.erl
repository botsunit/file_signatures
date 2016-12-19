-module(file_signatures).

-export([
         is_type/2,
         is_valid/1,
         signature/1
        ]).

% @doc
% Return <tt>ok</tt> if <tt>Filename</tt> has a <tt>Type</tt> signature.
%
% Example:
% <pre>
% file_signatures:is_type("sample.png", png).
% file_signatures:is_type("sample.png", [gif, jpeg, bmp, png])
% </pre>
% @end
-spec is_type(file:name_all(), atom() | [atom()]) -> ok | {error, term()}.
is_type(Filename, Type) when is_atom(Type) ->
  case file:read_file(Filename) of
    {ok, Binary} ->
      verify_signature(Binary, Type);
    Error ->
      Error
  end;
is_type(_, []) ->
  {error, invalid_signature};
is_type(Filename, [Type|Rest]) ->
  case is_type(Filename, Type) of
    ok ->
      ok;
    {error, Error} when Error =:= invalid_signature;
                        Error =:= unknow_file_signature ->
      is_type(Filename, Rest);
    Error ->
      Error
  end.

% @doc
% Return <tt>ok</tt> if <tt>Filename</tt> is a valid signature according to is extension.
%
% Example:
% <pre>
% file_signatures:is_valid("sample.png").
% </pre>
% @end
-spec is_valid(file:name_all()) -> ok | {error, term()}.
is_valid(Filename) when is_list(Filename) ->
  case filename:extension(Filename) of
    [$.|Ext] ->
      is_type(Filename, erlang:list_to_atom(string:to_lower(Ext)));
    _ ->
      {error, missing_extension}
  end;
is_valid(Filename) when is_binary(Filename) ->
  is_valid(erlang:binary_to_list(Filename)).

signature(Filename) ->
  case file:read_file(Filename) of
    {ok, Binary} ->
      signature(Binary, files_signatures:module_info(exports));
    Error ->
      Error
  end.

% @doc
% Return the first matching signature for <tt>Filename</tt>.
%
% Example:
% <pre>
% file_signatures:signature("sample.png").
% </pre>
% @end
-spec signature(file:name_all()) -> atom() | undefined.
signature(_, []) ->
  undefined;
signature(Data, [{Type, 1}|Rest]) when Type =/= module_info ->
  case verify_signature(Data, Type) of
    ok -> Type;
    _ -> signature(Data, Rest)
  end;
signature(Data, [_|Rest]) ->
  signature(Data, Rest).

% @hidden
verify_signature(Data, Type) ->
  try
    case erlang:apply(files_signatures, Type, [Data]) of
      true ->
        ok;
      _ ->
        {error, invalid_signature}
    end
  catch
    _:_ ->
      {error, unknow_file_signature}
  end.



