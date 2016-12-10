-module(file_signatures).

-export([is_type/2]).

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



