-module(file_signatures_tests).
-include_lib("eunit/include/eunit.hrl").

file_signatures_tests_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        case file:list_dir("test/files") of
          {ok, Files} ->
            [assert_extention_match(filename:join("test/files", File)) || File <- Files];
          _ ->
            ok
        end
    end,
    fun() ->
        ?assertEqual(ok, file_signatures:is_type(<<"test/files/gif.gif">>, [zip, png, gif])),
        ?assertEqual({error, invalid_signature},
                     file_signatures:is_type(<<"test/files/gif.gif">>, [titi, tata, toto])),
        ?assertEqual({error, invalid_signature},
                     file_signatures:is_type(<<"test/files/gif.gif">>, [bz2, zip, z]))
    end
   ]}.

assert_extention_match(File) ->
  [$.|Ext] = filename:extension(File),
  ?debugFmt("~s", [File]),
  ?assertEqual(ok, file_signatures:is_type(File, list_to_atom(Ext))).
