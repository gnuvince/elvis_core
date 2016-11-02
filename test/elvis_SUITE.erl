-module(elvis_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         %% Rocking
         rock_with_empty_map_config/1,
         rock_with_empty_list_config/1,
         rock_with_incomplete_config/1,
         rock_with_list_config/1,
         rock_with_file_config/1,
         rock_with_old_config/1,
         rock_with_rebar_default_config/1,
         rock_this/1,
         rock_without_colors/1,
         rock_with_rule_groups/1,
         %% Util & Config
         throw_configuration/1,
         find_file_and_check_src/1,
         find_file_with_ignore/1,
         invalid_file/1,
         to_string/1,
         gitignore_wildmatch/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = elvis_SUITE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(elvis),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    ok = application:stop(elvis),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Rocking

-spec rock_with_empty_map_config(config()) -> any().
rock_with_empty_map_config(_Config) ->
    ok = try
             ok = elvis_core:rock([#{}]),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end,
    ok = try
             ok = elvis_core:rock([#{} || X <- lists:seq(1,10), X < 1]),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_empty_list_config(config()) -> any().
rock_with_empty_list_config(_Config) ->
    ok = try
             ok = elvis_core:rock([#{}, #{}]),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_incomplete_config(config()) -> any().
rock_with_incomplete_config(_Config) ->
    ElvisConfig = [#{src_dirs => ["src"]}],
    ok = try
             ok = elvis_core:rock(ElvisConfig),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_list_config(config()) -> any().
rock_with_list_config(_Config) ->
    ElvisConfig = [#{src_dirs => ["src"],
                     rules => []},
                   #{dirs => ["."],
                     filter => "Makefile",
                     rules => []}],
    ok = try
             ok = elvis_core:rock(ElvisConfig)
         catch
             throw:{invalid_config, _} -> fail
         end.

-spec rock_with_file_config(config()) -> ok.
rock_with_file_config(_Config) ->
    Fun = fun() -> elvis_core:rock() end,
    Expected = "# \\.\\./\\.\\./_build/test/lib/elvis_core/test/" ++
               "examples/.*\\.erl.*FAIL",
    [_ | _] = check_some_line_output(Fun, Expected, fun matches_regex/2),
    ok.

-spec rock_with_old_config(config()) -> ok.
rock_with_old_config(_Config) ->
    ConfigPath = "../../config/old/elvis.config",
    ElvisConfig = elvis_config:load_file(ConfigPath),
    ok = try
             ok = elvis_core:rock(ElvisConfig)
         catch
             throw:{invalid_config, _} -> fail
         end,

    ConfigPath1 = "../../config/old/elvis-test.config",
    ElvisConfig1 = elvis_config:load_file(ConfigPath1),
    ok = try
             ok = elvis_core:rock(ElvisConfig1)
         catch
             throw:{invalid_config, _} -> fail
         end,

    ConfigPath2 = "../../config/old/elvis-test-rule-config-list.config",
    ElvisConfig2 = elvis_config:load_file(ConfigPath2),
    ok = try
             ok = elvis_core:rock(ElvisConfig2)
         catch
             throw:{invalid_config, _} -> fail
         end.

-spec rock_with_rebar_default_config(config()) -> ok.
rock_with_rebar_default_config(_Config) ->
    {ok, _} = file:copy("../../config/rebar.config", "rebar.config"),
    [#{name := line_length}] = try
        {fail, Results} = elvis_core:rock(),
        [Rule || #{rules := [Rule]} <- Results]
    after
        file:delete("rebar.config")
    end,
    ok.

-spec rock_this(config()) -> ok.
rock_this(_Config) ->
    ok = elvis_core:rock_this(elvis_core),

    ok = try
             {fail, _} = elvis_core:rock_this("bla.erl")
         catch
             _:{enoent, "bla.erl"} -> ok
         end,

    Path =
        "../../_build/test/lib/elvis_core/test/examples/fail_line_length.erl",
    {fail, _} = elvis_core:rock_this(Path),

    ok.

-spec rock_without_colors(config()) -> ok.
rock_without_colors(_Config) ->
    ConfigPath = "../../config/test.config",
    ElvisConfig = elvis_config:load_file(ConfigPath),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected = "\\e.*?m",
    ok = try check_some_line_output(Fun, Expected, fun matches_regex/2) of
             Result -> ct:fail("Unexpected result ~p", [Result])
         catch
             _:{badmatch, []} -> ok
         end.

-spec rock_with_rule_groups(Config::config()) -> ok.
rock_with_rule_groups(_Config) ->
    % elvis_config will load default elvis_core rules for every
    % rule_group in the config.
    RulesGroupConfig =
        [#{dirs => ["src"], filter => "*.erl", ruleset => erl_files},
         #{dirs => ["."], filter => "Makefile", ruleset => makefiles},
         #{dirs => ["."], filter => "rebar.config", ruleset => rebar_config},
         #{dirs => ["."], filter => "elvis.config", ruleset => elvis_config}],
    ok = try
             ok = elvis_core:rock(RulesGroupConfig)
         catch
             throw:{invalid_config, _} -> fail
         end,
    % Override default elvis_core rules without ruleset should fail.
    OverrideFailConfig =
        [#{dirs => ["src"],
           rules => [{elvis_style, line_length, #{limit => 90}},
                     {elvis_style, state_record_and_type, disable}]}],
    ok = try
           _ = elvis_core:rock(OverrideFailConfig),
           fail
       catch
           throw:{invalid_config, _} -> ok
       end,
    % Override default elvis_core rules.
    OverrideConfig =
        [#{dirs => ["src"],
           filter => "*.erl",
           ruleset => erl_files,
           rules => [{elvis_style, line_length, #{limit => 90}}, % I like 90 chars per line.
                     {elvis_style, no_tabs, disable}]}, % I like tabs so disable this rule.
         #{dirs => ["."],
           filter => "Makefile",
           ruleset => makefiles,
           rules => [{elvis_project, no_deps_master_erlang_mk, disable}]}, % I like stable dependencies
         #{dirs => ["."], filter => "rebar.config", ruleset => rebar_config},
         #{dirs => ["."], filter => "elvis.config", ruleset => elvis_config}],
    ok = try
           ok = elvis_core:rock(OverrideConfig)
       catch
           throw:{invalid_config, _} -> fail
       end.

%%%%%%%%%%%%%%%
%%% Utils

-spec throw_configuration(config()) -> any().
throw_configuration(_Config) ->
    Filename = "./elvis.config",
    ok = file:write_file(Filename, <<"-">>),
    ok = try
             _ = elvis_config:default(),
             fail
         catch
             throw:_ -> ok
         after
             file:delete(Filename)
         end.

-spec find_file_and_check_src(config()) -> any().
find_file_and_check_src(_Config) ->
    Dirs = ["../../test/examples"],

    [] = elvis_file:find_files(Dirs, "doesnt_exist.erl"),
    [File] = elvis_file:find_files(Dirs, "small.erl"),

    {<<"-module(small).\n">>, _} = elvis_file:src(File),
    {error, enoent} = elvis_file:src(#{path => "doesnt_exist.erl"}).


-spec find_file_with_ignore(config()) -> any().
find_file_with_ignore(_Config) ->
    Dirs = ["../../test/examples"],
    Filter = "find_test*.erl",
    Ignore = elvis_config:ignore(#{ignore => [find_test1]}),
    Files = [_, _] = elvis_file:find_files(Dirs, Filter),
    [_, _] = elvis_file:filter_files(Files, Dirs, Filter, []),
    [#{path := "../../test/examples/find_test2.erl"}] =
        elvis_file:filter_files(Files, Dirs, Filter, Ignore).

-spec invalid_file(config()) -> any().
invalid_file(_Config) ->
    ok = try
             {error, _} = elvis_file:src(#{}),
             fail
         catch
             throw:{invalid_file, #{}} -> ok
         end.

-spec to_string(config()) -> any().
to_string(_Config) ->
    "1" = elvis_utils:to_str(1),
    "hello" = elvis_utils:to_str(<<"hello">>),
    "atom" = elvis_utils:to_str(atom).


-spec gitignore_wildmatch(config()) -> any().
gitignore_wildmatch(_Config) ->
    TestCases = [
                   %% basic patterns
                   {true , "ab", "ab"}
                 , {false, "ab", "Ab"}
                 , {false, "ab", "cab"}
                 , {false, "ab", "abe"}
                 , {false, "ab", "a"}
                 , {false, "ab", "b"}
                 , {false, "ab", ""}

                 %% escapes
                 , {true , "\\?\\*\\\\", "?*\\"}
                 , {true , "\\?\\*\\*", "?**"}

                 %% question mark wildcard
                 , {true , "?", "a"}
                 , {true , "?", "1"}
                 , {true , "?", "."}
                 , {true , "a?c", "abc"}
                 , {true , "a?c", "aBc"}
                 , {true , "a?c", "a1c"}
                 , {true , "a?c", "a.c"}
                 , {false, "a?b", "a/b"}

                 %% single star wildcard
                 , {true , "*", ""}
                 , {true , "*", "a"}
                 , {true , "*", "ab"}
                 , {true , "*", "abc"}
                 , {true , "*.txt", ".txt"}
                 , {true , "*.txt", "a.txt"}
                 , {true , "*.txt", "ab.txt"}
                 , {false, "*.txt", "a/b.txt"}

                 %% double star wildcard
                 , {true , "a/**/b", "a/b"}
                 , {true , "a/**/b", "a//b"}
                 , {true , "a/**/b", "a/x/b"}
                 , {true , "a/**/b", "a/x/y/b"}
                 , {true , "a/**/b", "a//x/y/b"}
                 , {true , "a/**/b", "a/x//y/b"}

                 %% mix and match
                , {true , "**/*/?.txt", "foo/bar/1.txt"}
                , {true , "**/*/?.txt", "foo/bar/baz/1.txt"}
                ],
    %% Adapted from git's t/t3070-wildmatch.sh
    GitTestCases = [
                    %% Basic wildmat features
                     {true,  "foo", "foo"}
                   , {false, "bar", "foo"}
                   , {true,  "", ""}
                   , {true,  "???", "foo"}
                   , {false, "??", "foo"}
                   , {true,  "*", "foo"}
                   , {true,  "f*", "foo"}
                   , {false, "*f", "foo"}
                   , {true,  "*foo*", "foo"}
                   , {true,  "*ob*a*r*", "foobar"}
                   , {true,  "*ab", "aaaaaaabababab"}
                   , {true,  "foo\\*", "foo*"}
                   , {false, "foo\\*bar", "foobar"}
                   , {true,  "f\\\\oo", "f\\oo"}
                   , {true,  "*[al]?", "ball"}
                   , {false, "[ten]", "ten"}
                   , {false, "**[!te]", "ten"}
                   , {false, "**[!ten]", "ten"}
                   , {true,  "t[a-g]n", "ten"}
                   , {false, "t[!a-g]n", "ten"}
                   , {true,  "t[!a-g]n", "ton"}
                   , {true,  "t[^a-g]n", "ton"}
                   , {true,  "a[]]b", "a]b"}
                   , {true,  "a[]-]b", "a-b"}
                   , {true,  "a[]-]b", "a]b"}
                   , {false, "a[]-]b", "aab"}
                   , {true,  "a[]a-]b", "aab"}
                   , {true,  "]", "]"}

                    %% Extended slash-matching features
                   , {false, "foo*bar", "foo/baz/bar"}
                   , {false, "foo**bar", "foo/baz/bar"}
                   , {false, "foo**bar", "foobazbar"}
                   , {true,  "foo/**/bar", "foo/baz/bar"}
                   , {true,  "foo/**/**/bar", "foo/baz/bar"}
                   , {true,  "foo/**/bar", "foo/b/a/z/bar"}
                   , {true,  "foo/**/**/bar", "foo/b/a/z/bar"}
                   , {true,  "foo/**/bar", "foo/bar"}
                   , {true,  "foo/**/**/bar", "foo/bar"}
                   , {false, "foo?bar", "foo/bar"}
                   , {false, "foo[/]bar", "foo/bar"}
                   , {false, "f[^eiu][^eiu][^eiu][^eiu][^eiu]r", "foo/bar"}
                   , {true,  "f[^eiu][^eiu][^eiu][^eiu][^eiu]r", "foo-bar"}
                   , {true,  "**/foo", "foo"}
                   , {true,  "**/foo", "/foo"}
                   , {true,  "**/foo", "bar/baz/foo"}
                   , {false, "*/foo", "bar/baz/foo"}
                   , {false, "**/bar*", "foo/bar/baz"}
                   , {true,  "**/bar/*", "deep/foo/bar/baz"}
                   , {false, "**/bar/*", "deep/foo/bar/baz/"}
                   , {true,  "**/bar/**", "deep/foo/bar/baz/"}
                   , {false, "**/bar/*", "deep/foo/bar"}
                   , {true,  "**/bar/**", "deep/foo/bar/"}
                   , {false, "**/bar**", "foo/bar/baz"}
                   , {true,  "*/bar/**", "foo/bar/baz/x"}
                   , {false, "*/bar/**", "deep/foo/bar/baz/x"}
                   , {true,  "**/bar/*/*", "deep/foo/bar/baz/x"}

                   , {false, "a[c-c]st", "acrt"}
                   , {true,  "a[c-c]rt", "acrt"}
                   , {false, "[!]-]", "]"}
                   , {true,  "[!]-]", "a"}
                   , {false, "\\", ""}
                   , {false, "\\", "\\"}
                   , {false, "*/\\", "/\\"}
                   , {true,  "*/\\\\", "/\\"}
                   , {true,  "foo", "foo"}
                   , {true,  "@foo", "@foo"}
                   , {false, "@foo", "foo"}
                   , {true,  "\\[ab]", "[ab]"}
                   , {true,  "[[]ab]", "[ab]"}
                   , {true,  "[[:]ab]", "[ab]"}
                   %%, {false, "[[::]ab]", "[ab]"}
                   , {true,  "[[:digit]ab]", "[ab]"}
                   , {true,  "[\\[:]ab]", "[ab]"}
                   , {true,  "\\??\\?b", "?a?b"}
                   , {true,  "\\a\\b\\c", "abc"}
                   , {false, "", "foo"}
                   , {true,  "**/t[o]", "foo/bar/baz/to"}

                   , {true,  "[\\\\-^]", "]"}
                   , {false, "[\\\\-^]", "["}
                   , {true,  "[\\-_]", "-"}
                   , {true,  "[\\]]", "]"}
                   , {false, "[\\]]", "\\]"}
                   , {false, "[\\]]", "\\"}
                   , {false, "a[]b", "ab"}
                   , {false, "a[]b", "a[]b"}
                   , {false, "ab[", "ab["}
                   , {false, "[!", "ab"}
                   , {false, "[-", "ab"}
                   , {true,  "[-]", "-"}
                   , {false, "[a-", "-"}
                   , {false, "[!a-", "-"}
                   , {true,  "[--A]", "-"}
                   , {true,  "[--A]", "5"}
                   , {true,  "[ --]", " "}
                   , {true,  "[ --]", "$"}
                   , {true,  "[ --]", "-"}
                   , {false, "[ --]", "0"}
                   , {true,  "[---]", "-"}
                   , {true,  "[------]", "-"}
                   , {false, "[a-e-n]", "j"}
                   , {true,  "[a-e-n]", "-"}
                   , {true,  "[!------]", "a"}
                   , {false, "[]-a]", "["}
                   , {true,  "[]-a]", "^"}
                   , {false, "[!]-a]", "^"}
                   , {true,  "[!]-a]", "["}
                   , {true,  "[a^bc]", "^"}
                   , {true,  "[a-]b]", "-b]"}
                   , {false, "[\\]", "\\"}
                   , {true,  "[\\\\]", "\\"}
                   , {false, "[!\\\\]", "\\"}
                   , {true,  "[A-\\\\]", "G"}
                   , {false, "b*a", "aaabbb"}
                   , {false, "*ba*", "aabcaa"}
                   , {true,  "[,]", ","}
                   , {true,  "[\\\\,]", ","}
                   , {true,  "[\\\\,]", "\\"}
                   , {true,  "[,-.]", "-"}
                   , {false, "[,-.]", "+"}
                   , {false, "[,-.]", "-.]"}
                   , {true,  "[\\1-\\3]", "2"}
                   , {true,  "[\\1-\\3]", "3"}
                   , {false, "[\\1-\\3]", "4"}
                   , {true,  "[[-\\]]", "\\"}
                   , {true,  "[[-\\]]", "["}
                   , {true,  "[[-\\]]", "]"}
                   , {false, "[[-\\]]", "-"}
                   ],
    TryMatch = fun(F, R, Pat, Text) ->
                       try
                           R = F(Pat, Text)
                       catch
                           _:_ -> throw({wildmatch_failure, F, R, Pat, Text})
                       end
               end,
    [TryMatch(fun elvis_gitignore:wildmatch/2, R, Pat, Text) || {R, Pat, Text} <- TestCases],
    [TryMatch(fun elvis_gitignore:wildmatch/2, R, Pat, Text) || {R, Pat, Text} <- GitTestCases].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_some_line_output(Fun, Expected, FilterFun) ->
    _ = ct:capture_start(),
    _ = Fun(),
    _ = ct:capture_stop(),
    Lines = ct:capture_get([]),
    ListFun = fun(Line) -> FilterFun(Line, Expected) end,
    [_ | _] = lists:filter(ListFun, Lines).

matches_regex(Result, Regex) ->
    case re:run(Result, Regex) of
        {match, _} -> true;
        nomatch -> false
    end.
