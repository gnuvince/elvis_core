-module(elvis_gitignore).

-export([wildmatch/2]).

-ifdef(TEST).
-export([compile/1, tokenize_pattern/1, tokenize_class/1]).
-endif.

-ifdef(debug).
-define(DEBUG(X), io:format("DEBUG: ~p~n", [X])).
-else.
-define(DEBUG(_), ok).
-endif.

-type token() :: char()
               | starstar
               | star
               | qmark
               | {class, iolist()}
               | {nclass, iolist()}.


%% wildmatch implementation that converts a glob to a regular expression.
%% Note: this function should always return a boolean, even if the pattern
%%   is malformed.
-spec wildmatch(string(), string()) -> boolean().
wildmatch(Pattern, Text) ->
    try
        case compile(Pattern) of
            {ok, Re} ->
                case re:run(Text, Re) of
                    {match, _} -> true;
                    nomatch -> false
                end;
            fail -> false
        end
    catch
        throw:malformed_pattern -> false;
        throw:unfinished_class -> false
    end.


-spec compile(string()) -> {ok, iolist()} | fail.
compile(Pattern)         ->
    case compile(tokenize_pattern(Pattern), [], start_of_pattern) of
        {ok, Re} -> {ok, [$^, lists:reverse(Re), $$]};
        {fail, Tokens, PatSoFar, Pos} ->
            ?DEBUG({Tokens, PatSoFar, Pos}),
            fail
    end.


compile([], Acc, _) ->
    {ok, Acc};
compile([starstar, $/ | Rest], Acc, start_of_pattern) ->
    compile(Rest, [".*" | Acc], not_start_of_pattern);
compile([$/, starstar], Acc, _) ->
    {ok, ["/.*" | Acc]};
compile([$/, starstar, $/ | Rest], Acc, Pos) ->
    %% foo/**/bar and foo/**/**/bar match the same
    %% strings, so we drop the redundant slashes
    %% and double asterisks.
    Rest2 = lists:dropwhile(fun(T) ->
                                    T == starstar orelse T == $/
                            end, Rest),
    compile(Rest2, ["(?:/|/.*/)" | Acc], Pos);
compile([star | Rest], Acc, Pos) ->
    compile(Rest, ["[^/]*" | Acc], Pos);
compile([qmark | Rest], Acc, Pos) ->
    compile(Rest, ["[^/]" | Acc], Pos);
compile([{class, Chars} | Rest], Acc, Pos) ->
    ClassRe = case Chars of
                  [] -> [];
                  _ -> [$[, Chars, $]]
              end,
    compile(Rest, [ClassRe | Acc], Pos);
compile([{nclass, Chars} | Rest], Acc, Pos) ->
    ClassRe = case Chars of
                  [] -> [];
                  _ -> [$[, $^, Chars, $]]
              end,
    compile(Rest, [ClassRe | Acc], Pos);
compile([$\\ | Rest], Acc, Pos) ->
    compile(Rest, ["\\\\" | Acc], Pos);
compile([C | Rest], Acc, Pos) when is_integer(C) ->
    case is_regex_special(C) of
        true -> compile(Rest, [[$[, C, $]] | Acc], Pos);
        false -> compile(Rest, [C | Acc], Pos)
    end;
compile(Tokens, PatSoFar, Pos) ->
    {fail, Tokens, PatSoFar, Pos}.

is_regex_special(C) ->
    lists:member(C, "^$[](){}?*+.").


-spec tokenize_pattern(string()) -> [token()].
tokenize_pattern([])              -> [];
tokenize_pattern([$*, $* | Rest]) ->
    Rest2 = lists:dropwhile(fun(C) -> C =:= $* end, Rest),
    [starstar | tokenize_pattern(Rest2)];
tokenize_pattern([$* | Rest])     -> [star | tokenize_pattern(Rest)];
tokenize_pattern([$? | Rest])     -> [qmark | tokenize_pattern(Rest)];
tokenize_pattern(Pat = [$[ | _Rest]) ->
    {Class, Rest2} = tokenize_class(Pat),
    [Class | tokenize_pattern(Rest2)];
tokenize_pattern([$\\])           -> throw(malformed_pattern);
tokenize_pattern([$\\, C | Rest]) -> [C | tokenize_pattern(Rest)];
tokenize_pattern([C | Rest])      -> [C | tokenize_pattern(Rest)].


%% The tokenization of a class is a bit complex and deserves some
%% explanations.
%% The beginning of the class has 6 forms that we distinguish:
%%
%% 1. "[!]..." : a negated class where the first character is ']';
%%    unless it is escaped, this is the only place that ']' can appear;
%% 2. "[^]..." : this is the same thing as form 1;
%% 3. "[!..." : a negated class, i.e. a character cannot be any listed here;
%% 4. "[^..." : the same thing as form 3;
%% 5. "[]..." : a class containing ']' without escaping it;
%% 6. "[..." : a class, i.e. a matching character must be one listed here.
%%
%% Once we know what kind of class (regular or negated) we are dealing with,
%% and whether we should include ']' in the class's set, we invoke the
%% token_class2/3 function.
tokenize_class([$[, $!, $] | Rest]) -> tokenize_class2(Rest, "]", nclass);
tokenize_class([$[, $^, $] | Rest]) -> tokenize_class2(Rest, "]", nclass);
tokenize_class([$[, $! | Rest])     -> tokenize_class2(Rest, "",  nclass);
tokenize_class([$[, $^ | Rest])     -> tokenize_class2(Rest, "",  nclass);
tokenize_class([$[, $] | Rest])     -> tokenize_class2(Rest, "]", class);
tokenize_class([$[ | Rest])         -> tokenize_class2(Rest, "",  class).

%% A dash ('-') signifies a range in a class; if we want to include a
%% literal dash, we must either escape it or place it as the last
%% character of the range.
%%
%% In Git's wildmatch() implementation, a class *cannot* match a slash,
%% i.e., wildmatch("[/]", "/") returns false.  When we have a negative
%% match, we always add the '/' character to the class to make sure it
%% will not be matched.  When we are building the class's set, we skip
%% over slashes.
%% BUG: The pattern [.-^] will incorrectly accept the slash.
tokenize_class2([$-, $] | Rest], Acc, ClassType) ->
    {{ClassType, lists:reverse([$-] ++ include_slash(ClassType) ++ Acc)}, Rest};
tokenize_class2([$] | Rest], Acc, ClassType) ->
    {{ClassType, lists:reverse(include_slash(ClassType) ++ Acc)}, Rest};
tokenize_class2([$/ | Rest], Acc, ClassType) ->
    tokenize_class2(Rest, Acc, ClassType);
tokenize_class2([$\\, C | Rest], Acc, ClassType) ->
    C2 = case requires_escape(C) of
             true -> [C, $\\];
             false -> [C]
         end,
    tokenize_class2(Rest, C2 ++ Acc, ClassType);
tokenize_class2([C | Rest], Acc, ClassType) ->
    tokenize_class2(Rest, [C | Acc], ClassType);
tokenize_class2([], _, _) -> throw(unfinished_class).

include_slash(class) -> [];
include_slash(nclass) -> [$/].

requires_escape($\\) -> true;
requires_escape($]) -> true;
requires_escape($-) -> true;
requires_escape(_) -> false.
