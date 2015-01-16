%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: path_util.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri Dec 26 22:10:56 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Mongoose URI helper.
%----------------------------------------------------------------------------%

:- module mercury_mongoose.path_util.

:- interface.

:- import_module string.

%----------------------------------------------------------------------------%

:- type encoded_uri.

:- type decoded_uri == string.

:- pred decode_uri(encoded_uri, decoded_uri).
:- mode decode_uri(in, out) is det.

:- type encoded_uri_path.

:- type decoded_uri_path == string.

:- pred decode_uri_path(encoded_uri_path, decoded_uri_path).
:- mode decode_uri_path(in, out) is det.

:- pred match_prefix(decoded_uri_path::in, string::in, int::out) is semidet.

:- pred match_prefix_and_suffix(decoded_uri_path, string, string).
:- mode match_prefix_and_suffix(in, in, out) is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module std_util.

%----------------------------------------------------------------------------%

:- type encoded_uri == string.

:- type encoded_uri_path == string.

%----------------------------------------------------------------------------%
%
% Decode and encode functions for URIs
% TODO: Optimise for C to avoid excessive memory allocations

decode_uri_path(EncodedPath, DecodedPath) :-
    % XXX: This is based on the implementation detail that mg_url_decode also
    % can decode stand-alone pathes.
    decode_uri(EncodedPath, DecodedPath).

decode_uri(_, _) :-
    sorry($file, $pred, $grade ++ " is not supported").

:- pragma foreign_proc("C", decode_uri(EncodedUri::in, DecodedUri::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    int n = (int) strlen(EncodedUri);
    MR_allocate_aligned_string_msg(DecodedUri, n, MR_ALLOC_ID);
    DecodedUri[n] = '\\0';
    mg_url_decode(EncodedUri, n, DecodedUri, n + 1, 0);
").

%----------------------------------------------------------------------------%

match_prefix(_, _, _) :-
    sorry($file, $pred, $grade ++ " is not supported").

    % XXX: This should be in mongoose.h
:- pragma foreign_decl("C",
"
    int mg_match_prefix(
        const char *pattern,
        int pattern_len,
        const char *str);
").

:- pragma foreign_proc("C",
    match_prefix(DecodedUri::in, PrefixPattern::in, MatchPos::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    int n = (int) strlen(PrefixPattern);
    MatchPos = mg_match_prefix(PrefixPattern, n, DecodedUri);
    SUCCESS_INDICATOR = MatchPos > 0;
").

match_prefix_and_suffix(DecodedUri, PrefixPattern, Suffix) :-
    match_prefix(DecodedUri, PrefixPattern, MatchPos),
    DecodedUriLength = length(DecodedUri),
    Suffix = right(DecodedUri, DecodedUriLength - MatchPos).

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.path_util.
%----------------------------------------------------------------------------%
